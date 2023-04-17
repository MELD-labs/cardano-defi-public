{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lending.Contracts.Account.OnChain.Liquidate
  ( LiquidationResult (..)
  , validateLiquidate
  , calculateLiquidationResult
  )
where

import Plutarch.Api.V1.AssocMap (KeyGuarantees (Sorted), PMap)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Value
  ( AmountGuarantees (Positive)
  , PValue
  )
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Extra.Applicative (PApplicative (ppure))
import Plutarch.Extra.AssetClass (PAssetClassData)
import Plutarch.Extra.Comonad (pextract)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.FixedDecimal (pediv, pemul, pfromFixedDecimal, ptoFixedDecimal)
import Plutarch.Extra.Function (pidentity)
import Plutarch.Extra.List (ptryFromSingleton)
import Plutarch.Extra.Map (pfoldMapWithKey, pfoldlWithKey, ptryLookup)
import Plutarch.Extra.Maybe (pdjust, pdnothing, pisDJust)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.Sum (PSum (PSum))
import Plutarch.Extra.These (pdthese)
import Plutarch.Monadic qualified as P

import Lending.Contracts.Account.Types
  ( PAccountDatum (PAccountDatum)
  , PAccountLiquidateRedeemerData
  , PClearRequest (PClearBorrowing, PClearSupplying)
  )
import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Common
  ( expectInlineDatum
  , expectNextOutput
  , getCollateralAssets
  , getInlineDatum
  , getOracleAssetPrices
  , getState
  , hasUniqueAsset
  , percentOf
  , pmapWithKey
  , pmin
  , uniqueAssetFromTxInInfo
  )
import Lending.Contracts.Exchange (PActual, PDecimal, PFiat, PPrice, PReceipt, convertFrom, convertTo)
import Lending.Contracts.LoanToValue.OnChain
  ( InterestData
      ( InterestData
      , idSafeHealthFactorThreshold
      , idUnderLiquidationThreshold
      )
  , getInterestData
  )
import Lending.Contracts.Manager.Types (PManagerDatum)
import Lending.Contracts.Map (punionWithThese)
import Lending.Contracts.Pool.Types (PAssetInformation (PAssetInformation), PPoolDatum)
import Lending.Contracts.Request (calculateAccountValue)

data LiquidationResult (s :: S) = LiquidationResult
  { lrContinuingAccountDatum :: Term s PAccountDatum
  , lrNewAccountDatum :: Term s PAccountDatum
  , lrNewAccountValue :: Term s (PValue 'Sorted 'Positive)
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType LiquidationResult where
  type DPTStrat _ = PlutusTypeScott

calculateLiquidationResult
  :: Term
      s
      ( PManagerDatum
          :--> PMap 'Sorted PAsset PPrice
          :--> PMap 'Sorted PAsset PAssetInformation
          :--> PAccountDatum
          :--> PAccountLiquidateRedeemerData
          :--> LiquidationResult
      )
calculateLiquidationResult =
  phoistAcyclic $
    plam $
      \managerDatum'
       assetPrices
       poolDatum
       accountDatum'
       liquidateRedeemerData' -> P.do
          accountDatum <- pletAll accountDatum'
          supplyAssets <- plet accountDatum.adSupplies
          collateralList <- plet accountDatum.adCollateralAssets
          borrowTokens <- plet accountDatum.adBorrowings
          -- Conversion
          accountActualBorrowToken <- plet $ pmapWithKey # (actualBorrow # poolDatum) # accountDatum.adBorrowings
          -- Extract collateral assets
          collateralAssets <- plet $ getCollateralAssets # supplyAssets # collateralList

          managerDatum <- pletFields @["mdAccountAuthToken", "mdRiskParameters", "mdGlobalRiskParameters"] managerDatum'
          globalRiskParameters' <- plet managerDatum.mdGlobalRiskParameters
          globalRiskParameters <- pletAll globalRiskParameters'
          riskParameters <- plet managerDatum.mdRiskParameters
          closeFactorHealthFactorThreshold <- plet globalRiskParameters.grpCloseFactorHealthFactorThreshold
          maxLiquidationCloseFactor <- plet globalRiskParameters.grpMaxLiquidationCloseFactor
          closeFactorMinCollateralThreshold <- plet globalRiskParameters.grpCloseFactorMinCollateralThreshold
          closeFactor <- plet globalRiskParameters.grpCloseFactor
          liquidatorIncentive <- plet globalRiskParameters.grpLiquidatorIncentive
          protocolIncentive <- plet globalRiskParameters.grpProtocolIncentive

          liquidateRedeemerData <- pletAll liquidateRedeemerData'
          liquidatingDebt <- plet liquidateRedeemerData.alrBorrowings
          liquidatingCollateral <- plet liquidateRedeemerData.alrCollaterals
          newAccountExtraLovelace <- plet liquidateRedeemerData.alrExtraLovelace
          newAccountUserNft <- plet liquidateRedeemerData.alrUserNft
          newAccountClearRequests <- plet liquidateRedeemerData.alrClearRequests
          ptrace "Check @Negative value in LiquidateRedeemerData@"
          pif
            ( AssocMap.pany # plam (#< 0) # liquidatingDebt
                #|| AssocMap.pany # plam (#< 0) # liquidatingCollateral
            )
            (ptraceError ("Negative value in LiquidateRedeemerData: " <> pshow liquidateRedeemerData'))
          -- Conversions
          receiptLiquidatingCollateral <- plet $ pmapWithKey # (receiptSupply # poolDatum) # liquidatingCollateral
          receiptLiquidatingDebt <- plet $ pmapWithKey # (receiptBorrow # poolDatum) # liquidatingDebt

          -- Calculate liquidate percent
          -- liquidate percent = liquidating debt / total debt
          liquidatePercent <- plet $ calculateValueRate # assetPrices # liquidatingDebt # accountActualBorrowToken
          -- Calculate liquidator receive percent.
          -- liquidator receive percent = receiving collateral value / repaying debt value
          liquidatorReceivePercent <- plet $ calculateValueRate # assetPrices # liquidatingCollateral # liquidatingDebt
          -- Check liquidator incentive

          ptrace "Check @Exceed the maximum amount of collateral that can be taken@"

          pif
            (1 + liquidatorIncentive #< liquidatorReceivePercent)
            ( ptraceError
                ("Exceed the maximum amount of collateral that can be taken: " <> pshow liquidatorReceivePercent)
            )

          InterestData {idUnderLiquidationThreshold, idSafeHealthFactorThreshold} <-
            pmatch $
              getInterestData
                # riskParameters
                # assetPrices
                # poolDatum
                # collateralAssets
                # borrowTokens
                # closeFactorHealthFactorThreshold
                # closeFactorMinCollateralThreshold
                # (closeFactor #< liquidatePercent)

          liquidateLimitation <-
            plet $
              pif idSafeHealthFactorThreshold closeFactor maxLiquidationCloseFactor

          ptrace "Check @Loan-to-value must reach liquidation threshold@"

          -- Check LTV
          pif
            idUnderLiquidationThreshold
            (ptraceError "Loan-to-value must reach liquidation threshold")

          ptrace
            "Check @Exceed liquidating limitation percent@"
          -- Check liquidating limitation
          pif
            (liquidateLimitation #< liquidatePercent)
            ( ptraceError $
                "Exceed liquidating limitation percent: Liquidate percent: "
                  <> pshow liquidatePercent
                  <> ", Liquidate limitation percent: "
                  <> pshow liquidateLimitation
            )

          -- Collaterals which protocol will take (Receipt)
          protocolIncentiveCollaterals <-
            plet $
              pmapWithKey
                # ( calculateProtocolIncentive
                      # supplyAssets
                      # liquidatorReceivePercent
                      # protocolIncentive
                  )
                # receiptLiquidatingCollateral
          -- Total collaterals taken from the current account
          losingCollaterals <-
            plet $ AssocMap.punionWith # plam (+) # receiptLiquidatingCollateral # protocolIncentiveCollaterals
          -- NOTE: Assume the second parameter always subset of the first parameter

          ptrace "Check @The second parameter must be a subset of the first parameter@"

          combine <-
            plet $
              pdthese
                # pidentity
                # plam
                  ( const $
                      ptraceError "combine (pdthese): The second parameter must be a subset of the first parameter"
                  )
                # plam (-)
          continuingSupplyAssets <- plet $ punionWithThese # combine # supplyAssets # losingCollaterals
          continuingBorrowAssets <- plet $ punionWithThese # combine # borrowTokens # receiptLiquidatingDebt

          continuingAccountDatum <-
            plet $
              mkRecordConstr PAccountDatum $
                #adSupplies .= pdata continuingSupplyAssets
                  .& #adBorrowings .= pdata continuingBorrowAssets
                  .& #adCollateralAssets .= pdata collateralList
                  .& #adUserNft .= accountDatum.adUserNft
                  .& #adNormalRequests .= accountDatum.adNormalRequests
                  .& #adCollateralUpdate .= accountDatum.adCollateralUpdate
                  .& #adProtocolIncentive .= pdata pdnothing
                  .& #adClearRequests .= accountDatum.adClearRequests
                  .& #adExtraLovelace .= accountDatum.adExtraLovelace

          newAccountDatum <-
            plet $
              mkRecordConstr PAccountDatum $
                #adSupplies .= pdata receiptLiquidatingCollateral
                  .& #adBorrowings .= pdata receiptLiquidatingDebt
                  .& #adCollateralAssets .= pdata collateralList
                  .& #adUserNft .= newAccountUserNft
                  .& #adNormalRequests .= pdata pnil
                  .& #adCollateralUpdate .= pdata pdnothing
                  .& #adProtocolIncentive .= pdata (pdjust # pdata protocolIncentiveCollaterals)
                  .& #adClearRequests .= newAccountClearRequests
                  .& #adExtraLovelace .= newAccountExtraLovelace

          newAccountValue <- plet $ calculateAccountValue # managerDatum' # newAccountDatum

          pcon $
            LiquidationResult
              { lrContinuingAccountDatum = continuingAccountDatum
              , lrNewAccountDatum = newAccountDatum
              , lrNewAccountValue = newAccountValue
              }

validateLiquidate
  :: Term s (PAssetClassData :--> PAccountDatum :--> PAccountLiquidateRedeemerData :--> PScriptContext :--> PBool)
validateLiquidate = phoistAcyclic $ plam $ \managerAuthToken datum' liquidateRedeemerData ctx' -> P.do
  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  txinfo <- pletFields @["inputs", "outputs", "referenceInputs", "mint", "redeemers"] ctx.txInfo
  datum <- pletAll datum'

  checkBeingLiquidatedAccount <- plet $ pnot #$ pisDJust # datum.adProtocolIncentive

  -- Extract reference inputs
  refInputs <- plet txinfo.referenceInputs
  manager' <- plet $ getState @PManagerDatum # managerAuthToken # refInputs
  manager <-
    pletFields
      @'[ "mdAccountAuthToken"
        , "mdPoolNft"
        , "mdBatcherFee"
        , "mdMinAdaUtxo"
        , "mdOracleCheckerToken"
        , "mdGlobalRiskParameters"
        ]
      manager'
  poolDatum' <- plet $ getState @PPoolDatum # manager.mdPoolNft # refInputs
  poolDatum <- plet $ pfield @"pdAssets" # poolDatum'

  assetPrices <- plet $ getOracleAssetPrices # txinfo.redeemers # manager.mdOracleCheckerToken

  LiquidationResult
    { lrNewAccountDatum
    , lrContinuingAccountDatum
    , lrNewAccountValue
    } <-
    pmatch $
      calculateLiquidationResult
        # manager'
        # assetPrices
        # poolDatum
        # datum'
        # liquidateRedeemerData

  accountAuthAsset <- plet $ manager.mdAccountAuthToken
  inputs <- plet txinfo.inputs
  outputs <- plet txinfo.outputs

  -- Extract account output and address
  inputsHavingAuthToken <- plet $ pfilter # (uniqueAssetFromTxInInfo # accountAuthAsset) # inputs
  inputTxInInfo <- plet $ ptryFromSingleton # inputsHavingAuthToken

  accInput <- pletFields @["address", "value"] $ pfield @"resolved" # inputTxInInfo

  accOutputs <- plet $ pfilter # (hasUniqueAsset # accountAuthAsset) # outputs

  newAccountTxout' <-
    plet $
      ptryFromSingleton #$
        expectNextOutput # accOutputs # accInput.address # accInput.value #$
          expectInlineDatum # lrContinuingAccountDatum

  newAccountTxout <- pletFields @["address", "value", "datum"] newAccountTxout'

  checkNewAddress <- plet $ accInput.address #== newAccountTxout.address

  newAccDatum' <- plet $ getInlineDatum @PAccountDatum # pfromData newAccountTxout.datum
  newAccDatum <- pletAll newAccDatum'
  checkAccDatum <-
    plet $
      newAccDatum' #== lrNewAccountDatum

  clearRequests <- plet $ newAccDatum.adClearRequests

  checkNewAccountValue <- plet $ lrNewAccountValue #== newAccountTxout.value
  liquidatorBorrow <- plet $ pfield @"adBorrowings" # lrNewAccountDatum
  checkCompleteLiquidation <-
    plet $ pfoldlWithKey # (checkRepayAsset # poolDatum # clearRequests) # pconstant True # liquidatorBorrow

  haveOnlyClearBorrowing <- plet $ AssocMap.pall # isClearBorrowing # clearRequests

  ptraceIfFalse ("Validate account being liquidated: " <> pshow datum.adProtocolIncentive) checkBeingLiquidatedAccount
    #&& ptraceIfFalse "Validate repay all debt" checkCompleteLiquidation
    #&& ptraceIfFalse
      ( "Validate liquidator address: \nExpect: "
          <> pshow accInput.address
          <> "\nActual: "
          <> pshow newAccountTxout.address
      )
      checkNewAddress
    #&& ptraceIfFalse
      ( "Validate liquidator datum: \nExpect: "
          <> pshow lrNewAccountDatum
          <> "\nActual: "
          <> pshow newAccDatum'
      )
      checkAccDatum
    #&& ptraceIfFalse
      ( "Validate liquidator value: \nExpect: "
          <> pshow lrNewAccountValue
          <> "\nActual: "
          <> pshow newAccountTxout.value
      )
      checkNewAccountValue
    #&& ptraceIfFalse "Validate have only repay requests" haveOnlyClearBorrowing

checkRepayAsset
  :: Term
      s
      ( PMap 'Sorted PAsset PAssetInformation
          :--> PMap 'Sorted PAsset PClearRequest
          :--> PBool
          :--> PAsset
          :--> PReceipt
          :--> PBool
      )
checkRepayAsset = phoistAcyclic $ plam $ \pool clearRequests checker ac amount ->
  pif
    (amount #== 0)
    checker
    $ P.do
      ptrace ("Check @Asset " <> pshow ac <> " is not included in pool@")
      PAssetInformation assetInfo' <- pmatch (ptryLookup # ac # pool)
      ptrace ("Check @Asset " <> pshow ac <> " is not included in list clear requests@")
      PClearBorrowing request <- pmatch (ptryLookup # ac # clearRequests)
      rate <- plet $ pfield @"aiCumulatedInterestRateBorrowing" # assetInfo'
      limit <- plet $ pfield @"cbLimited" # request
      checker
        #&& (pextract #$ convertFrom # rate # amount) #<= (pextract # limit)

isClearBorrowing :: Term s (PClearRequest :--> PBool)
isClearBorrowing = phoistAcyclic $ plam $ \request -> P.do
  pmatch request $ \case
    PClearBorrowing _ -> pconstant True
    PClearSupplying _ -> pconstant False

-- | Calculate the Fiat value rate
calculateValueRate
  :: Term
      s
      ( PMap 'Sorted PAsset PPrice
          :--> PMap 'Sorted PAsset PActual
          :--> PMap 'Sorted PAsset PActual
          :--> PDecimal
      )
calculateValueRate = phoistAcyclic $ plam $ \priceMap dividend divisor -> P.do
  PSum dividendValue <- pmatch $ pfoldMapWithKey # (assetValue # priceMap) # dividend
  PSum divisorValue <- pmatch $ pfoldMapWithKey # (assetValue # priceMap) # divisor
  percentOf # dividendValue # divisorValue

-- | Calculate the Fiat value of the given asset
assetValue
  :: Term
      s
      ( PMap 'Sorted PAsset PPrice
          :--> PAsset
          :--> PActual
          :--> PSum PFiat
      )
assetValue = phoistAcyclic $ plam $ \priceMap asset actualAmount -> P.do
  ptrace ("Check @Asset " <> pshow asset <> " is not included in price list@")
  price <- plet $ ptryLookup # asset # priceMap
  power <- plet $ convertFrom # price # actualAmount
  pcon (PSum power)

-- | Convert Receipt borrow amount to Actual
actualBorrow
  :: Term
      s
      ( PMap 'Sorted PAsset PAssetInformation
          :--> PAsset
          :--> PReceipt
          :--> PActual
      )
actualBorrow = phoistAcyclic $ plam $ \oldPool asset receiptAmount -> P.do
  ptrace ("Check @Asset " <> pshow asset <> " is not included in pool@")
  assetInfo <- plet $ ptryLookup # asset # oldPool
  borrowingRate <- plet $ pfield @"aiCumulatedInterestRateBorrowing" # assetInfo
  convertFrom # borrowingRate # receiptAmount

-- | Convert Actual borrow amount to Receipt
receiptBorrow
  :: Term
      s
      ( PMap 'Sorted PAsset PAssetInformation
          :--> PAsset
          :--> PActual
          :--> PReceipt
      )
receiptBorrow = phoistAcyclic $ plam $ \oldPool asset actualAmount -> P.do
  ptrace ("Check @Asset " <> pshow asset <> " is not included in pool@")
  assetInfo <- plet $ ptryLookup # asset # oldPool
  borrowingRate <- plet $ pfield @"aiCumulatedInterestRateBorrowing" # assetInfo
  convertTo # borrowingRate # actualAmount

-- | Convert Actual supply amount to Receipt
receiptSupply
  :: Term
      s
      ( PMap 'Sorted PAsset PAssetInformation
          :--> PAsset
          :--> PActual
          :--> PReceipt
      )
receiptSupply = phoistAcyclic $ plam $ \oldPool asset actualAmount -> P.do
  ptrace ("Check @Asset " <> pshow asset <> " is not included in pool@")
  assetInfo <- plet $ ptryLookup # asset # oldPool
  lendingRate <- plet $ pfield @"aiCumulatedInterestRateSupplying" # assetInfo
  convertTo # lendingRate # actualAmount

-- | Calculate Protocol Incentive from liquidator incentive
-- Incentives are distributed proportionally to each asset type.
calculateProtocolIncentive
  :: Term
      s
      ( PMap 'Sorted PAsset PReceipt
          :--> PDecimal
          :--> PDecimal
          :--> PAsset
          :--> PReceipt
          :--> PReceipt
      )
calculateProtocolIncentive =
  phoistAcyclic $
    plam $
      \suppliedAsset liquidatorReceivePercent protocolIncentivePercent asset liquidatingCollateral ->
        P.do
          ptrace ("Check @Asset " <> pshow asset <> " is not included in account list supply@")
          suppliedReceipt <- plet $ ptryLookup # asset # suppliedAsset
          ptrace ("Check @Insufficent collateral amount " <> pshow asset <> "@")
          pif
            (suppliedReceipt #< liquidatingCollateral)
            ( ptraceError
                ( "Insufficent collateral amount: Asset: "
                    <> pshow asset
                    <> ", supplied: "
                    <> pshow suppliedReceipt
                    <> ", liquidating: "
                    <> pshow liquidatingCollateral
                )
            )
          -- The maximum collateral that protocol will take:
          -- liquidator receive amount * protocol incentive percent / liquidator receive percent
          protocolIncentiveCollaterals <-
            plet $
              pfromFixedDecimal #$
                (ptoFixedDecimal @18 # pto liquidatingCollateral)
                  `pemul` protocolIncentivePercent
                  `pediv` liquidatorReceivePercent
          pmin
            # (suppliedReceipt - liquidatingCollateral)
            #$ ppure # protocolIncentiveCollaterals
