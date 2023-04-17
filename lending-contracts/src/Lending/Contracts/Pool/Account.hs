{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
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

module Lending.Contracts.Pool.Account
  ( MatcherContext (..)
  , StateChange (..)
  , RequestResult (..)
  , processAccount
  , getPoolChange
  )
where

import Control.Category qualified as Category
import Data.Monoid (Endo (Endo))
import Data.Monoid qualified as Monoid
import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Value qualified as V
import Plutarch.Api.V2
  ( AmountGuarantees (Positive)
  , KeyGuarantees (Sorted)
  , PAddress
  , POutputDatum (PNoOutputDatum)
  , PTxOut (PTxOut)
  , PValue
  )
import Plutarch.Extra.AssetClass (PAssetClass)
import Plutarch.Extra.Bool (passert)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.List (pfromList)
import Plutarch.Extra.Map (pfoldMapWithKey, ptryLookup)
import Plutarch.Extra.Maybe (pdnothing, pisDJust, pmaybeData)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.ScriptContext (ptryFromInlineDatum)
import Plutarch.Extra.Traversable (pfoldMap)
import Plutarch.Extra.Value (passetClassDataValue)
import Plutarch.List (ptryUncons)
import Plutarch.Monadic qualified as P
import Plutarch.Num (pnegate)

import Lending.Contracts.Account.Types
  ( PAccountDatum (PAccountDatum)
  , PClearRequest (PClearBorrowing, PClearSupplying)
  , PRequest (PBorrowRequest, PRepayRequest, PSupplyRequest, PWithdrawRequest)
  )
import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Common
  ( expectInlineDatum
  , getAssetClassData
  , getCollateralAssets
  , passetClassValuePositive
  , pmin
  , ptryFromData
  )
import Lending.Contracts.Exchange (PActual, PPrice, PReceipt, convertFrom, convertTo)
import Lending.Contracts.LoanToValue.OnChain (checkMaxLoanToValue)
import Lending.Contracts.Manager.Types (PRiskParameters)
import Lending.Contracts.Pool.Types (PAssetInformation)

data MatcherContext (s :: S) = MatcherContext
  { mcPriceMap :: Term s (PMap 'Sorted PAsset PPrice)
  , mcAssetInfoMap :: Term s (PMap 'Sorted PAsset PAssetInformation)
  , mcRiskParameters :: Term s (PMap 'Sorted PAsset PRiskParameters)
  , mcMinAdaUtxo :: Term s PInteger
  , mcAccountAuthToken :: Term s PAssetClass
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType MatcherContext where
  type DPTStrat _ = PlutusTypeScott

data MatcherState (s :: S) = MatcherState
  { msRemainingOutputs :: Term s (PBuiltinList PTxOut)
  , msPoolStateChange :: Term s StateChange
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType MatcherState where
  type DPTStrat _ = PlutusTypeScott

data StateChange (s :: S) = StateChange
  { scSupply :: Term s (PMap 'Sorted PAsset PReceipt)
  , scBorrow :: Term s (PMap 'Sorted PAsset PReceipt)
  , scPoolReceivable :: Term s (PValue 'Sorted 'Positive)
  , scPoolPayable :: Term s (PValue 'Sorted 'Positive)
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType StateChange where
  type DPTStrat _ = PlutusTypeScott

instance Semigroup (Term s StateChange) where
  x <> y = P.do
    StateChange xSupply xBorrow xReceivable xPayable <- pmatch x
    StateChange ySupply yBorrow yReceivable yPayable <- pmatch y
    pcon $
      StateChange
        (AssocMap.punionWith # plam (+) # xSupply # ySupply)
        (AssocMap.punionWith # plam (+) # xBorrow # yBorrow)
        (xReceivable <> yReceivable)
        (xPayable <> yPayable)

instance Monoid (Term s StateChange) where
  mempty = pcon defaultStateChange

data RequestResult (s :: S) = RequestResult
  { rrOutputs :: Term s (PBuiltinList PTxOut)
  , rrAccountStateChange :: Term s StateChange
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType RequestResult where
  type DPTStrat _ = PlutusTypeScott

instance Semigroup (Term s RequestResult) where
  x <> y = P.do
    RequestResult xOutputs xChange <- pmatch x
    RequestResult yOutputs yChange <- pmatch y
    pcon $
      RequestResult
        (pconcat # xOutputs # yOutputs)
        (xChange <> yChange)

instance Monoid (Term s RequestResult) where
  mempty =
    pcon
      RequestResult
        { rrOutputs = pnil
        , rrAccountStateChange = mempty
        }

defaultStateChange :: StateChange s
defaultStateChange =
  StateChange
    { scSupply = AssocMap.pempty
    , scBorrow = AssocMap.pempty
    , scPoolReceivable = mempty
    , scPoolPayable = mempty
    }

getPoolChange :: Term s (MatcherContext :--> PBuiltinList PTxOut :--> PBuiltinList PTxOut :--> StateChange)
getPoolChange = phoistAcyclic $ plam $ \matcherContext accounts outputs -> P.do
  MatcherState {msPoolStateChange} <-
    pmatch $ pfoldl # (matchAccount # matcherContext) # pcon (MatcherState outputs mempty) # accounts
  msPoolStateChange

-- Note: Don't need to check redeemer of spending Account UTxO because only redeemer Apply can pass the validation.
--    For each input Account UTxO, produce 1 output Account UTxO (1)
--    General changes of Account Datum after spending along with Pool with redeemer Update:
--     (2a) Request Fields (adNormalRequests, adCollateralUpdate, adClearRequests) are set to empty.
--     (2b) If input Request Fields are not empty, adSupplies, adBorrowings change.
--          Else adSupplies, adBorrowings must not change.
--    For all Account redeemers except Apply, you can only spend 1 Account UTxO.
--    Consider the available scenarios when spending Pool with redeemer Update:
--      + Consume Account with redeemer Liquidate:
--          Change adSupplies and adBorrowings, other fields (including Request Fields) must not change.
--          => Conflict with (2b)
--      + Consume Account UTxO with redeemer Close:
--          Burn 1 Account Auth Token -> No output Account UTxO
--          => Conflict with (1)
--      + Consume Account UTxO with redeemer Update:
--          UnchangableAccountFields (all fields except Request Fields) must not change
--          => Conflict with (2b)

matchAccount :: Term s (MatcherContext :--> MatcherState :--> PTxOut :--> MatcherState)
matchAccount = phoistAcyclic $ plam $ \matcherContext matcherState account -> P.do
  MatcherState {msRemainingOutputs, msPoolStateChange} <- pmatch matcherState
  RequestResult {rrOutputs, rrAccountStateChange} <- pmatch (processAccount # matcherContext # account)
  remainingOutputs <- plet $ pfoldl # expectOutput # msRemainingOutputs # rrOutputs
  pcon $
    MatcherState
      { msRemainingOutputs = remainingOutputs
      , msPoolStateChange = msPoolStateChange <> rrAccountStateChange
      }

-- Validate each account and return new pool data after each time
processAccount :: Term s (MatcherContext :--> PTxOut :--> RequestResult)
processAccount = phoistAcyclic $ plam $ \matcherContext account' -> P.do
  account <- pletFields @["address", "datum"] account'
  MatcherContext {mcPriceMap, mcAssetInfoMap, mcRiskParameters, mcAccountAuthToken} <- pmatch matcherContext
  ptrace "Check @Cannot extract account datum@"
  datum <- pletAll (ptryFromData @PAccountDatum (pto (ptryFromInlineDatum # account.datum)))

  afterNormalRequests <- plet $ pfoldMap # (getRequestResultFromRequest # matcherContext) # datum.adNormalRequests

  RequestResult {rrAccountStateChange = stateChangeAfterNormalRequests} <- pmatch afterNormalRequests
  StateChange {scSupply = stateSupply, scBorrow = stateBorrow} <- pmatch stateChangeAfterNormalRequests
  supplyAfterNormalRequests <- plet $ AssocMap.punionWith # plam (+) # datum.adSupplies # stateSupply
  borrowAfterNormalRequests <- plet $ AssocMap.punionWith # plam (+) # datum.adBorrowings # stateBorrow

  -- Check supply and borrow
  ptrace "Check @Supply value or Borrow value is not positive@"

  _ <-
    plet $
      pif
        (checkNonNegative # supplyAfterNormalRequests #&& checkNonNegative # borrowAfterNormalRequests)
        (pconstant ())
        (ptraceError "Supply value or Borrow value is not positive")

  afterClearRequests <-
    plet $
      pfoldMapWithKey
        # (updateMapAssetClearRequest # supplyAfterNormalRequests # borrowAfterNormalRequests # matcherContext)
        # datum.adClearRequests

  -- Combine apply request and pay all request -- ##### 1M for <>
  allRequestResults <- plet $ afterNormalRequests <> afterClearRequests
  RequestResult {rrOutputs, rrAccountStateChange} <- pmatch allRequestResults
  StateChange {scSupply, scBorrow, scPoolPayable} <- pmatch rrAccountStateChange
  newAccountSupply <- plet $ AssocMap.punionWith # plam (+) # datum.adSupplies # scSupply
  newAccountBorrow <- plet $ AssocMap.punionWith # plam (+) # datum.adBorrowings # scBorrow
  -- ##### 1M for <>
  newCollateral <- plet $ pmaybeData # datum.adCollateralAssets # plam pfromData # datum.adCollateralUpdate
  isUpdatingCollateral <- plet $ pisDJust # datum.adCollateralUpdate
  shouldSkipLtvCheck <- plet $ pnot # isUpdatingCollateral #&& scPoolPayable #== mempty
  ptrace "Check @New account has invalid loan-to-value@"
  _ <-
    plet $
      shouldSkipLtvCheck
        #|| checkMaxLoanToValue
          # mcRiskParameters
          # mcPriceMap
          # mcAssetInfoMap
          # (getCollateralAssets # newAccountSupply # newCollateral)
          # newAccountBorrow
        #|| ptraceError ("Invalid loan to value for new datum: " <> pshow account.datum)
  extraLovelace <- plet $ datum.adExtraLovelace
  accountBaseValue <- plet $ toAda # extraLovelace <> passetClassValuePositive # mcAccountAuthToken # 1
  newDatum <-
    plet $
      mkRecordConstr PAccountDatum $
        #adSupplies .= pdata newAccountSupply
          .& #adBorrowings .= pdata newAccountBorrow
          .& #adCollateralAssets .= pdata newCollateral
          .& #adUserNft .= datum.adUserNft
          .& #adNormalRequests .= pdata pnil
          .& #adCollateralUpdate .= pdata pdnothing
          .& #adProtocolIncentive .= pdata pdnothing
          .& #adClearRequests .= pdata AssocMap.pempty
          .& #adExtraLovelace .= datum.adExtraLovelace
  accountTxOut <- plet $ mkTxOut # account.address # accountBaseValue # (expectInlineDatum # newDatum)
  protocolIncentive <-
    plet $
      pmaybeData # mempty # getAllProtocolIncentives # datum.adProtocolIncentive

  pcon (RequestResult (pcons # accountTxOut # rrOutputs) (rrAccountStateChange <> protocolIncentive))

expectOutput :: Term s (PBuiltinList PTxOut :--> PTxOut :--> PBuiltinList PTxOut)
expectOutput = phoistAcyclic $ plam $ \outputs expectedOutput -> P.do
  PPair nextOutput remaining <- pmatch (ptryUncons # outputs)
  passert ("Next user output doesn't match expected " <> pshow expectedOutput) (nextOutput #== expectedOutput) remaining

getAllProtocolIncentives :: Term s (PAsData (PMap 'Sorted PAsset PReceipt) :--> StateChange)
getAllProtocolIncentives = phoistAcyclic $ plam \incentiveMap ->
  pcon
    defaultStateChange
      { scSupply = AssocMap.pmap # pnegate # pfromData incentiveMap
      }

getSupplyReceipt :: Term s (PMap 'Sorted PAsset PAssetInformation :--> PAsset :--> PActual :--> PReceipt)
getSupplyReceipt = phoistAcyclic $ plam \pool asset actual -> P.do
  ptrace "Check @Cannot find asset in pool's assets list@"
  oldAssetInfo <- plet $ ptryLookup # asset # pool
  rate <- plet $ pfield @"aiCumulatedInterestRateSupplying" # oldAssetInfo
  convertTo # rate # actual

getBorrowReceipt :: Term s (PMap 'Sorted PAsset PAssetInformation :--> PAsset :--> PActual :--> PReceipt)
getBorrowReceipt = phoistAcyclic $ plam \pool asset actual -> P.do
  ptrace "Check @Cannot find asset in pool's assets list@"
  oldAssetInfo <- plet $ ptryLookup # asset # pool
  rate <- plet $ pfield @"aiCumulatedInterestRateBorrowing" # oldAssetInfo
  convertTo # rate # actual

addSupply :: Term s PAsset -> Term s PReceipt -> StateChange s -> StateChange s
addSupply asset change old = old {scSupply = AssocMap.psingleton # asset # change}

addBorrow :: Term s PAsset -> Term s PReceipt -> StateChange s -> StateChange s
addBorrow asset change old = old {scBorrow = AssocMap.psingleton # asset # change}

payToPool :: Term s (PValue 'Sorted 'Positive) -> StateChange s -> StateChange s
payToPool value old = old {scPoolReceivable = value}

payFromPool :: Term s (PValue 'Sorted 'Positive) -> StateChange s -> StateChange s
payFromPool value old = old {scPoolPayable = value}

mkTxOut :: Term s (PAddress :--> PValue 'Sorted 'Positive :--> POutputDatum :--> PTxOut)
mkTxOut = phoistAcyclic $ plam \address value datum ->
  mkRecordConstr PTxOut $
    #address .= pdata address
      .& #value .= pdata value
      .& #datum .= pdata datum
      .& #referenceScript .= pdata pdnothing

mkUserTxOut :: Term s (PInteger :--> PAddress :--> PValue 'Sorted 'Positive :--> PTxOut)
mkUserTxOut = phoistAcyclic $ plam \minAdaUtxo address value -> P.do
  mkTxOut # address # (toAda # minAdaUtxo <> value) # mkRecordConstr PNoOutputDatum Category.id

toValue :: Term s (PMap 'Sorted PAsset PRiskParameters :--> PAsset :--> PActual :--> PValue 'Sorted 'Positive)
toValue = phoistAcyclic $ plam \riskParams asset amount ->
  pif
    (amount #== 0)
    mempty
    (V.passertPositive #$ passetClassDataValue # (getAssetClassData # asset # riskParams) # pto amount)

toAda :: Term s (PInteger :--> PValue 'Sorted 'Positive)
toAda = phoistAcyclic $ plam \amount -> V.passertPositive #$ V.psingleton # V.padaSymbol # V.padaToken # amount

mkRequestResult :: [StateChange s -> StateChange s] -> [Term s PTxOut] -> Term s RequestResult
mkRequestResult changes outputs =
  pcon $
    RequestResult
      { rrAccountStateChange = pcon (Monoid.appEndo (foldMap Endo changes) defaultStateChange)
      , rrOutputs = pfromList outputs
      }

getRequestResultFromRequest :: Term s (MatcherContext :--> PRequest :--> RequestResult)
getRequestResultFromRequest = phoistAcyclic $ plam $ \ctx request' -> P.do
  MatcherContext {mcAssetInfoMap, mcRiskParameters, mcMinAdaUtxo} <- pmatch ctx
  pmatch request' $ \case
    PSupplyRequest req -> P.do
      requestSupply <- pletFields @["srAsset", "srAmount"] req
      asset <- plet $ requestSupply.srAsset
      amount <- plet $ requestSupply.srAmount
      receiptAmount <- plet $ getSupplyReceipt # mcAssetInfoMap # asset # amount
      value <- plet $ toValue # mcRiskParameters # asset # amount
      mkRequestResult
        [addSupply asset receiptAmount, payToPool value]
        []
    PWithdrawRequest req -> P.do
      requestWithdraw <- pletFields @["wrAsset", "wrAmount", "wrReceiver"] req
      asset <- plet $ requestWithdraw.wrAsset
      amount <- plet $ requestWithdraw.wrAmount
      receiptAmount <- plet $ getSupplyReceipt # mcAssetInfoMap # asset # amount
      value <- plet $ toValue # mcRiskParameters # asset # amount
      mkRequestResult
        [addSupply asset (-receiptAmount), payFromPool value]
        [mkUserTxOut # mcMinAdaUtxo # requestWithdraw.wrReceiver # value]
    PBorrowRequest req -> P.do
      requestBorrow <- pletFields @["brAsset", "brAmount", "brReceiver"] req
      asset <- plet $ requestBorrow.brAsset
      amount <- plet $ requestBorrow.brAmount
      receiptAmount <- plet $ getBorrowReceipt # mcAssetInfoMap # asset # amount
      value <- plet $ toValue # mcRiskParameters # asset # amount
      mkRequestResult
        [addBorrow asset receiptAmount, payFromPool value]
        [mkUserTxOut # mcMinAdaUtxo # requestBorrow.brReceiver # value]
    PRepayRequest req -> P.do
      requestRepay <- pletFields @["rrAsset", "rrAmount"] req
      asset <- plet $ requestRepay.rrAsset
      amount <- plet $ requestRepay.rrAmount
      receiptAmount <- plet $ getBorrowReceipt # mcAssetInfoMap # asset # amount
      value <- plet $ toValue # mcRiskParameters # asset # amount
      mkRequestResult
        [addBorrow asset (-receiptAmount), payToPool value]
        []

checkNonNegative :: Term s (PMap 'Sorted PAsset PReceipt :--> PBool)
checkNonNegative = phoistAcyclic $ AssocMap.pall # plam (0 #<=)

updateMapAssetClearRequest
  :: Term
      s
      ( PMap 'Sorted PAsset PReceipt
          :--> PMap 'Sorted PAsset PReceipt
          :--> MatcherContext
          :--> PAsset
          :--> PClearRequest
          :--> RequestResult
      )
updateMapAssetClearRequest = phoistAcyclic $ plam $ \curSupplyMap curBorrowMap ctx asset clearRequest -> P.do
  MatcherContext {mcAssetInfoMap, mcRiskParameters, mcMinAdaUtxo} <- pmatch ctx
  pmatch clearRequest $ \case
    PClearSupplying clearSupplying' -> P.do
      clearSupplying <- pletFields @'["cdReceiver"] clearSupplying'
      ptrace "Check @Cannot find asset in pool's assets list@"
      rate <- plet $ pfield @"aiCumulatedInterestRateSupplying" #$ ptryLookup # asset # mcAssetInfoMap
      ptrace "Check @Cannot find asset in user's supply assets list@"
      receiptAmount <- plet $ ptryLookup # asset # curSupplyMap
      amount <- plet $ convertFrom # rate # receiptAmount
      value <- plet $ toValue # mcRiskParameters # asset # amount
      mkRequestResult
        [addSupply asset (-receiptAmount), payFromPool value]
        [mkUserTxOut # mcMinAdaUtxo # clearSupplying.cdReceiver # value]
    PClearBorrowing clearBorrowing' -> P.do
      clearBorrowing <- pletFields @["cbLimited", "cbReceiver"] clearBorrowing'
      ptrace "Check @Cannot find asset in pool's assets list@"
      rate <- plet $ pfield @"aiCumulatedInterestRateBorrowing" #$ ptryLookup # asset # mcAssetInfoMap
      maxAmount <- plet $ clearBorrowing.cbLimited
      ptrace "Check @Cannot find asset in user's borrow assets list@"
      accountReceipt <- plet $ ptryLookup # asset # curBorrowMap
      accountActual <- plet $ convertFrom # rate # accountReceipt
      repaidActual <- plet $ pmin # accountActual # maxAmount
      repaidValue <- plet $ toValue # mcRiskParameters # asset # repaidActual
      repaidReceipt <- plet $ pif (accountActual #<= maxAmount) accountReceipt (convertTo # rate # maxAmount)
      remainingAmount <- plet $ maxAmount - repaidActual
      remainingValue <- plet $ toValue # mcRiskParameters # asset # remainingAmount
      mkRequestResult
        [addBorrow asset (-repaidReceipt), payToPool repaidValue]
        [mkUserTxOut # mcMinAdaUtxo # clearBorrowing.cbReceiver # remainingValue]
