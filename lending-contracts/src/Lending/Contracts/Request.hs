{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Contracts.Request
  ( UnchangableAccountFields (..)
  , checkAccountWithRequests
  , valueFromRequestUpdate
  , valueFromClearRequest
  , calculateAccountValue
  )
where

import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Api.V1.AssocMap qualified as Map
import Plutarch.Api.V1.Value (AmountGuarantees (Positive), KeyGuarantees (Sorted), PValue)
import Plutarch.Api.V2 (PAddress, PMaybeData, PTxOut)
import Plutarch.Extra.AssetClass (PAssetClassData, padaClass, ptoScottEncoding)
import Plutarch.Extra.Comonad (pextract)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.List (ptryFromSingleton)
import Plutarch.Extra.Map (pfoldMapWithKey)
import Plutarch.Extra.Maybe (pisDJust)
import Plutarch.Extra.Traversable (pfoldMap)
import Plutarch.Monadic qualified as P

import Lending.Contracts.Account.Types
  ( PAccountDatum
  , PClearRequest (PClearBorrowing, PClearSupplying)
  , PRequest (PBorrowRequest, PRepayRequest, PSupplyRequest, PWithdrawRequest)
  )
import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Common
  ( getAssetClassData
  , getInlineDatum
  , hasUniqueAsset
  , isBorrowableToken
  , isSupplyableToken
  , passetClassValuePositive
  )
import Lending.Contracts.Exchange (PReceipt)
import Lending.Contracts.Manager.Types (PManagerDatum, PRiskParameters)

data UnchangableAccountFields (s :: S) = UnchangableAccountFields
  { uafAddress :: Term s PAddress
  , uafSupplyAssets :: Term s (PAsData (PMap 'Sorted PAsset PReceipt))
  , uafBorrowTokens :: Term s (PAsData (PMap 'Sorted PAsset PReceipt))
  , uafCollateralAsset :: Term s (PAsData (PMap 'Sorted PAsset PBool))
  , uafUserNft :: Term s (PAsData PAssetClassData)
  , uafBeingLiquidated :: Term s (PMaybeData (PAsData (PMap 'Sorted PAsset PReceipt)))
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType UnchangableAccountFields where
  type DPTStrat _ = PlutusTypeScott

valueFromRequestUpdate
  :: Term
      s
      ( PMap 'Sorted PAsset PRiskParameters
          :--> PValue 'Sorted 'Positive
          :--> PValue 'Sorted 'Positive
          :--> PRequest
          :--> PValue 'Sorted 'Positive
      )
valueFromRequestUpdate =
  phoistAcyclic $ plam $ \riskParameters minAdaUtxoAndBathcherFeeValue batcherFeeValue request' -> P.do
    pmatch request' $ \case
      PSupplyRequest req -> P.do
        request <- pletFields @["srAsset", "srAmount"] req
        asset <- plet $ request.srAsset
        amount <- plet $ request.srAmount
        pif
          (isSupplyableToken # riskParameters # asset)
          ( passetClassValuePositive
              # (ptoScottEncoding # (getAssetClassData # asset # riskParameters))
              # (pextract # amount)
              <> batcherFeeValue
          )
          (ptraceError $ "Supply asset is not valid: " <> pshow asset)
      PWithdrawRequest req -> P.do
        -- TODO: Check if asset is withdrawable
        amount <- plet $ pfromData $ pfield @"wrAmount" # req
        pif
          (0 #<= amount)
          minAdaUtxoAndBathcherFeeValue
          (ptraceError $ "Negative Withdraw request amount: " <> pshow amount)
      PBorrowRequest req -> P.do
        amount <- plet $ pfromData $ pfield @"brAmount" # req
        asset <- plet $ pfield @"brAsset" # req
        pif
          (0 #<= amount #&& isBorrowableToken # riskParameters # asset)
          minAdaUtxoAndBathcherFeeValue
          (ptraceError $ "Borrow asset is not valid: " <> pshow asset <> ": " <> pshow amount)
      PRepayRequest req -> P.do
        -- TODO: Check if asset is repayable
        request <- pletFields @["rrAsset", "rrAmount"] req
        asset <- plet $ request.rrAsset
        amount <- plet $ request.rrAmount
        passetClassValuePositive
          # (ptoScottEncoding # (getAssetClassData # asset # riskParameters))
          # (pextract # amount)
          <> batcherFeeValue

valueFromClearRequest
  :: Term
      s
      ( PMap 'Sorted PAsset PRiskParameters
          :--> PValue 'Sorted 'Positive
          :--> PAsset
          :--> PClearRequest
          :--> PValue 'Sorted 'Positive
      )
valueFromClearRequest = phoistAcyclic $
  plam $ \riskParams minAdaUtxoAndBathcherFeeValue asset clearRequest' -> P.do
    pmatch clearRequest' $ \case
      PClearSupplying _ -> minAdaUtxoAndBathcherFeeValue
      PClearBorrowing clearBorrowing' -> P.do
        clearBorrowing <- pletFields @'["cbLimited"] clearBorrowing'
        passetClassValuePositive
          # (ptoScottEncoding # (getAssetClassData # asset # riskParams))
          # (pextract # clearBorrowing.cbLimited)
          <> minAdaUtxoAndBathcherFeeValue

calculateAccountValue
  :: Term s (PManagerDatum :--> PAccountDatum :--> PValue 'Sorted 'Positive)
calculateAccountValue = phoistAcyclic $ plam $ \manager' accountDatum' -> P.do
  manager <-
    pletFields
      @["mdAccountAuthToken", "mdRiskParameters", "mdGlobalRiskParameters"]
      manager'
  accountAuthAsset <- plet manager.mdAccountAuthToken
  globalRiskParameters <- pletFields @["grpMinAdaUtxo", "grpBatcherFee"] manager.mdGlobalRiskParameters
  accountDatum <- pletAll accountDatum'

  minAdaUtxo <- plet $ globalRiskParameters.grpMinAdaUtxo
  batcherFee <- plet $ globalRiskParameters.grpBatcherFee
  minAdaUtxoAndBathcherFeeValue <- plet $ passetClassValuePositive # pto padaClass # (minAdaUtxo + batcherFee)
  batcherFeeValue <- plet $ passetClassValuePositive # pto padaClass # batcherFee
  requests <- plet $ accountDatum.adNormalRequests
  mapClearRequest <- plet $ accountDatum.adClearRequests
  riskParameters <- plet $ manager.mdRiskParameters
  requestsValue <-
    plet $
      pfoldMap
        # (valueFromRequestUpdate # riskParameters # minAdaUtxoAndBathcherFeeValue # batcherFeeValue)
        # requests
  collateralUpdateValue <-
    plet $
      pif
        ( (pisDJust # accountDatum.adCollateralUpdate)
            #&& (pnull # requests)
            #&& (Map.pnull # mapClearRequest)
        )
        batcherFeeValue
        mempty

  clearRequestValue <-
    plet $
      pfoldMapWithKey
        # (valueFromClearRequest # riskParameters # minAdaUtxoAndBathcherFeeValue)
        # mapClearRequest
  minAdaValue <- plet $ passetClassValuePositive # pto padaClass # accountDatum.adExtraLovelace
  minAdaValue
    <> passetClassValuePositive # (ptoScottEncoding # accountAuthAsset) # 1
    <> requestsValue
    <> collateralUpdateValue
    <> clearRequestValue

checkAccountWithRequests
  :: Term
      s
      ( PBuiltinList PTxOut
          :--> PManagerDatum
          :--> UnchangableAccountFields
          :--> PBool
      )
checkAccountWithRequests = phoistAcyclic $ plam $ \outputs manager' unchangeAccFields' -> P.do
  manager <-
    pletFields
      @["mdAccountAuthToken", "mdRiskParameters"]
      manager'
  accountAuthAsset <- plet manager.mdAccountAuthToken

  -- Extract account output
  outputAccountTxOut' <- plet $ ptryFromSingleton #$ pfilter # (hasUniqueAsset # accountAuthAsset) # outputs
  outputAccountTxOut <- pletFields @["address", "datum", "value"] outputAccountTxOut'

  UnchangableAccountFields {..} <- pmatch unchangeAccFields'

  -- Check address
  checkAddress <- plet $ outputAccountTxOut.address #== uafAddress

  -- Check datum unchangable fields
  newDatum' <- plet $ getInlineDatum @PAccountDatum # pfromData outputAccountTxOut.datum
  newDatum <- pletAll newDatum'
  checkUnchangableFields <-
    plet $
      uafSupplyAssets #== newDatum.adSupplies
        #&& uafBorrowTokens #== newDatum.adBorrowings
        #&& uafCollateralAsset #== newDatum.adCollateralAssets
        #&& uafUserNft #== newDatum.adUserNft
        #&& uafBeingLiquidated #== newDatum.adProtocolIncentive

  -- Check value
  expectedValue <-
    plet $ calculateAccountValue # manager' # newDatum'

  -- Check output address
  checkAddress
    -- Check unchangable fields
    #&& checkUnchangableFields
    -- Check output value
    #&& expectedValue #== outputAccountTxOut.value
