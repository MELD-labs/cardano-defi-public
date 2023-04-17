{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module Lending.Contracts.Pool.Withdraw (validateUpdateTreasuryPool) where

import Plutarch.Api.V1.Address (PCredential)
import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2
  ( AmountGuarantees (Positive)
  , KeyGuarantees (Sorted)
  , PDatum (PDatum)
  , POutputDatum (POutputDatum)
  , PValue
  )
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Api.V2.Tx (PTxOut)
import Plutarch.Extra.AssetClass (PAssetClassData, ptoScottEncoding)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.ScriptContext (ptryOwnInput)
import Plutarch.Extra.Value (passetClassValueOf, phasOneTokenOfAssetClass)
import Plutarch.Monadic qualified as P

import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Common
  ( getAssetClassData
  , getState
  , ptryFromData
  , uniqueAssetFromTxInInfo
  )
import Lending.Contracts.Exchange (PActual, convertFrom)
import Lending.Contracts.Manager.Types (PManagerDatum, PRiskParameters)
import Lending.Contracts.Pool.Types (PAssetInformation, PPoolDatum)
import Plutarch.Extra.Map (pfoldlWithKey)

validateUpdateTreasuryPool
  :: Term s (PAssetClassData :--> PPoolDatum :--> PScriptContext :--> PBool)
validateUpdateTreasuryPool = phoistAcyclic $ plam $ \managerAuthToken oldPoolDatum ctx' -> P.do
  ctx <- pletFields @'["txInfo"] ctx'
  txinfo <- pletFields @'["inputs", "referenceInputs", "outputs"] ctx.txInfo

  refInputs <- plet txinfo.referenceInputs
  inputs <- plet $ txinfo.inputs
  outputs <- plet $ txinfo.outputs

  manager' <- plet $ getState @PManagerDatum # managerAuthToken # refInputs -- ###### 39M
  manager <- pletAll manager'
  poolNft <- plet $ manager.mdPoolNft
  operatorNft <- plet $ manager.mdTreasuryOperatorNft

  unresolvedPoolInput <- plet $ ptryOwnInput # ctx'
  resolvedPoolInput <- plet $ pfield @"resolved" # unresolvedPoolInput
  poolInput <- pletFields @["address", "value"] resolvedPoolInput
  poolInputAddress <- plet $ pfield @"credential" # poolInput.address
  mapAssetSupply <- plet $ pfield @"pdAssets" # oldPoolDatum

  findPoolOutputResult <- plet $ findPoolOutput # poolNft # poolInputAddress # outputs -- ###### 25M
  PPair poolOutput _ <- pmatch findPoolOutputResult

  PPair newPoolDatum newPoolValue <- pmatch poolOutput

  unborrowedMap <- plet $ AssocMap.pmap # subtractBorrowFromSupply # mapAssetSupply

  checkWithdrawValue <-
    plet $
      pfoldlWithKey
        # (checkWithdrawAmount # manager.mdRiskParameters # newPoolValue)
        # pconstant True
        # unborrowedMap

  -- Consume operator Nft
  pany # (uniqueAssetFromTxInInfo # operatorNft) # inputs
    -- Can't withdraw more than the treasury amount
    #&& checkWithdrawValue
    -- Pool datum not change
    #&& (newPoolDatum #== oldPoolDatum)

findPoolOutput
  :: Term
      s
      ( PAssetClassData
          :--> PCredential
          :--> PBuiltinList PTxOut
          :--> PPair (PPair PPoolDatum (PValue 'Sorted 'Positive)) (PBuiltinList PTxOut)
      )
findPoolOutput = phoistAcyclic $ plam $ \authTokenData address -> P.do
  authToken <- plet $ ptoScottEncoding # authTokenData
  precList
    ( \self txOut remaining -> P.do
        output <- pletFields @["address", "value", "datum"] txOut
        outputValue <- plet $ output.value
        outputAddress <- plet $ pfield @"credential" # output.address
        outputTxOutDatum <- plet $ output.datum
        pif
          (phasOneTokenOfAssetClass # authToken # outputValue)
          ( P.do
              POutputDatum outputDatum <- pmatch outputTxOutDatum
              PDatum datum <- pmatch $ pfield @"outputDatum" # outputDatum
              _ <-
                plet $
                  pif (outputAddress #== address) (pconstant ()) (ptraceError "Pool output address doesn't match")
              poolOutput <- plet $ pcon $ PPair (pfromData (ptryFromData @PPoolDatum datum)) outputValue
              pcon (PPair poolOutput remaining)
          )
          (self # remaining)
    )
    (const (ptraceError "Cannot find pool output"))

subtractBorrowFromSupply :: Term s (PAssetInformation :--> PActual)
subtractBorrowFromSupply = phoistAcyclic $ plam \oldAssetInfo' -> P.do
  oldAssetInfo <- pletAll oldAssetInfo'
  borrowingRate <- plet oldAssetInfo.aiCumulatedInterestRateBorrowing
  lendingRate <- plet oldAssetInfo.aiCumulatedInterestRateSupplying
  (convertFrom # lendingRate # oldAssetInfo.aiSupplyAmount)
    - (convertFrom # borrowingRate # oldAssetInfo.aiBorrowAmount)

checkWithdrawAmount
  :: Term
      s
      ( PMap 'Sorted PAsset PRiskParameters
          :--> PValue 'Sorted 'Positive
          :--> PBool
          :--> PAsset
          :--> PActual
          :--> PBool
      )
checkWithdrawAmount =
  phoistAcyclic $ plam \riskParams poolValue result asset unborrowedAmount -> P.do
    assetClass <- plet $ ptoScottEncoding #$ getAssetClassData # asset # riskParams
    cashAmount <- plet $ passetClassValueOf # assetClass # poolValue
    -- supply + treasury = borrow + cash
    result #&& (pto unborrowedAmount #<= cashAmount)
