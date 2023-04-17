{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Contracts.Manager.OnChain
  ( managerValidator
  )
where

import Plutarch.Api.V2
  ( PDatum (PDatum)
  , POutputDatum (POutputDatum)
  , PTxOut
  , PValidator
  )
import Plutarch.Extra.AssetClass (PAssetClassData, ptoScottEncoding)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.Value (phasOneTokenOfAssetClass)
import Plutarch.Monadic qualified as P

import Lending.Contracts.Common (ptryFromData, uniqueAssetFromTxInInfo)
import Lending.Contracts.Manager.Types (PManagerDatum)
import Lending.Types.Manager (ManagerScriptParams (ManagerScriptParams))

extractManagerOutput :: Term s (PAssetClassData :--> PBuiltinList PTxOut :--> PManagerDatum)
extractManagerOutput = phoistAcyclic $ plam $ \authTokenData -> P.do
  authToken <- plet $ ptoScottEncoding # authTokenData
  precList
    ( \self txOut remaining -> P.do
        output <- pletFields @["value", "datum"] txOut
        outputValue <- plet $ output.value
        outputTxOutDatum <- plet $ output.datum
        pif
          (phasOneTokenOfAssetClass # authToken # outputValue)
          ( P.do
              POutputDatum outputDatum <- pmatch outputTxOutDatum
              PDatum datum <- pmatch $ pfield @"outputDatum" # outputDatum
              pfromData (ptryFromData @PManagerDatum datum)
          )
          (self # remaining)
    )
    (const (ptraceError "Cannot find manager output"))

checkFieldsManagerOperatorCannotUpdate
  :: Term
      s
      (PManagerDatum :--> PAssetClassData :--> PBuiltinList PTxOut :--> PBool)
checkFieldsManagerOperatorCannotUpdate = phoistAcyclic $ plam $ \datum' managerAuthToken outputs -> P.do
  datum <- pletAll datum'
  newDatum <- pletAll $ extractManagerOutput # managerAuthToken # outputs
  datum.mdAccountAuthToken #== newDatum.mdAccountAuthToken
    #&& datum.mdPoolNft #== newDatum.mdPoolNft
    #&& datum.mdTreasuryOperatorNft #== newDatum.mdTreasuryOperatorNft
    #&& datum.mdRiskParamsOperatorNft #== newDatum.mdRiskParamsOperatorNft
    #&& datum.mdAccountAddress #== newDatum.mdAccountAddress
    #&& datum.mdMaxValidityDuration #== newDatum.mdMaxValidityDuration
    #&& datum.mdOracleCheckerToken #== newDatum.mdOracleCheckerToken

managerValidator :: ManagerScriptParams -> ClosedTerm PValidator
managerValidator (ManagerScriptParams migrationNft managerAuthToken) =
  plam $ \datum' _ ctx' -> P.do
    ctx <- pletFields @["txInfo", "purpose"] ctx'
    inputs <- plet $ pfromData $ pfield @"inputs" # ctx.txInfo
    outputs <- plet $ pfromData $ pfield @"outputs" # ctx.txInfo
    datum <- plet $ pfromData (ptryFromData @PManagerDatum datum')

    operatorManagerNft <- plet $ pfield @"mdRiskParamsOperatorNft" # datum

    pif
      (pany # (uniqueAssetFromTxInInfo # pconstant migrationNft) # inputs)
      (popaque $ pconstant ())
      ( pif
          ( (pany # (uniqueAssetFromTxInInfo # operatorManagerNft) # inputs)
              #&& (checkFieldsManagerOperatorCannotUpdate # datum # pconstant managerAuthToken # outputs)
          )
          (popaque $ pconstant ())
          (popaque $ ptraceError "Cannot consume Manager utxo")
      )
