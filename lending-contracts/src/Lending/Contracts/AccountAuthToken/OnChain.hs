{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lending.Contracts.AccountAuthToken.OnChain
  ( accountAuthTokenName
  , accountPolicyTerm
  )
where

import Plutarch.Api.V1.Address (PAddress)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Scripts (PRedeemer (PRedeemer))
import Plutarch.Api.V2
  ( PDatum (PDatum)
  , PMintingPolicy
  , POutputDatum (POutputDatum)
  , PScriptContext
  , PScriptPurpose (PMinting, PSpending)
  , PTxInInfo
  , PTxOut
  )
import Plutarch.Extra.AssetClass
  ( AssetClass
  , PAssetClass (PAssetClass)
  , PAssetClassData
  , pfromScottEncoding
  , ptoScottEncoding
  )
import Plutarch.Extra.List (ptryFromSingleton)
import Plutarch.Extra.Map (ptryLookup)
import Plutarch.Extra.Maybe (pdnothing)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.ScriptContext (pownMintValue, ptryFromInlineDatum)
import Plutarch.Extra.Value (phasOneTokenOfAssetClass)
import Plutarch.Monadic qualified as P
import PlutusLedgerApi.V2 qualified as Plutus

import Lending.Contracts.Account.Types (PAccountDatum (PAccountDatum), PAccountRedeemer (PAccountLiquidateRedeemer))
import Lending.Contracts.AccountAuthToken.Types (PAccountTokenRedeemer (PBurn, PMigrate, PMint))
import Lending.Contracts.Common
  ( checkValueInMapIsZero
  , findAccountTxIns
  , getState
  , hasUniqueAsset
  , phasOneTokenOfAssetClassBurned
  , ptraceAssertEqual
  , ptryFromData
  , uniqueAssetFromTxInInfo
  )
import Lending.Contracts.Manager.Types (PManagerDatum)
import Lending.Contracts.Request (calculateAccountValue)
import Plutarch.Builtin (PIsData (pdataImpl))

accountAuthTokenName :: Plutus.TokenName
accountAuthTokenName = ""

validateAccountAuthTokenMinting
  :: Term s (PAssetClassData :--> PAssetClassData :--> PAccountTokenRedeemer :--> PScriptContext :--> PBool)
validateAccountAuthTokenMinting = phoistAcyclic $ plam $ \managerAuthToken migrationNft redeemer ctx' -> P.do
  mintedValue <- plet $ pownMintValue # ctx'
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  txInfo <- pletFields @["inputs", "outputs", "redeemers", "referenceInputs"] ctx.txInfo
  txInputs <- plet txInfo.inputs
  refInputs <- plet txInfo.referenceInputs
  PMinting ownMintCurr <- pmatch $ pfromData ctx.purpose
  let accountAuthCs = pfield @"_0" # ownMintCurr
      accountAuthTn = pconstantData accountAuthTokenName
      accountAuthAc = pcon $ PAssetClass accountAuthCs accountAuthTn
  pmatch redeemer $ \case
    PMint re -> P.do
      manager' <- plet $ getState @PManagerDatum # managerAuthToken # refInputs
      accountAddr <- plet $ pfield @"mdAccountAddress" # manager'
      let userNftAc = pfield @"_0" # re
      accInputs <- plet $ pfilter # (uniqueAssetFromTxInInfo #$ pfromScottEncoding # accountAuthAc) # txInputs

      ptraceIfFalse
        "Must mint exactly one account authentic token"
        (phasOneTokenOfAssetClass # accountAuthAc # mintedValue)
        #&& ptraceIfFalse
          "Must mint exactly one owner nft"
          (phasOneTokenOfAssetClass # (ptoScottEncoding # pfromData userNftAc) # mintedValue)
        #&& pelimList
          (checkLiquidatedAccount txInfo.redeemers)
          ( checkNewAccountOutput
              # txInfo.outputs
              # accountAddr
              # accountAuthAc
              # userNftAc
              # manager'
          )
          accInputs
    PBurn _ -> P.do
      (checkAccountOutputBurning # txInputs # accountAuthAc)
        #&& (phasOneTokenOfAssetClassBurned # accountAuthAc # mintedValue)
    PMigrate _ -> pany # (uniqueAssetFromTxInInfo # migrationNft) # txInputs

checkAccountOutputBurning :: Term s (PBuiltinList PTxInInfo :--> PAssetClass :--> PBool)
checkAccountOutputBurning = phoistAcyclic $ plam \txIns accAuthAc -> P.do
  accountTxIns <- plet $ findAccountTxIns # txIns # (pfromScottEncoding # accAuthAc)
  accTxIn <- pletFields @["address", "datum", "value"] $ ptryFromSingleton # accountTxIns
  PDatum datum <- pmatch $ ptryFromInlineDatum # accTxIn.datum
  let accDatum' = pfromData $ ptryFromData @PAccountDatum datum
  accDatum <- pletFields @["adSupplies", "adBorrowings", "adNormalRequests", "adUserNft"] accDatum'
  -- Input include userNFT
  (pany # (uniqueAssetFromTxInInfo # accDatum.adUserNft) # txIns)
    -- Check account datum
    #&& (checkValueInMapIsZero # accDatum.adSupplies)
    #&& (checkValueInMapIsZero # accDatum.adBorrowings)

checkNewAccountOutput
  :: Term
      s
      ( PBuiltinList PTxOut
          :--> PAddress
          :--> PAssetClass
          :--> PAsData PAssetClassData
          :--> PManagerDatum
          :--> PBool
      )
checkNewAccountOutput =
  phoistAcyclic $ plam $ \txouts' expectAddr accAuthAc userNftAs manager -> P.do
    accTxOuts <- plet $ pfilter # (hasUniqueAsset #$ pfromScottEncoding # accAuthAc) # txouts'
    accTxOut <- plet $ ptryFromSingleton # accTxOuts
    output <- pletFields @["value", "datum", "address"] accTxOut
    POutputDatum outputDatum <- pmatch output.datum
    PDatum datum' <- pmatch $ pfromData $ pfield @"outputDatum" # outputDatum
    accDatum' <- plet $ pfromData $ ptryFromData @PAccountDatum datum'
    datumChangableFields <- pletFields @["adNormalRequests", "adCollateralAssets", "adExtraLovelace"] accDatum'
    pendingRequests <- plet $ datumChangableFields.adNormalRequests
    extraLovelace <- plet $ datumChangableFields.adExtraLovelace
    expectedDatum <-
      plet $
        mkRecordConstr PAccountDatum $
          #adSupplies .= pdata AssocMap.pempty
            .& #adBorrowings .= pdata AssocMap.pempty
            .& #adCollateralAssets .= datumChangableFields.adCollateralAssets
            .& #adUserNft .= userNftAs
            .& #adNormalRequests .= pendingRequests
            .& #adCollateralUpdate .= pdata pdnothing
            .& #adProtocolIncentive .= pdata pdnothing
            .& #adClearRequests .= pdata AssocMap.pempty
            .& #adExtraLovelace .= extraLovelace
    expectedValue <-
      plet $ calculateAccountValue # manager # expectedDatum
    ptraceIfFalse
      ("Account address does not match: \nExpect: " <> pshow expectAddr <> "\nActual: " <> pshow output.address)
      (output.address #== expectAddr)
      #&& ptraceIfFalse
        ("Account datum does not match: \nExpect: " <> pshow (pdataImpl expectedDatum) <> "\nActual: " <> pshow datum')
        (datum' #== pdataImpl expectedDatum)
      #&& ptraceAssertEqual "Account value does not match" expectedValue output.value

isLiquidateRedeemer :: Term s (PData :--> PBool)
isLiquidateRedeemer = phoistAcyclic $ plam $ \redeemerData -> P.do
  redeemer <- plet $ pfromData $ ptryFromData @PAccountRedeemer redeemerData
  pmatch redeemer $ \case
    PAccountLiquidateRedeemer _ -> pconstant True
    _ -> pconstant False

checkLiquidatedAccount
  :: Term s (AssocMap.PMap 'AssocMap.Unsorted PScriptPurpose PRedeemer)
  -> Term s PTxInInfo
  -> Term s (PBuiltinList PTxInInfo)
  -> Term s PBool
checkLiquidatedAccount redeemers accountInput remainingAccInputs = P.do
  accInputTxOutRef <- plet $ pfromData $ pfield @"outRef" #$ accountInput
  spendingAcc <- plet $ mkRecordConstr PSpending $ #_0 .= pdata accInputTxOutRef
  accRedeemer' <- plet $ ptryLookup # spendingAcc # redeemers
  PRedeemer accRedeemer <- pmatch accRedeemer'
  (isLiquidateRedeemer # accRedeemer) #&& (pnull # remainingAccInputs)

accountPolicyTerm :: AssetClass -> AssetClass -> ClosedTerm PMintingPolicy
accountPolicyTerm managerAuthToken migrationNft =
  plam $ \redeemer ctx -> P.do
    let accRedeemer = pfromData $ ptryFromData @PAccountTokenRedeemer redeemer
    pif
      ( validateAccountAuthTokenMinting
          # pconstant @PAssetClassData managerAuthToken
          # pconstant @PAssetClassData migrationNft
          # accRedeemer
          # ctx
      )
      (popaque $ pconstant ())
      (popaque perror)
