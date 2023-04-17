{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Contracts.Account.OnChain.Apply
  ( validateApply
  )
where

import Plutarch.Api.V2 (PAddress, PScriptContext, PScriptPurpose (PSpending), PTxInInfo, PTxOutRef)
import Plutarch.Extra.AssetClass (PAssetClassData)
import Plutarch.Monadic qualified as P

import Lending.Contracts.Common (getState, uniqueAssetFromTxInInfo)
import Lending.Contracts.Manager.Types (PManagerDatum)
import Plutarch.Extra.List (pfindJust)
import Plutarch.Extra.Maybe (pfromMaybe, pjust, pnothing)
import Plutarch.Extra.ScriptContext (pfindTxInByTxOutRef)
import Plutarch.Maybe (pfromJust)

checkInput :: Term s (PTxOutRef :--> PAddress :--> PTxInInfo :--> PMaybe PBool)
checkInput = phoistAcyclic $ plam $ \curTxOutRef curAddress txInInfo' -> P.do
  txInInfo <- pletFields @["outRef", "resolved"] txInInfo'
  pif
    (curTxOutRef #== txInInfo.outRef)
    (pjust # pconstant True)
    (pif (pfield @"address" # txInInfo.resolved #== curAddress) (pjust # pconstant False) pnothing)

-- | Optimization
validateApply :: Term s (PAssetClassData :--> PScriptContext :--> PBool)
validateApply = phoistAcyclic $ plam $ \managerAuthToken ctx' -> P.do
  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  txinfo <- pletFields @["inputs", "referenceInputs"] ctx.txInfo
  inputs <- plet $ txinfo.inputs
  PSpending spending <- pmatch ctx.purpose
  curTxOutRef <- plet $ pfield @"_0" # spending
  curTxInInfo <- plet $ pfromJust #$ pfindTxInByTxOutRef # curTxOutRef # inputs
  curResolved <- plet $ pfield @"resolved" # curTxInInfo
  curAddress <- plet $ pfield @"address" # curResolved
  shouldFindPool <- plet $ pfindJust # (checkInput # curTxOutRef # curAddress) # inputs
  pif
    (pfromMaybe # pconstant True # shouldFindPool)
    ( P.do
        refInputs <- plet $ txinfo.referenceInputs
        manager' <- plet $ getState @PManagerDatum # managerAuthToken # refInputs
        manager <- pletFields @'["mdPoolNft"] manager'
        pany # (uniqueAssetFromTxInInfo # manager.mdPoolNft) # inputs
    )
    (pconstant True)
