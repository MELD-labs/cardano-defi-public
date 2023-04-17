{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}

module Lending.Contracts.Migrate (validateMigrate) where

import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Extra.AssetClass (PAssetClassData)
import Plutarch.Monadic qualified as P

import Lending.Contracts.Common (uniqueAssetFromTxInInfo)

validateMigrate :: Term s (PAssetClassData :--> PScriptContext :--> PBool)
validateMigrate = phoistAcyclic $ plam $ \migrationNft ctx' -> P.do
  ctx <- pletFields @'["txInfo"] ctx'
  txinfo <- pletFields @'["inputs"] ctx.txInfo

  inputs <- plet $ txinfo.inputs
  -- Must consume migrate NFT
  pany # (uniqueAssetFromTxInInfo # migrationNft) # inputs
