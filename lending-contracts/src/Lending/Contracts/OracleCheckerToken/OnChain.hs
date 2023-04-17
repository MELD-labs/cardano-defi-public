{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lending.Contracts.OracleCheckerToken.OnChain
  ( oracleCheckerTokenName
  , oracleCheckerPolicyTerm
  )
where

import Plutarch.Api.V2
  ( PMintingPolicy
  , PScriptContext
  , PScriptPurpose (PMinting)
  )
import Plutarch.Extra.AssetClass
  ( AssetClass
  , PAssetClassData
  )
import Plutarch.Monadic qualified as P
import PlutusLedgerApi.V2 qualified as Plutus

import Lending.Contracts.Common
  ( getState
  , ptraceAssertEqual
  , ptryFromData
  )
import Lending.Contracts.Oracle.Types (POracleDatum)
import Lending.Contracts.OracleCheckerToken.Types (POracleCheckerTokenRedeemer (POracleCheckerTokenRedeemer))

oracleCheckerTokenName :: Plutus.TokenName
oracleCheckerTokenName = ""

validateOracleCheckerTokenMinting
  :: Term s (PAssetClassData :--> POracleCheckerTokenRedeemer :--> PScriptContext :--> PBool)
validateOracleCheckerTokenMinting = phoistAcyclic $ plam $ \oracleNft redeemer ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  refInputs <- plet $ pfield @"referenceInputs" # ctx.txInfo
  PMinting _ <- pmatch $ pfromData ctx.purpose
  POracleCheckerTokenRedeemer rPrices' <- pmatch redeemer
  rPrices <- plet $ pfield @"odAssetPrices" # rPrices'
  oracleDatum' <- plet $ getState @POracleDatum # oracleNft # refInputs
  oracleDatum <- plet $ pfield @"odAssetPrices" # oracleDatum'
  ptraceAssertEqual "Oracle prices do not match" oracleDatum rPrices

oracleCheckerPolicyTerm :: AssetClass -> ClosedTerm PMintingPolicy
oracleCheckerPolicyTerm oracleNft =
  plam $ \redeemer ctx -> P.do
    let accRedeemer = pfromData $ ptryFromData @POracleCheckerTokenRedeemer redeemer
    pif
      ( validateOracleCheckerTokenMinting
          # pconstant @PAssetClassData oracleNft
          # accRedeemer
          # ctx
      )
      (popaque $ pconstant ())
      (popaque perror)
