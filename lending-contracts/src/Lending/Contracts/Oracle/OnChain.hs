{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Contracts.Oracle.OnChain
  ( oracleValidatorTerm
  )
where

import Plutarch.Api.V2 (PScriptPurpose (PSpending), PValidator)
import Plutarch.Monadic qualified as P

import Lending.Contracts.Common (uniqueAssetFromTxInInfo)
import Lending.Types.Oracle (OracleScriptParams (OracleScriptParams))

oracleValidatorTerm :: OracleScriptParams -> ClosedTerm PValidator
oracleValidatorTerm (OracleScriptParams operatorOracleNft) =
  plam $ \_ _ ctx' -> P.do
    ctx <- pletFields @["txInfo", "purpose"] ctx'
    PSpending _ <- pmatch ctx.purpose
    inputs <- plet $ pfromData $ pfield @"inputs" # ctx.txInfo
    pif
      (pany # (uniqueAssetFromTxInInfo # pconstant operatorOracleNft) # inputs)
      (popaque $ pconstant ())
      (popaque perror)
