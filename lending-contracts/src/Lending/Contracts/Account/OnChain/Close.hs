{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Contracts.Account.OnChain.Close
  ( validateClose
  )
where

import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Extra.AssetClass (PAssetClassData, ptoScottEncoding)
import Plutarch.Monadic qualified as P

import Lending.Contracts.Common (getState, phasOneTokenOfAssetClassBurned, uniqueAssetFromTxInInfo)
import Lending.Contracts.Manager.Types (PManagerDatum)

validateClose :: Term s (PAssetClassData :--> PScriptContext :--> PBool)
validateClose = phoistAcyclic $ plam $ \managerAuthToken ctx' -> P.do
  ctx <- pletFields @'["txInfo"] ctx'
  txinfo <- pletFields @["inputs", "referenceInputs", "mint"] ctx.txInfo
  refInputs <- plet $ txinfo.referenceInputs
  manager' <- plet $ getState @PManagerDatum # managerAuthToken # refInputs
  manager <- pletFields @'["mdAccountAuthToken"] manager'

  accountAuthAsset <- plet $ manager.mdAccountAuthToken
  inputs <- plet $ txinfo.inputs
  -- Consume only one account
  (plength # (pfilter # (uniqueAssetFromTxInInfo # accountAuthAsset) # inputs) #== pconstant 1)
    -- Must burn exactly one account authentic token
    #&& (phasOneTokenOfAssetClassBurned # (ptoScottEncoding # accountAuthAsset) # txinfo.mint)
