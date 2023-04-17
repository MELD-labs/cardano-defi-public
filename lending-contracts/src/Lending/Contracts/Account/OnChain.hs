{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Contracts.Account.OnChain
  ( PAccountDatum (..)
  , PAccountRedeemer (..)
  , validateAccount
  , accountValidatorTerm
  )
where

import Plutarch.Api.V2 (PValidator)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Extra.AssetClass (AssetClass, PAssetClassData)
import Plutarch.Monadic qualified as P

import Lending.Contracts.Account.OnChain.Apply (validateApply)
import Lending.Contracts.Account.OnChain.Close (validateClose)

import Lending.Contracts.Account.OnChain.Liquidate (validateLiquidate)
import Lending.Contracts.Account.OnChain.Update (validateUpdate)
import Lending.Contracts.Account.Types
  ( PAccountDatum (PAccountDatum)
  , PAccountRedeemer
    ( PAccountApplyRedeemer
    , PAccountCloseRedeemer
    , PAccountLiquidateRedeemer
    , PAccountMigrateRedeemer
    , PAccountUpdateRedeemer
    )
  )
import Lending.Contracts.Common
  ( ptryFromData
  )
import Lending.Contracts.Migrate (validateMigrate)

validateAccount
  :: Term
      s
      (PAssetClassData :--> PAssetClassData :--> PData :--> PAccountRedeemer :--> PScriptContext :--> PBool)
validateAccount = phoistAcyclic $ plam $ \managerAuthToken migrationNft datum redeemer ctx' -> P.do
  pmatch redeemer $ \case
    PAccountUpdateRedeemer _ -> P.do
      validateUpdate
        # managerAuthToken
        # pfromData (ptryFromData @PAccountDatum datum)
        # ctx'
    PAccountApplyRedeemer _ ->
      validateApply
        # managerAuthToken
        # ctx'
    PAccountCloseRedeemer _ ->
      validateClose
        # managerAuthToken
        # ctx'
    PAccountLiquidateRedeemer re -> P.do
      liquidateRedeemerData <- plet $ pfield @"alrData" # re
      validateLiquidate
        # managerAuthToken
        # pfromData (ptryFromData @PAccountDatum datum)
        # liquidateRedeemerData
        # ctx'
    PAccountMigrateRedeemer _ -> P.do
      validateMigrate
        # migrationNft
        # ctx'

accountValidatorTerm :: AssetClass -> AssetClass -> ClosedTerm PValidator
accountValidatorTerm managerAuthToken migrationNft =
  plam $ \datum redeemer ctx ->
    pif
      ( validateAccount
          # pconstant @PAssetClassData managerAuthToken
          # pconstant @PAssetClassData migrationNft
          # datum
          # pfromData (ptryFromData @PAccountRedeemer redeemer)
          # ctx
      )
      (popaque $ pconstant ())
      (popaque perror)
