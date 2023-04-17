{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Contracts.AccountAuthToken.Types (PAccountTokenRedeemer (..)) where

import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.Extra.AssetClass (PAssetClassData)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))

import Lending.Contracts.Orphans ()
import Lending.Types.Account (AccountTokenRedeemer)

data PAccountTokenRedeemer (s :: S)
  = PMint (Term s (PDataRecord '["_0" ':= PAssetClassData]))
  | PBurn (Term s (PDataRecord '[]))
  | PMigrate (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)
instance DerivePlutusType PAccountTokenRedeemer where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PAccountTokenRedeemer where type PLifted PAccountTokenRedeemer = AccountTokenRedeemer
deriving via
  (DerivePConstantViaData AccountTokenRedeemer PAccountTokenRedeemer)
  instance
    PConstantDecl AccountTokenRedeemer
instance PTryFrom PData PAccountTokenRedeemer
instance PTryFrom PData (PAsData PAccountTokenRedeemer)
