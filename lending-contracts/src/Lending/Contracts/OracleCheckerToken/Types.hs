{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Contracts.OracleCheckerToken.Types (POracleCheckerTokenRedeemer (..)) where

import Plutarch.Api.V2 (KeyGuarantees (Sorted), PMap)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))

import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Exchange (PPrice)
import Lending.Contracts.Orphans ()
import Lending.Types.OracleCheckerToken (OracleCheckerTokenRedeemer)

newtype POracleCheckerTokenRedeemer (s :: S)
  = POracleCheckerTokenRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "odAssetPrices" ':= PMap 'Sorted PAsset PPrice
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)
instance DerivePlutusType POracleCheckerTokenRedeemer where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl POracleCheckerTokenRedeemer where
  type PLifted POracleCheckerTokenRedeemer = OracleCheckerTokenRedeemer
deriving via
  (DerivePConstantViaData OracleCheckerTokenRedeemer POracleCheckerTokenRedeemer)
  instance
    PConstantDecl OracleCheckerTokenRedeemer
instance PTryFrom PData POracleCheckerTokenRedeemer
instance PTryFrom PData (PAsData POracleCheckerTokenRedeemer)
