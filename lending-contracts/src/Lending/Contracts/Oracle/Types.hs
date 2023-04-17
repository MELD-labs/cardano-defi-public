{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Contracts.Oracle.Types where

import Plutarch.Api.V1.AssocMap (KeyGuarantees (Sorted), PMap)
import Plutarch.DataRepr
  ( DerivePConstantViaData (DerivePConstantViaData)
  , PDataFields
  )
import Plutarch.Lift
  ( PConstantDecl
  , PLifted
  , PUnsafeLiftDecl
  )

import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Exchange (PPrice)
import Lending.Contracts.Orphans ()
import Lending.Types.Oracle (OracleDatum)

newtype POracleDatum (s :: S)
  = POracleDatum
      ( Term
          s
          ( PDataRecord
              '[ "odAssetPrices" ':= PMap 'Sorted PAsset PPrice
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)
instance DerivePlutusType POracleDatum where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl POracleDatum where type PLifted POracleDatum = OracleDatum
deriving via
  (DerivePConstantViaData OracleDatum POracleDatum)
  instance
    PConstantDecl OracleDatum
instance PTryFrom PData (PAsData POracleDatum)
