{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Contracts.Pool.Types where

import Plutarch.Api.V1 (PPOSIXTime)
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
import Ply.Plutarch (PlyArgOf)

import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Exchange (PCumulativeRate, PReceipt)
import Lending.Contracts.Orphans ()
import Lending.Types.Pool (AssetInformation, PoolDatum, PoolRedeemer)

newtype PAssetInformation (s :: S)
  = PAssetInformation
      ( Term
          s
          ( PDataRecord
              '[ "aiSupplyAmount" ':= PReceipt
               , "aiBorrowAmount" ':= PReceipt
               , "aiCumulatedInterestRateSupplying" ':= PCumulativeRate
               , "aiCumulatedInterestRateBorrowing" ':= PCumulativeRate
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields, PShow)

instance DerivePlutusType PAssetInformation where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PAssetInformation)
instance PUnsafeLiftDecl PAssetInformation where type PLifted PAssetInformation = AssetInformation
deriving via (DerivePConstantViaData AssetInformation PAssetInformation) instance PConstantDecl AssetInformation
type instance PlyArgOf PAssetInformation = AssetInformation

data PPoolRedeemer (s :: S)
  = PUpdatePoolRedeemer (Term s (PDataRecord '[]))
  | PMigratePoolRedeemer (Term s (PDataRecord '[]))
  | PUpdateTreasuryPoolRedeemer (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)
instance DerivePlutusType PPoolRedeemer where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PPoolRedeemer where type PLifted PPoolRedeemer = PoolRedeemer
deriving via (DerivePConstantViaData PoolRedeemer PPoolRedeemer) instance PConstantDecl PoolRedeemer
instance PTryFrom PData (PAsData PPoolRedeemer)

newtype PPoolDatum (s :: S)
  = PPoolDatum
      ( Term
          s
          ( PDataRecord
              '[ "pdAssets" ':= PMap 'Sorted PAsset PAssetInformation
               , "pdLastUpdatedTime" ':= PPOSIXTime
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)
instance DerivePlutusType PPoolDatum where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PPoolDatum)
instance PUnsafeLiftDecl PPoolDatum where type PLifted PPoolDatum = PoolDatum
deriving via (DerivePConstantViaData PoolDatum PPoolDatum) instance PConstantDecl PoolDatum
