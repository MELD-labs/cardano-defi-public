{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Contracts.Manager.Types where

import Plutarch.Api.V1.AssocMap (KeyGuarantees (Sorted), PMap)
import Plutarch.Api.V2 (PAddress, PPOSIXTime)
import Plutarch.DataRepr
  ( DerivePConstantViaData (DerivePConstantViaData)
  , PDataFields
  )
import Plutarch.Extra.AssetClass (PAssetClassData)
import Plutarch.Lift
  ( PConstantDecl
  , PLifted
  , PUnsafeLiftDecl
  )
import Ply.Plutarch (PlyArgOf)

import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Exchange (PActual, PCumulativeRate, PDecimal, PFiat, PLtvRatio)
import Lending.Contracts.Orphans ()
import Lending.Types.Manager
  ( GlobalRiskParameters
  , ManagerDatum
  , ManagerRedeemer
  , RiskParameters
  )

newtype PRiskParameters (s :: S)
  = PRiskParameters
      ( Term
          s
          ( PDataRecord
              '[ "rpMaxLoanToValue" ':= PLtvRatio
               , "rpLiquidationThreshold" ':= PLtvRatio
               , "rpBorrowCap" ':= PActual
               , "rpSupplyCap" ':= PActual
               , "rpAssetClassData" ':= PAssetClassData
               , "rpReserveFactor" ':= PDecimal
               , "rpTargetUtilizationRate" ':= PDecimal
               , "rpBaseBorrowingRate" ':= PCumulativeRate
               , "rpInterestRateSlope1" ':= PDecimal
               , "rpInterestRateSlope2" ':= PDecimal
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)
instance DerivePlutusType PRiskParameters where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PRiskParameters where type PLifted PRiskParameters = RiskParameters
deriving via
  (DerivePConstantViaData RiskParameters PRiskParameters)
  instance
    PConstantDecl RiskParameters
instance PTryFrom PData (PAsData PRiskParameters)
type instance PlyArgOf PRiskParameters = RiskParameters

newtype PGlobalRiskParameters (s :: S)
  = PGlobalRiskParameters
      ( Term
          s
          ( PDataRecord
              '[ "grpCloseFactor" ':= PDecimal
               , "grpMaxLiquidationCloseFactor" ':= PDecimal
               , "grpCloseFactorHealthFactorThreshold" ':= PDecimal
               , "grpCloseFactorMinCollateralThreshold" ':= PFiat
               , "grpLiquidatorIncentive" ':= PDecimal
               , "grpProtocolIncentive" ':= PDecimal
               , "grpMinAdaUtxo" ':= PInteger
               , "grpBatcherFee" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)
instance DerivePlutusType PGlobalRiskParameters where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PGlobalRiskParameters where type PLifted PGlobalRiskParameters = GlobalRiskParameters
deriving via
  (DerivePConstantViaData GlobalRiskParameters PGlobalRiskParameters)
  instance
    PConstantDecl GlobalRiskParameters
instance PTryFrom PData PGlobalRiskParameters
type instance PlyArgOf PGlobalRiskParameters = GlobalRiskParameters

-- TODO: verify decimal number of close factor and health factor.
newtype PManagerDatum (s :: S)
  = PManagerDatum
      ( Term
          s
          ( PDataRecord
              '[ "mdAccountAuthToken" ':= PAssetClassData
               , "mdRiskParameters" ':= PMap 'Sorted PAsset PRiskParameters
               , "mdPoolNft" ':= PAssetClassData
               , "mdTreasuryOperatorNft" ':= PAssetClassData
               , "mdRiskParamsOperatorNft" ':= PAssetClassData
               , "mdAccountAddress" ':= PAddress
               , "mdMaxValidityDuration" ':= PPOSIXTime
               , "mdOracleCheckerToken" ':= PAssetClassData
               , "mdGlobalRiskParameters" ':= PGlobalRiskParameters
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)
instance DerivePlutusType PManagerDatum where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PManagerDatum where type PLifted PManagerDatum = ManagerDatum
deriving via
  (DerivePConstantViaData ManagerDatum PManagerDatum)
  instance
    PConstantDecl ManagerDatum
instance PTryFrom PData (PAsData PManagerDatum)
type instance PlyArgOf PManagerDatum = ManagerDatum

newtype PManagerRedeemer (s :: S) = PUpdateManagerRedeemer (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)
instance DerivePlutusType PManagerRedeemer where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PManagerRedeemer where type PLifted PManagerRedeemer = ManagerRedeemer
deriving via
  (DerivePConstantViaData ManagerRedeemer PManagerRedeemer)
  instance
    PConstantDecl ManagerRedeemer
