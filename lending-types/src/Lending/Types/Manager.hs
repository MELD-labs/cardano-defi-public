{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Types.Manager
  ( ManagerScriptParams (..)
  , ManagerDatum (..)
  , ManagerRedeemer (..)
  , RiskParameters (..)
  , GlobalRiskParameters (..)
  )
where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Plutarch.Extra.AssetClass (AssetClass)
import PlutusLedgerApi.V2 (Address, Data, POSIXTime, toData)
import PlutusTx qualified

import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, CumulativeRate, Decimal, Fiat, LtvRatio)
import Lending.Types.Orphans ()
import Plutarch.Extra.Tagged ()
import Ply.Core.Class (PlyArg (UPLCRep, toBuiltinArg, toBuiltinArgData))

data ManagerScriptParams = ManagerScriptParams
  { mspMigrationOperatorNFT :: AssetClass
  , mspManagerAuthToken :: AssetClass
  }

data RiskParameters = RiskParameters
  { rpMaxLoanToValue :: LtvRatio
  , rpLiquidationThreshold :: LtvRatio
  , rpBorrowCap :: Actual
  , rpSupplyCap :: Actual
  , rpAssetClassData :: AssetClass
  , rpReserveFactor :: Decimal
  , rpTargetUtilizationRate :: Decimal
  , rpBaseBorrowingRate :: CumulativeRate
  , rpInterestRateSlope1 :: Decimal
  , rpInterestRateSlope2 :: Decimal
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

data GlobalRiskParameters = GlobalRiskParameters
  { grpCloseFactor :: Decimal
  , grpMaxLiquidationCloseFactor :: Decimal
  , grpCloseFactorHealthFactorThreshold :: Decimal
  , grpCloseFactorMinCollateralThreshold :: Fiat
  , grpLiquidatorIncentive :: Decimal
  , grpProtocolIncentive :: Decimal
  , grpMinAdaUtxo :: Integer
  , grpBatcherFee :: Integer
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

data ManagerDatum = ManagerDatum
  { mdAccountAuthToken :: AssetClass
  , mdRiskParameters :: Map Asset RiskParameters
  , mdPoolNft :: AssetClass
  , mdTreasuryOperatorNft :: AssetClass
  , mdRiskParamsOperatorNft :: AssetClass
  , mdAccountAddress :: Address
  , mdMaxValidityDuration :: POSIXTime
  , mdOracleCheckerToken :: AssetClass
  , mdGlobalRiskParameters :: GlobalRiskParameters
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- PlutusTx.makeLift ''ManagerDatum

data ManagerRedeemer = UpdateParamsRedeemer
  deriving stock (Show, Generic)

PlutusTx.makeIsDataIndexed ''RiskParameters [('RiskParameters, 0)]
PlutusTx.makeIsDataIndexed ''GlobalRiskParameters [('GlobalRiskParameters, 0)]
PlutusTx.makeIsDataIndexed ''ManagerDatum [('ManagerDatum, 0)]
PlutusTx.makeIsDataIndexed ''ManagerRedeemer [('UpdateParamsRedeemer, 0)]

instance PlyArg RiskParameters where
  type UPLCRep RiskParameters = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData

instance PlyArg GlobalRiskParameters where
  type UPLCRep GlobalRiskParameters = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData

instance PlyArg ManagerDatum where
  type UPLCRep ManagerDatum = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData
