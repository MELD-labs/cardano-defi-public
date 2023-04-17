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

module Lending.Types.Pool
  ( AssetInformation (..)
  , PoolDatum (..)
  , PoolRedeemer (..)
  , CumulativeRate
  , PoolScriptParams (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import GHC.Generics (Generic)
import Plutarch.Extra.AssetClass (AssetClass)
import Plutarch.Extra.Tagged ()
import PlutusTx qualified

import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (CumulativeRate, Receipt)
import Lending.Types.Orphans ()
import PlutusLedgerApi.V2 (Data, POSIXTime, toData)
import Ply.Core.Class (PlyArg (UPLCRep, toBuiltinArg, toBuiltinArgData))

data PoolScriptParams = PoolScriptParams
  { pspOperatorPoolNft :: AssetClass
  , pspManagerNft :: AssetClass
  , pspMigrateNft :: AssetClass
  }

data AssetInformation = AssetInformation
  { aiSupplyAmount :: Receipt
  , aiBorrowAmount :: Receipt
  , aiCumulatedInterestRateSupplying :: CumulativeRate
  , aiCumulatedInterestRateBorrowing :: CumulativeRate
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

data PoolDatum = PoolDatum
  { pdAssets :: Map Asset AssetInformation
  , pdLastUpdatedTime :: POSIXTime
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

data PoolRedeemer = UpdatePoolRedeemer | MigratePoolRedeemer | UpdateTreasuryPoolRedeemer
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''AssetInformation [('AssetInformation, 0)]
PlutusTx.makeIsDataIndexed
  ''PoolRedeemer
  [('UpdatePoolRedeemer, 0), ('MigratePoolRedeemer, 1), ('UpdateTreasuryPoolRedeemer, 2)]
PlutusTx.makeIsDataIndexed ''PoolDatum [('PoolDatum, 0)]

instance PlyArg AssetInformation where
  type UPLCRep AssetInformation = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData
