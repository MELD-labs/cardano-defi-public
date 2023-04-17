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
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lending.Types.Account
  ( AccountDatum (..)
  , AccountRedeemer (..)
  , Request (..)
  , AccountTokenRedeemer (..)
  , AccountLiquidateRedeemerData (..)
  , ClearRequest (..)
  )
where

import Data.Aeson (FromJSON, Options (sumEncoding), SumEncoding (ObjectWithSingleField), ToJSON)
import Data.Aeson.TH qualified as Aeson
import Data.Map (Map)
import GHC.Generics (Generic)
import Plutarch.Extra.AssetClass (AssetClass)
import Plutarch.Extra.Tagged ()
import PlutusLedgerApi.V2 (Address, Data, toData)
import PlutusTx qualified
import Ply.Core.Class (PlyArg (UPLCRep, toBuiltinArg, toBuiltinArgData))

import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, Receipt)
import Lending.Types.Orphans ()
import Plutarch.Orphans ()

data ClearRequest
  = ClearSupplying
      { cdReceiver :: Address
      }
  | ClearBorrowing
      { cbLimited :: Actual
      , cbReceiver :: Address
      }
  deriving stock (Generic, Eq, Show)

data Request
  = SupplyRequest
      { srAsset :: Asset
      , srAmount :: Actual
      }
  | WithdrawRequest
      { wrAsset :: Asset
      , wrAmount :: Actual
      , wrReceiver :: Address
      }
  | BorrowRequest
      { brAsset :: Asset
      , brAmount :: Actual
      , brReceiver :: Address
      }
  | RepayRequest
      { rrAsset :: Asset
      , rrAmount :: Actual
      }
  deriving stock (Generic, Eq, Show)

$(Aeson.deriveJSON Aeson.defaultOptions {sumEncoding = ObjectWithSingleField} ''Request)
$(Aeson.deriveJSON Aeson.defaultOptions {sumEncoding = ObjectWithSingleField} ''ClearRequest)

data AccountDatum = AccountDatum
  { adSupplies :: Map Asset Receipt
  , adBorrowings :: Map Asset Receipt
  , adCollateralAssets :: Map Asset Bool
  , adUserNft :: AssetClass
  , adNormalRequests :: [Request]
  , adCollateralUpdate :: Maybe (Map Asset Bool)
  , adProtocolIncentive :: Maybe (Map Asset Receipt)
  , adClearRequests :: Map Asset ClearRequest
  , adExtraLovelace :: Integer
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

data AccountLiquidateRedeemerData = AccountLiquidateRedeemerData
  { alrBorrowings :: Map Asset Actual
  , alrCollaterals :: Map Asset Actual
  , alrClearRequests :: Map Asset ClearRequest
  , alrExtraLovelace :: Integer
  , alrUserNft :: AssetClass
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

data AccountRedeemer
  = AccountUpdateRedeemer
  | AccountApplyRedeemer
  | AccountCloseRedeemer
  | AccountLiquidateRedeemer AccountLiquidateRedeemerData
  | AccountMigrateRedeemer
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

data AccountTokenRedeemer = Mint AssetClass | Burn | Migrate
  deriving stock (Generic)

PlutusTx.makeIsDataIndexed
  ''ClearRequest
  [('ClearSupplying, 0), ('ClearBorrowing, 1)]
PlutusTx.makeIsDataIndexed
  ''Request
  [('SupplyRequest, 0), ('WithdrawRequest, 1), ('BorrowRequest, 2), ('RepayRequest, 3)]
PlutusTx.makeIsDataIndexed ''AccountDatum [('AccountDatum, 0)]
PlutusTx.makeIsDataIndexed ''AccountLiquidateRedeemerData [('AccountLiquidateRedeemerData, 0)]
PlutusTx.makeIsDataIndexed
  ''AccountRedeemer
  [ ('AccountUpdateRedeemer, 0)
  , ('AccountApplyRedeemer, 1)
  , ('AccountCloseRedeemer, 2)
  , ('AccountLiquidateRedeemer, 3)
  , ('AccountMigrateRedeemer, 4)
  ]
PlutusTx.makeIsDataIndexed ''AccountTokenRedeemer [('Mint, 0), ('Burn, 1), ('Migrate, 2)]

instance PlyArg AccountDatum where
  type UPLCRep AccountDatum = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData

instance PlyArg AccountLiquidateRedeemerData where
  type UPLCRep AccountLiquidateRedeemerData = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData

instance PlyArg Request where
  type UPLCRep Request = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData

instance PlyArg ClearRequest where
  type UPLCRep ClearRequest = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData
