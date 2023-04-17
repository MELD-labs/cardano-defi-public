{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Lending.Api.Types.SyncStatus (SyncStatusApi, SyncStatusResponse (..), AmountAsset (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Servant.API (Get, JSON, Summary, type (:>))

import Cardano.Index.ChainPoint.Model (ChainPoint)
import Data.Map (Map)
import Lending.Api.Types.Aeson (DefaultApiJson (DefaultApiJson), HasAesonOptions)
import Lending.Api.Types.Orphans ()
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Receipt)
import Lending.Types.Orphans ()
import Plutarch.Extra.FixedDecimal (FixedDecimal)

-- | The API to query current sync status of lending-index.
type SyncStatusApi =
  "sync-status"
    :> Summary "Query current sync status of lending-index"
    :> Get '[JSON] SyncStatusResponse

data SyncStatusResponse = SyncStatusResponse
  { ssrChainPoint :: ChainPoint
  , ssrSyncProgress :: FixedDecimal 4
  , ssrAmountInProtocol :: Map Asset AmountAsset
  , ssrAssetAnomaly :: [Asset]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson SyncStatusResponse

data AmountAsset = AmountAsset
  { aaSupplyAmountAccount :: Receipt
  , aaBorrowAmountAccount :: Receipt
  , aaSupplyPool :: Receipt
  , aaBorrowPool :: Receipt
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson AmountAsset
