{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

-- | The module defining types for /global-state/ API endpoint.
module Lending.Api.Types.GlobalState
  ( GlobalStateApi
  , GlobalState (..)
  , AssetState (..)
  , QueryGlobalStateResponse (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Servant.API (Get, JSON, Summary, type (:>))

import Cardano.Api.Extra.AssetId (AssetIdText)
import Lending.Api.Types.Aeson (DefaultApiJson (DefaultApiJson), HasAesonOptions)
import Lending.Api.Types.Orphans ()
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, CumulativeRate, Decimal, Fiat, Price)
import Lending.Types.Manager (GlobalRiskParameters, RiskParameters)
import Plutarch.Extra.FixedDecimal (FixedDecimal)

data AssetState = AssetState
  { asRiskParameters :: RiskParameters
  -- ^ Risk parameter for each asset.
  , asPrice :: Price
  -- ^ Price of asset
  , asTotalSupply :: Actual
  -- ^ Total supply.
  , asTotalBorrow :: Actual
  -- ^ Total borrow.
  , asTotalSuppliedValue :: Fiat
  -- ^ Total supply.
  , asTotalBorrowedValue :: Fiat
  -- ^ Total borrow.
  , asUtilization :: Decimal
  -- ^ Current utilization
  , asSupplyApy :: CumulativeRate
  -- ^ Current supply APY
  , asBorrowApy :: CumulativeRate
  -- ^ Current borrow APY
  , asDisplayName :: String
  -- ^ The display name of asset
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson AssetState

-- | Current global state.
data GlobalState = GlobalState
  { gsAssets :: Map Asset AssetState
  , gsAccountAuthToken :: AssetIdText
  , gsPoolNft :: AssetIdText
  , gsOracleCheckerToken :: AssetIdText
  , gsGlobalRiskParameters :: GlobalRiskParameters
  , gsTreasuryValue :: Map Asset Actual
  , gsTreasuryWithdrawableValue :: Map Asset Actual
  , gsAverageSupplyApy :: FixedDecimal 4
  , gsAverageBorrowApy :: FixedDecimal 4
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson GlobalState

newtype QueryGlobalStateResponse = QueryGlobalStateResponse
  { qgsGlobalState :: GlobalState
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson QueryGlobalStateResponse

-- | The API to query json for global state.
type GlobalStateApi =
  "global-state"
    :> Summary "Query JSON for global state"
    :> Get '[JSON] QueryGlobalStateResponse
