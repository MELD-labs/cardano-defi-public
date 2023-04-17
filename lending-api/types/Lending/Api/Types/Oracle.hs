{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

-- | Module defining types for for /oracle/ API endpoint.
module Lending.Api.Types.Oracle
  ( OracleApi
  , UpdateOracleApi
  , UpdateOracleRequest (..)
  , UpdateOracleResponse (..)
  )
where

import Cardano.Api qualified as CA
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Servant.API (JSON, Post, ReqBody, Summary, type (:>))

import Lending.Api.Types.Aeson (DefaultApiJson (DefaultApiJson), HasAesonOptions)
import Lending.Api.Types.Orphans ()
import Lending.Core.JsonViaTextEnvelope (WrapAsTx (WrapAsTx))
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)

-- | Request body for Updating Oracle API.
newtype UpdateOracleRequest = UpdateOracleRequest
  { uorTokenPrices :: Map Asset Price
  -- ^ Map of asset class and its price.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson UpdateOracleRequest

-- | Repsonse body for Updating Oracle API.
newtype UpdateOracleResponse
  = -- | The balanced transaction body.
    UpdateOracleResponse {uorTx :: CA.TxBody CA.BabbageEra}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via WrapAsTx

-- | Query current price of assets stored in Oracle UTXO.
type OracleApi =
  "oracle"
    :> Summary "Query current price of assets stored in Oracle UTXO"
    :> UpdateOracleApi

-- | The API to update price of assets.
type UpdateOracleApi =
  Summary "Update price of assets"
    :> ReqBody '[JSON] UpdateOracleRequest
    :> Post '[JSON] UpdateOracleResponse
