{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

-- | The module defining types for /pool/ API endpoint.
module Lending.Api.Types.Pool
  ( PoolApi
  , QueryPoolApi
  , UpdateTreasuryPoolApi
  , QueryPoolResponse (..)
  , UpdateTreasuryPoolRequest (..)
  , UpdateTreasuryPoolResponse (..)
  , MigratePoolApi
  , MigratePoolInputResponse (..)
  , MigratePoolInputApi
  , MigratePoolInput (..)
  )
where

import Cardano.Api qualified as CA
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Servant.API (Get, JSON, Post, ReqBody, Summary, type (:<|>), type (:>))

import Cardano.Index.Data.AddressText (AddressText)
import Lending.Api.Types.Aeson (DefaultApiJson (DefaultApiJson), HasAesonOptions)
import Lending.Api.Types.Orphans ()
import Lending.Core.JsonViaTextEnvelope (WrapAsTx (WrapAsTx))
import Lending.Types (PoolDatum)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual)
import TxBuilder.Api.Types (UtxoInput)

-- | Response body for Query Pool API.
newtype QueryPoolResponse
  = -- | The status of pool
    -- containing lending amount, borrowing amount and cumulative interest rate of each asset.
    QueryPoolResponse {qprPoolDatum :: PoolDatum}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson QueryPoolResponse

data UpdateTreasuryPoolRequest = UpdateTreasuryPoolRequest
  { utprUpdateRequest :: Map Asset Actual
  -- ^ The request for updating the treasury pool. Amount asset is negative, which means withdraw treasury.
  , utprNewStakingCredential :: Maybe CA.StakeAddress
  -- ^ New stake address if operator want to change stake key
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson UpdateTreasuryPoolRequest

newtype UpdateTreasuryPoolResponse = UpdateTreasuryPoolResponse {utprTx :: CA.TxBody CA.BabbageEra}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via WrapAsTx

data MigratePoolInput = MigratePoolInput
  { mpiPoolInput :: UtxoInput
  , mpiPoolScriptRefUtxo :: UtxoInput
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson MigratePoolInput

data MigratePoolInputResponse = MigratePoolInputResponse
  { mpirMigratePoolInput :: MigratePoolInput
  , mpirOldPoolDatum :: PoolDatum
  , mpirOldPoolValue :: CA.Value
  , mpirOperatorAddress :: AddressText (CA.AddressInEra CA.BabbageEra)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson MigratePoolInputResponse

type PoolApi =
  "pool"
    :> (QueryPoolApi :<|> UpdateTreasuryPoolApi :<|> MigratePoolApi)

-- | Returns the current status of pool.
type QueryPoolApi =
  Summary "Query current status of the pool"
    :> Get '[JSON] QueryPoolResponse

-- | Build transaction to update the treasury amount from Pool.
type UpdateTreasuryPoolApi =
  "update-treasury"
    :> Summary "Build transaction to update the treasury amount from Pool"
    :> ReqBody '[JSON] UpdateTreasuryPoolRequest
    :> Post '[JSON] UpdateTreasuryPoolResponse

-- | API to query old pool utxo.
type MigratePoolApi =
  "migrate"
    :> MigratePoolInputApi

-- | Query current pool to migrate pool.
type MigratePoolInputApi =
  "input"
    :> Summary "Query current pool to spend as input of migration tx."
    :> Get '[JSON] MigratePoolInputResponse
