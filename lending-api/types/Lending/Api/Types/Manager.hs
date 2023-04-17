{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

-- | The module defining types for /admin/ Api endpoint.
module Lending.Api.Types.Manager
  ( ManagerApi
  , QueryManagerApi
  , UpdateManagerApi
  , MigrateManagerApi
  , QueryManagerResponse (..)
  , UpdateManagerResponse (..)
  , MigrateManagerInputResponse (..)
  )
where

import Cardano.Api qualified as CA
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Servant.API (Get, JSON, Post, ReqBody, Summary, type (:<|>), type (:>))

import Lending.Api.Types.Aeson (DefaultApiJson (DefaultApiJson), HasAesonOptions)
import Lending.Api.Types.Orphans ()
import Lending.Core.JsonViaTextEnvelope (WrapAsTx (WrapAsTx))
import Lending.Types.Manager (ManagerDatum)
import TxBuilder.Api (UtxoInput)

-- | Response body for Manager Api.
newtype UpdateManagerResponse
  = -- | The balanced transaction body.
    UpdateManagerResponse {umTx :: CA.TxBody CA.BabbageEra}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via WrapAsTx

-- | Response body for Manager Api.
newtype QueryManagerResponse
  = -- | The Manager datum containing protocol params.
    QueryManagerResponse {mrManagerDatum :: ManagerDatum}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson QueryManagerResponse

data MigrateManagerInputResponse = MigrateManagerInputResponse
  { mmirOldManagerDatum :: ManagerDatum
  , mmirOldManagerValue :: CA.Value
  , mmirManagerUtxo :: UtxoInput
  , mmirManagerScriptRefUtxo :: UtxoInput
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson MigrateManagerInputResponse

-- | The Api to update Manager datum.
type ManagerApi = "manager" :> (UpdateManagerApi :<|> QueryManagerApi :<|> MigrateManagerApi)

type UpdateManagerApi =
  Summary "Update protocol params stored in Manager UTXO"
    :> ReqBody '[JSON] ManagerDatum
    :> Post '[JSON] UpdateManagerResponse

type MigrateManagerApi =
  "migrate"
    :> Summary "Migrate asset class and address stored in Manager UTXO"
    :> ReqBody '[JSON] ManagerDatum
    :> Post '[JSON] UpdateManagerResponse

type QueryManagerApi =
  Summary "Query current protocol params stored in Manager UTXO"
    :> Get '[JSON] QueryManagerResponse
