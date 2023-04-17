{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Lending.Faucet.Types
  ( MintTokenRequest (..)
  , MintTokenResponse (..)
  , MintTokenApi
  , HealthcheckApi
  , FaucetApi
  , FaucetApiInner
  , FaucetApiWithHealthcheck
  , FaucetStatusApi
  , PrepareFaucetApi
  , FaucetStatusResponse (..)
  , PrepareFaucetRequest (..)
  , PrepareFaucetResponse (..)
  )
where

import Cardano.Api qualified as CA
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Servant.API (Get, JSON, NoContent, Post, ReqBody, Summary, (:>), type (:<|>))

import Cardano.Index.Data.AddressText (AddressText)
import Lending.Api.Types.Aeson (DefaultApiJson (DefaultApiJson), HasAesonOptions)
import Lending.Api.Types.Orphans ()

type FaucetApiWithHealthcheck = HealthcheckApi :<|> FaucetApi

type HealthcheckApi = Get '[JSON] NoContent

type FaucetApiInner = MintTokenApi :<|> FaucetStatusApi :<|> PrepareFaucetApi

type FaucetApi = "faucet" :> "v1" :> FaucetApiInner

type MintTokenApi =
  "mint"
    :> Summary "Mint test tokens and NFTs and send to user"
    :> ReqBody '[JSON] MintTokenRequest
    :> Post '[JSON] MintTokenResponse

type FaucetStatusApi =
  "status"
    :> Summary "Query status for Faucet"
    :> Get '[JSON] FaucetStatusResponse

type PrepareFaucetApi =
  "prepare"
    :> Summary "Prepare more packets for Faucet"
    :> ReqBody '[JSON] PrepareFaucetRequest
    :> Post '[JSON] PrepareFaucetResponse

newtype MintTokenRequest = MintTokenRequest
  { mtUserAddress :: AddressText (CA.AddressInEra CA.BabbageEra)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MintTokenResponse = MintTokenResponse {mtTxId :: CA.TxId}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FaucetStatusResponse = FaucetStatusResponse {fsrMVarUtxoCount :: Int, fsrMVarUtxos :: [CA.TxIn]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson FaucetStatusResponse

data PrepareFaucetRequest = PrepareFaucetRequest
  { pfrFirstLayer :: Integer
  , pfrSecondLayer :: Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson PrepareFaucetRequest

data PrepareFaucetResponse = PrepareFaucetResponse
  { pfrFirstLayerTxId :: CA.TxId
  , pfrSecondLayerTxIds :: [CA.TxId]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson PrepareFaucetResponse
