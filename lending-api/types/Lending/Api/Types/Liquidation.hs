{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Lending.Api.Types.Liquidation
  ( LiquidationApi
  , QueryLiquidationApi
  , BatchingInfo (..)
  , QueryLiquidationResponse (..)
  , LiquidationData (..)
  )
where

import Cardano.Api qualified as CA
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (Get, JSON, QueryParams, Summary, type (:>))

import Cardano.Index.Data.AddressText (AddressText)
import Lending.Api.Types.Aeson (DefaultApiJson (DefaultApiJson), HasAesonOptions)
import Lending.Api.Types.Orphans ()
import Lending.Types.Account (AccountDatum)

data BatchingInfo = BatchingInfo
  { biTxId :: Text
  , biTimestamp :: CA.SlotNo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson BatchingInfo

data LiquidationData = LiquidationData
  { ldTxId :: Text
  , ldLiquidatorAddress :: AddressText CA.AddressAny
  , ldTimestamp :: CA.SlotNo
  , ldBatchingInfo :: Maybe BatchingInfo
  , ldLiquidatedAccount :: AccountDatum
  , ldContinuingAccount :: AccountDatum
  , ldLiquidatorAccount :: AccountDatum
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson LiquidationData

newtype QueryLiquidationResponse = QueryLiquidationResponse
  { qlLiquidations :: [LiquidationData]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson QueryLiquidationResponse

type LiquidationApi = "liquidation" :> QueryLiquidationApi

type QueryLiquidationApi =
  Summary "Query liquidation information"
    :> QueryParams "liquidatedAccount" CA.TxIn
    :> Get '[JSON] QueryLiquidationResponse
