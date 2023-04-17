{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

-- | The module defining types for /risk-dao/ API endpoint.
module Lending.Api.Types.RiskDao
  ( RiskDaoApi
  , QueryRiskDaoResponse (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import PlutusLedgerApi.V2 (POSIXTime)
import Servant.API (Get, JSON, Summary, type (:>))

import Lending.Api.Types.Account (AccountState)
import Lending.Api.Types.Aeson (DefaultApiJson (DefaultApiJson), HasAesonOptions)
import Lending.Api.Types.GlobalState (GlobalState)
import Lending.Api.Types.Orphans ()

-- | The API to query json for RiskDAO.
type RiskDaoApi =
  "risk-dao"
    :> Summary "Query JSON for RiskDAO"
    :> Get '[JSON] QueryRiskDaoResponse

-- | Response body for RiskDao API.
data QueryRiskDaoResponse = QueryRiskDaoResponse
  { qrdGlobalState :: GlobalState
  , qrdAccounts :: [AccountState]
  , qrdTimestamp :: POSIXTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson QueryRiskDaoResponse
