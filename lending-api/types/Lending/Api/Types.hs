{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lending.Api.Types (LendingApiWithHealthcheck, HealthcheckApi, LendingApi, LendingApiInner) where

import Servant.API (Get, JSON, NoContent, (:<|>), (:>))

import Lending.Api.Types.Account (AccountApi)
import Lending.Api.Types.GlobalState (GlobalStateApi)
import Lending.Api.Types.Liquidation (LiquidationApi)
import Lending.Api.Types.Manager (ManagerApi)
import Lending.Api.Types.Oracle (OracleApi)
import Lending.Api.Types.Pool (PoolApi)
import Lending.Api.Types.RiskDao (RiskDaoApi)
import Lending.Api.Types.SyncStatus (SyncStatusApi)

type LendingApiWithHealthcheck = HealthcheckApi :<|> LendingApi

type HealthcheckApi = Get '[JSON] NoContent

type LendingApi = "lending" :> "v1" :> LendingApiInner

type LendingApiInner =
  ManagerApi
    :<|> AccountApi
    :<|> OracleApi
    :<|> PoolApi
    :<|> RiskDaoApi
    :<|> GlobalStateApi
    :<|> SyncStatusApi
    :<|> LiquidationApi
