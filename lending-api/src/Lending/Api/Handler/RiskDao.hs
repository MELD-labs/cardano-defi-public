{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Lending.Api.Handler.RiskDao (riskDaoApi) where

import Control.Monad.Trans.Reader qualified as Reader
import Servant (ServerT)

import Lending.Api.Common (getCurrentTime)
import Lending.Api.Config (ContractsConfigApi (ccaLendingFunctionInterestRate))
import Lending.Api.Env (AppEnv (aeContractsConfig), AppM, aeProtocalStateConnectionPool, runSqlM)
import Lending.Api.Handler.Account (calculateAccountState)
import Lending.Api.Handler.GlobalState (getGlobalState)
import Lending.Api.Types.RiskDao
  ( QueryRiskDaoResponse
      ( QueryRiskDaoResponse
      )
  , RiskDaoApi
  )
import Lending.Core.AccountValue (ExtractedAccount (eaAccountUtxo), fillNewAssets, getDeltaTime, tryUpdateInterestRate)
import Lending.Index.Query.Account (getAllAccountUtxos)
import Lending.Index.Query.Manager (getManagerUtxo)
import Lending.Index.Query.Oracle (getOracleUtxo)
import Lending.Index.Query.Pool (getPoolUtxo)
import Lending.Types.Manager (ManagerDatum (ManagerDatum, mdRiskParameters))
import Lending.Types.Oracle (OracleDatum (OracleDatum, odAssetPrices))
import Lending.Types.Pool (PoolDatum (PoolDatum, pdAssets, pdLastUpdatedTime))
import TxBuilder.Api (UtxoInputWithDatum (UtxoInputWithDatum, uiwdDatum))

riskDaoApi :: ServerT RiskDaoApi AppM
riskDaoApi = queryRiskDaoH

data ProtocolUtxos = ProtocolUtxos
  { puManager :: UtxoInputWithDatum ManagerDatum
  , puPool :: UtxoInputWithDatum PoolDatum
  , puAccounts :: [ExtractedAccount]
  , puOracle :: UtxoInputWithDatum OracleDatum
  }

queryRiskDaoH :: AppM QueryRiskDaoResponse
queryRiskDaoH = do
  ProtocolUtxos
    { puManager = UtxoInputWithDatum {uiwdDatum = managerDatum}
    , puPool =
      currentPool@UtxoInputWithDatum
        { uiwdDatum = PoolDatum {pdAssets = currentAssetMap, pdLastUpdatedTime}
        }
    , puAccounts
    , puOracle = UtxoInputWithDatum {uiwdDatum = oracleDatum}
    } <-
    runSqlM
      (ProtocolUtxos <$> getManagerUtxo <*> getPoolUtxo <*> getAllAccountUtxos <*> getOracleUtxo)
      aeProtocalStateConnectionPool
  functionInterestRate <- Reader.asks (ccaLendingFunctionInterestRate . aeContractsConfig)
  curTime <- getCurrentTime
  let OracleDatum {odAssetPrices} = oracleDatum
      ManagerDatum {mdRiskParameters} = managerDatum
      dTime = getDeltaTime pdLastUpdatedTime curTime
  updatedAssetMap <-
    fillNewAssets (uiwdDatum . eaAccountUtxo <$> puAccounts)
      <$> tryUpdateInterestRate functionInterestRate mdRiskParameters currentAssetMap dTime
  accountStateList <-
    traverse
      (calculateAccountState managerDatum currentAssetMap updatedAssetMap dTime odAssetPrices)
      puAccounts
  globalState <- getGlobalState managerDatum currentPool oracleDatum
  pure $ QueryRiskDaoResponse globalState accountStateList curTime
