{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Main api module tying things together.
module Lending.Api.Handler (apiServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Data.ByteString.Char8 qualified as C8
import Data.Yaml qualified as Yaml
import Servant qualified as S
import Servant.Api.Handler (getTlsSettings, runAppServer)
import System.Environment qualified as Environment

import Cardano.Api.Extra.NetworkId (NetworkIdText (NetworkIdText))
import Cardano.Api.Extra.NetworkParams qualified as CAE
import Cardano.Api.Extra.Node qualified as CAE
import Lending.Api.Config (loadContractConfigApi)
import Lending.Api.Env
  ( ApiConfig (ApiConfig, acCardanoNetworkId, acNetworkParamsFetchDelay, acTokenDecimalMap)
  , AppEnv
    ( AppEnv
    , aeContractsConfig
    , aeNetworkParams
    , aeNodeConnection
    , aeNormalConnectionPool
    , aeProtocalStateConnectionPool
    , aeTokenDecimalMap
    )
  , AppM
  )
import Lending.Api.Handler.Account (accountApi)
import Lending.Api.Handler.GlobalState (globalStateApi)
import Lending.Api.Handler.Liquidation (liquidationApi)
import Lending.Api.Handler.Manager (managerApi)
import Lending.Api.Handler.Oracle (oracleApi)
import Lending.Api.Handler.Pool (poolApi)
import Lending.Api.Handler.RiskDao (riskDaoApi)
import Lending.Api.Handler.SyncStatus (syncStatusApi)
import Lending.Api.Types (HealthcheckApi, LendingApi, LendingApiWithHealthcheck)
import Lending.Core.Api (handleError)
import Lending.Core.Utils (withDbPoolLending)

healthcheckServer :: S.ServerT HealthcheckApi AppM
healthcheckServer = pure S.NoContent

-- | All endpoints the api provides
lendingApiServer :: S.ServerT LendingApi AppM
lendingApiServer =
  managerApi
    S.:<|> accountApi
    S.:<|> oracleApi
    S.:<|> poolApi
    S.:<|> riskDaoApi
    S.:<|> globalStateApi
    S.:<|> syncStatusApi
    S.:<|> liquidationApi

-- | Read all env vars and run the 'AppM' action using constructed environment
apiServer :: IO ()
apiServer = do
  tls <- getTlsSettings

  dbConnection <- C8.pack <$> Environment.getEnv "DB_CONNECTION_STRING"
  cardanoNodeSocketPath <- Environment.getEnv "CARDANO_NODE_SOCKET_PATH"

  ApiConfig {acCardanoNetworkId = NetworkIdText cardanoNetworkId, acNetworkParamsFetchDelay, acTokenDecimalMap} <-
    Environment.getEnv "API_CONFIG_FILE" >>= Yaml.decodeFileThrow
  contractConfigs <- Environment.getEnv "SCRIPT_DIR" >>= loadContractConfigApi

  let nodeConnection = CAE.mkNodeConnectInfo cardanoNetworkId cardanoNodeSocketPath
  networkParamsRef <- liftIO $ CAE.fetchNetworkParamsPeriodically nodeConnection acNetworkParamsFetchDelay

  Logger.runStdoutLoggingT $
    withDbPoolLending dbConnection $ \normalPool ->
      withDbPoolLending dbConnection $ \protocolStatePool ->
        MonadIO.liftIO $
          runAppServer @LendingApiWithHealthcheck 3000 tls (healthcheckServer S.:<|> lendingApiServer) $ \runner ->
            handleError $
              Logger.runStdoutLoggingT $
                Reader.runReaderT
                  runner
                  AppEnv
                    { aeNormalConnectionPool = normalPool
                    , aeProtocalStateConnectionPool = protocolStatePool
                    , aeNodeConnection = nodeConnection
                    , aeNetworkParams = networkParamsRef
                    , aeContractsConfig = contractConfigs
                    , aeTokenDecimalMap = acTokenDecimalMap
                    }
