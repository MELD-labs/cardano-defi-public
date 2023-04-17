{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader qualified as Reader
import Data.ByteString.Char8 qualified as C8
import Data.Yaml qualified as Yaml
import Network.HTTP.Client qualified as HttpClient
import Servant.Client qualified as Servant
import System.Environment qualified as Environment

import Cardano.Api.Extra.Key (readPaymentSigningKey)
import Cardano.Api.Extra.NetworkId (NetworkIdText (unNetworkIdText))
import Cardano.Api.Extra.NetworkParams qualified as CAE
import Cardano.Api.Extra.Node qualified as CAE
import Lending.Core.Utils (withDbPoolLending)
import Lending.Migration.Account (migrateAccounts)
import Lending.Migration.Env
  ( MigrationConfig
      ( MigrationConfig
      , apiClientEnv
      , dbConnection
      , migrationOperatorSigningKey
      , networkParams
      , nodeConnection
      , numberAccountsPerBatch
      , scriptDir
      )
  , MigrationFileConfig (networkId, numberAccounts)
  )
import Lending.Migration.Manager (migrateManager)
import Service.Runner (runApp)

run :: IO ()
run = do
  migrationConfigFilePath <- Environment.getEnv "MIGRATION_CONFIG_FILE"
  scriptDirPath <- Environment.getEnv "SCRIPT_DIR"
  migrationConfigFile <- Yaml.decodeFileThrow migrationConfigFilePath
  nodeConnectionInfo <-
    CAE.mkNodeConnectInfo (unNetworkIdText $ networkId migrationConfigFile)
      <$> Environment.getEnv "CARDANO_NODE_SOCKET_PATH"
  networkParamsRef <- liftIO $ CAE.fetchNetworkParamsPeriodically nodeConnectionInfo 5
  dbConnectionString <- C8.pack <$> Environment.getEnv "DB_CONNECTION_STRING"

  oldApi <-
    Servant.mkClientEnv
      <$> HttpClient.newManager HttpClient.defaultManagerSettings
      <*> (Environment.getEnv "API_URL" >>= Servant.parseBaseUrl)
  loadedMigrationOperatorSigningKey <- readPaymentSigningKey "MIGRATION_OPERATOR_SIGNING_KEY"
  runStdoutLoggingT $
    withDbPoolLending dbConnectionString $ \connectionPool ->
      Reader.runReaderT
        (migrateManager >>= migrateAccounts)
        MigrationConfig
          { apiClientEnv = oldApi
          , nodeConnection = nodeConnectionInfo
          , scriptDir = scriptDirPath
          , numberAccountsPerBatch = numberAccounts migrationConfigFile
          , migrationOperatorSigningKey = loadedMigrationOperatorSigningKey
          , networkParams = networkParamsRef
          , dbConnection = connectionPool
          }

main :: IO ()
main = runApp run
