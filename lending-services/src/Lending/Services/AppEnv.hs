{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Services.AppEnv
  ( AppEnv (..)
  , AppM
  , SqlM
  , runWithEnv
  , runSqlM
  )
where

import Cardano.Api qualified as CA
import Control.Concurrent (MVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Class qualified as MonadTrans
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.Resource qualified as Resource
import Data.Aeson (FromJSON)
import Data.ByteString.Char8 qualified as C8
import Data.Yaml qualified as Yaml
import Database.Persist.Postgresql (ConnectionPool)
import Database.Persist.Postgresql qualified as Persist
import GHC.Generics (Generic)
import System.Environment qualified as Environment

import Cardano.Api.Extra.Key (readPaymentSigningKey)
import Cardano.Api.Extra.NetworkId (NetworkIdText (NetworkIdText))
import Cardano.Api.Extra.NetworkParams qualified as CAE
import Cardano.Api.Extra.Node qualified as CAE
import Cardano.Index.Context (SqlM)
import Lending.Core.Utils (withDbPoolLending)
import Lending.Services.Config (ServiceContractsConfig, loadServiceContractConfig)
import TxBuilder.Api qualified as TB

data ServiceConfig = ServiceConfig
  { lcCardanoNetworkId :: NetworkIdText
  , lcMinRequestsPerTx :: Int
  , lcMinRequestsPerAttemptedTx :: Int
  , lcNetworkParamsFetchDelay :: Int
  , lcPeriodCycle :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

data AppEnv = AppEnv
  { aeSigningKey :: CA.SigningKey CA.PaymentExtendedKey
  , aePool :: ConnectionPool
  , aeNodeConnection :: CA.LocalNodeConnectInfo CA.CardanoMode
  , aeNetworkId :: CA.NetworkId
  , aeContractsConfig :: ServiceContractsConfig
  , aeNetworkParams :: MVar TB.NetworkParams
  , aeMinRequestsPerTx :: Int
  , aeMinRequestsPerAttemptedTx :: Int
  , aePeriodCycle :: Int
  }

type AppM = ReaderT AppEnv (LoggingT IO)

runSqlM :: SqlM a -> AppM a
runSqlM sql = MonadTrans.lift . Resource.runResourceT . Persist.runSqlPool sql =<< Reader.asks aePool

runWithEnv :: AppM () -> IO ()
runWithEnv runner = do
  signingKey <- readPaymentSigningKey "BATCHER_SIGNING_KEY"
  dbConnection <- C8.pack <$> Environment.getEnv "DB_CONNECTION_STRING"
  ServiceConfig
    { lcCardanoNetworkId = NetworkIdText cardanoNetworkId
    , lcMinRequestsPerTx
    , lcMinRequestsPerAttemptedTx
    , lcNetworkParamsFetchDelay
    , lcPeriodCycle
    } <-
    Environment.getEnv "SERVICES_CONFIG_FILE" >>= Yaml.decodeFileThrow
  cardanoNodeConnection <-
    CAE.mkNodeConnectInfo cardanoNetworkId <$> Environment.getEnv "CARDANO_NODE_SOCKET_PATH"
  contractConfigs <-
    Environment.getEnv "SCRIPT_DIR" >>= loadServiceContractConfig
  networkParamsRef <- liftIO $ CAE.fetchNetworkParamsPeriodically cardanoNodeConnection lcNetworkParamsFetchDelay
  Logger.runStdoutLoggingT $
    withDbPoolLending dbConnection $ \pool ->
      Reader.runReaderT
        runner
        AppEnv
          { aeSigningKey = signingKey
          , aePool = pool
          , aeNodeConnection = cardanoNodeConnection
          , aeNetworkId = cardanoNetworkId
          , aeContractsConfig = contractConfigs
          , aeNetworkParams = networkParamsRef
          , aeMinRequestsPerTx = lcMinRequestsPerTx
          , aeMinRequestsPerAttemptedTx = lcMinRequestsPerAttemptedTx
          , aePeriodCycle = lcPeriodCycle
          }
