{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Oracle.Service.AppEnv
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
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import Database.Persist.Postgresql (ConnectionPool)
import Database.Persist.Postgresql qualified as Persist
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClient
import Servant.Client (ClientEnv)
import Servant.Client qualified as Servant
import System.Environment qualified as Environment

import Cardano.Api.Extra.Key (readPaymentSigningKey)
import Cardano.Api.Extra.NetworkId (NetworkIdText (NetworkIdText))
import Cardano.Api.Extra.NetworkParams qualified as CAE
import Cardano.Api.Extra.Node qualified as CAE
import Cardano.Index.Context (SqlM)
import Lending.Core.Utils (withDbPoolLending)
import Lending.Oracle.Service.Config
  ( AssetConfig (adSlug)
  , CoinMarketCapConfig (CoinMarketCapConfig)
  , OracleServiceContractsConfig
  , loadOracleServiceContractConfig
  )
import Lending.Types.Asset (Asset)
import TxBuilder.Api qualified as TB

data OracleServiceConfig = OracleServiceConfig
  { ocCardanoNetworkId :: NetworkIdText
  , ocTokens :: Map.Map Asset AssetConfig
  , ocThreadDelay :: Int
  , ocNetworkParamsFetchDelay :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

data AppEnv = AppEnv
  { aeSigningKey :: CA.SigningKey CA.PaymentExtendedKey
  , aePool :: ConnectionPool
  , aeNodeConnection :: CA.LocalNodeConnectInfo CA.CardanoMode
  , aeNetworkId :: CA.NetworkId
  , aeContractsConfig :: OracleServiceContractsConfig
  , aeCMCConfig :: CoinMarketCapConfig
  , aeNetworkParams :: MVar TB.NetworkParams
  , aeAPIClientEnv :: ClientEnv
  , aeThreadDelay :: Int
  }

type AppM = ReaderT AppEnv (LoggingT IO)

runSqlM :: SqlM a -> AppM a
runSqlM sql = MonadTrans.lift . Resource.runResourceT . Persist.runSqlPool sql =<< Reader.asks aePool

getHttpClientEnv :: C8.ByteString -> String -> IO ClientEnv
getHttpClientEnv cmcApiKey env =
  let addHeader req = return $ req {HttpClient.requestHeaders = [("X-CMC_PRO_API_KEY", cmcApiKey)]}
   in Servant.mkClientEnv
        <$> HttpClient.newTlsManagerWith (HttpClient.tlsManagerSettings {HttpClient.managerModifyRequest = addHeader})
        <*> (Environment.getEnv env >>= Servant.parseBaseUrl)

getApiKey :: IO String
getApiKey = Environment.lookupEnv env >>= maybe (Environment.getEnv (env <> "_FILE") >>= readFile) pure
  where
    env :: String
    env = "CMC_PRO_API_KEY"

runWithEnv :: AppM () -> IO ()
runWithEnv runner = do
  signingKey <- readPaymentSigningKey "ORACLE_OPERATOR_SIGNING_KEY"
  dbConnection <- C8.pack <$> Environment.getEnv "DB_CONNECTION_STRING"
  OracleServiceConfig
    { ocCardanoNetworkId = NetworkIdText cardanoNetworkId
    , ocTokens
    , ocThreadDelay
    , ocNetworkParamsFetchDelay
    } <-
    Environment.getEnv "ORACLE_SERVICE_CONFIG_FILE" >>= Yaml.decodeFileThrow
  cardanoNodeConnection <-
    CAE.mkNodeConnectInfo cardanoNetworkId <$> Environment.getEnv "CARDANO_NODE_SOCKET_PATH"
  contractConfigs <-
    Environment.getEnv "SCRIPT_HASH_MAP_FILE" >>= Yaml.decodeFileThrow >>= loadOracleServiceContractConfig
  cmcApiKey <- C8.pack . List.trim <$> getApiKey
  cmcUrl <- Environment.getEnv "CMC_URL"
  let tokenNames = Text.unpack $ Text.intercalate "," $ Map.elems (adSlug <$> ocTokens)
  networkParamsRef <- liftIO $ CAE.fetchNetworkParamsPeriodically cardanoNodeConnection ocNetworkParamsFetchDelay
  clientEnv <- getHttpClientEnv cmcApiKey "CMC_URL"
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
          , aeCMCConfig = CoinMarketCapConfig cmcUrl cmcApiKey tokenNames ocTokens
          , aeNetworkParams = networkParamsRef
          , aeAPIClientEnv = clientEnv
          , aeThreadDelay = ocThreadDelay * 1_000_000
          }
