{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | General module that contains type definitions
module Lending.Api.Env
  ( ApiConfig (..)
  , AppEnv (..)
  , AppM
  , runSqlM
  , hoistScriptError
  )
where

import Cardano.Api qualified as CA
import Control.Concurrent (MVar)
import Control.Monad ((>=>))
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger (LoggingT)
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.Resource qualified as Resource
import Data.Aeson (FromJSON)
import Data.Map qualified as Map
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sql qualified as Sql
import GHC.Generics (Generic)
import Servant (Handler)

import Cardano.Api.Extra.NetworkId (NetworkIdText)
import Lending.Api.Config (ContractsConfigApi)
import Lending.Api.Types.Exception (ServerError (InvalidScript))
import Lending.Core.Errors (ScriptError)
import Lending.Index.Query.Types (SqlM)
import Lending.Types.Asset (Asset)
import TxBuilder.Api qualified as TB

-- | Api configuration, read from a local yaml file
data ApiConfig = ApiConfig
  { acCardanoNetworkId :: NetworkIdText
  , acNetworkParamsFetchDelay :: Int
  , acTokenDecimalMap :: Map.Map Asset Integer
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

-- | The full API environment type of the server.
-- contains all the data we need to run the api server successfully
data AppEnv = AppEnv
  { aeNormalConnectionPool :: ConnectionPool
  , aeProtocalStateConnectionPool :: ConnectionPool
  , aeNodeConnection :: CA.LocalNodeConnectInfo CA.CardanoMode
  , aeNetworkParams :: MVar TB.NetworkParams
  , aeContractsConfig :: ContractsConfigApi
  , aeTokenDecimalMap :: Map.Map Asset Integer
  }

-- | Main type synonym used in the api code
type AppM = ReaderT AppEnv (LoggingT Handler)

-- | Run a database action given a 'Config'
runSqlM :: SqlM a -> (AppEnv -> ConnectionPool) -> AppM a
runSqlM sql = Reader.asks >=> MonadIO.liftIO . Logger.runStderrLoggingT . Resource.runResourceT . Sql.runSqlPool sql

hoistScriptError :: Either ScriptError a -> AppM a
hoistScriptError = either (Catch.throwM . InvalidScript) pure
