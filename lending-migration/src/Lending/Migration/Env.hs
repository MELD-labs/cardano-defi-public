{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Lending.Migration.Env (MigrationFileConfig (..), MigrationConfig (..), runSqlM) where

import Cardano.Api qualified as CA
import Control.Concurrent (MVar)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (FromJSON)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import GHC.Generics (Generic)

import Cardano.Api.Extra.NetworkId (NetworkIdText)
import Cardano.Api.Extra.NetworkParams qualified as TB
import Cardano.Api.Extra.Node (NodeConnectInfo)
import Lending.Index.Query.Types (SqlM)
import Servant.Client (ClientEnv)

data MigrationFileConfig = MigrationFileConfig
  { networkId :: NetworkIdText
  , numberAccounts :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data MigrationConfig = MigrationConfig
  { apiClientEnv :: ClientEnv
  , nodeConnection :: NodeConnectInfo
  , scriptDir :: FilePath
  , numberAccountsPerBatch :: Int
  , migrationOperatorSigningKey :: CA.SigningKey CA.PaymentExtendedKey
  , networkParams :: MVar TB.NetworkParams
  , dbConnection :: ConnectionPool
  }

runSqlM :: SqlM a -> ReaderT MigrationConfig (LoggingT IO) a
runSqlM sql = lift . runResourceT . runSqlPool sql =<< asks dbConnection
