{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module LiquidationBot.Env
  ( AppEnv (..)
  , AppM
  , LiquidationBotConfig (..)
  , LiquidatingCollateralConfig (..)
  , runSqlM
  )
where

-- \| General module that contains type definitions
import Cardano.Api qualified as CA
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (FromJSON, Options (sumEncoding), SumEncoding (ObjectWithSingleField), ToJSON)
import Data.Aeson.TH qualified as Aeson
import Data.Map (Map)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import GHC.Generics (Generic)
import Lending.Types.Orphans ()

import Cardano.Api.Extra.NetworkId (NetworkIdText)
import Lending.Core.AccountValue (AccountId)
import Lending.Index.Query.Types (SqlM)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual)
import Plutarch.Extra.FixedDecimal (FixedDecimal)

data LiquidatingCollateralConfig = Custom (Map Asset Actual) | PriorityOrder [Asset]
  deriving stock (Eq, Show, Generic)

$(Aeson.deriveJSON Aeson.defaultOptions {sumEncoding = ObjectWithSingleField} ''LiquidatingCollateralConfig)

-- | The configs that are provided by the liquidator.
data LiquidationBotConfig = LiquidationBotConfig
  { lbRetryDelay :: Integer
  , lbAccountId :: AccountId
  , lbRepayExtra :: FixedDecimal 6
  , lbNetworkId :: NetworkIdText
  , lbLiquidatingDebt :: Map Asset Actual
  , lbLiquidatingCollateral :: LiquidatingCollateralConfig
  , lbMaxRetry :: Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The full environment type of the server.
-- contains all the data we need to run the liquidation bot successfully
data AppEnv = AppEnv
  { aeLiquidatorSigningKey :: CA.SigningKey CA.PaymentExtendedKey
  , aeLiquidatorAddress :: CA.AddressInEra CA.BabbageEra
  , aeLiquidationBotConfig :: LiquidationBotConfig
  , aeNodeConnection :: CA.LocalNodeConnectInfo CA.CardanoMode
  , aeDbConnectionPool :: ConnectionPool
  }

-- | Main type synonym used in the liquidation bot code
type AppM = ReaderT AppEnv (LoggingT IO)

-- | Run a database action given a 'Config'
runSqlM :: SqlM a -> (AppEnv -> ConnectionPool) -> AppM a
runSqlM sql getConnectionPool = lift . runResourceT . runSqlPool sql =<< asks getConnectionPool
