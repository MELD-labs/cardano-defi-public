{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Lending.Mock.Api.AppEnv
  ( AppEnv (..)
  , AppM
  , AssetConfig (..)
  , AssetsPrices
  , MockApiConfig (..)
  )
where

import Control.Concurrent (MVar)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Servant (Handler)

import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)

type AssetsPrices = Map.Map Asset Price

newtype MockApiConfig = MockApiConfig
  {ocTokens :: Map.Map Asset AssetConfig}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Yaml.FromJSON)

data AssetConfig = AssetConfig
  { adSlug :: Text
  , adDecimal :: Integer
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Yaml.FromJSON)

data AppEnv = AppEnv
  { aeAssetsPrices :: MVar AssetsPrices
  , aeAssetsAndSlugs :: Map.Map Asset AssetConfig
  }

type AppM = ReaderT AppEnv (LoggingT Handler)
