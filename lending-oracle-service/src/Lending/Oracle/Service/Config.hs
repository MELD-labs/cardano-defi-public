{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Oracle.Service.Config
  ( AssetConfig (..)
  , OracleServiceContractsConfig (..)
  , CoinMarketCapConfig (..)
  , loadOracleServiceContractConfig
  )
where

import Cardano.Api qualified as CA
import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics qualified as Generics

import Lending.Scripts (LendingScript (OperatorOracleNft, Oracle))
import Lending.Types.Asset (Asset)

newtype OracleServiceConfigException = ScriptNotFound LendingScript
  deriving stock (Show)
  deriving anyclass (Exception)

data OracleServiceContractsConfig = OracleServiceContractsConfig
  { occOraclePaymentScriptHash :: CA.ScriptHash
  , occOracleStakeScriptHash :: Maybe CA.ScriptHash
  , occOracleOperatorNft :: CA.AssetId
  }
  deriving stock (Generics.Generic, Show)

data AssetConfig = AssetConfig
  { adSlug :: Text
  , adDecimal :: Integer
  }
  deriving stock (Generics.Generic, Show, Eq)
  deriving anyclass (FromJSON)

-- | Data types of config for CoinMarketCap service
data CoinMarketCapConfig = CoinMarketCapConfig
  { cmCoinMarketCapUrl :: String
  , cmCoinMarketCapApiKey :: ByteString
  , cmTokensString :: String
  , -- TODO: Place this config in database for better updating list assets
    cmAssetsAndSlugs :: Map.Map Asset AssetConfig
  }

loadOracleServiceContractConfig :: Map.Map LendingScript CA.ScriptHash -> IO OracleServiceContractsConfig
loadOracleServiceContractConfig scriptHashMap =
  OracleServiceContractsConfig
    <$> getScriptHash Oracle
    <*> pure Nothing
    <*> getAssetId OperatorOracleNft
  where
    getScriptHash script =
      maybe
        (Exception.throwIO (ScriptNotFound script))
        pure
        (Map.lookup script scriptHashMap)

    getAssetId script = flip CA.AssetId "" . CA.PolicyId <$> getScriptHash script
