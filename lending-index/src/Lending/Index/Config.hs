{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Index.Config where

import Cardano.Api qualified as CA
import Cardano.Api.Shelley qualified as CA
import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics qualified as Generics

import Cardano.Index.ChainPoint.Model (ChainPointContext (ChainPointContext))
import Cardano.Index.Data.RawBytesHex (RawBytesHex (RawBytesHex))
import Cardano.Index.Extra.ScriptDeployment
  ( ScriptDeploymentConfig
      ( ScriptDeploymentConfig
      , sdcMaybeStakeCred
      , sdcPaymentCred
      )
  )
import Cardano.Index.Extra.WalletUtxo
  ( WalletUtxoConfig
      ( WalletUtxoConfig
      , wucPaymentCredential
      )
  )
import Cardano.Index.Has (Getter (getter))
import Lending.Index.Account (AccountConfig (AccountConfig))
import Lending.Index.Liquidation (LiquidationConfig (LiquidationConfig))
import Lending.Index.Manager (ManagerConfig (ManagerConfig))
import Lending.Index.Operator (OperatorConfig (OperatorConfig, ocAuthTokens))
import Lending.Index.Oracle (OracleConfig (OracleConfig))
import Lending.Index.Pool (PoolConfig (PoolConfig))
import Lending.Scripts
  ( LendingScript
      ( AccountAuthToken
      , AlwaysFalse
      , ManagerAuthToken
      , OperatorManagerNft
      , OperatorMigrationNft
      , OperatorOracleNft
      , OperatorPoolNft
      , OracleAuthToken
      , PoolAuthToken
      )
  )

newtype IndexConfigException = ScriptNotFound LendingScript
  deriving stock (Show)
  deriving anyclass (Exception)

newtype IndexOperationalConfig = IndexOperationalConfig
  { iocBatcherPubKeyHash :: RawBytesHex (CA.Hash CA.PaymentKey)
  }
  deriving stock (Generics.Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Data type of contracts config.
data ContractsConfig = ContractsConfig
  { ccCardanoAlwaysFalsePaymentScriptHash :: CA.ScriptHash
  -- ^ Payment script hash locking script deployments
  , ccCardanoAlwaysFalseStakeScriptHash :: Maybe CA.ScriptHash
  -- ^ Stake script hash locking script deployments
  , ccManagerNFT :: CA.AssetId
  , ccAccountNFT :: CA.AssetId
  , ccPoolNFT :: CA.AssetId
  , ccOracleNFT :: CA.AssetId
  , ccManagerOperatorNft :: CA.AssetId
  , ccOracleOperatorNft :: CA.AssetId
  , ccMigrationOperatorNft :: CA.AssetId
  , ccPoolOperatorNft :: CA.AssetId
  , ccOperationalConfig :: IndexOperationalConfig
  }
  deriving stock (Generics.Generic, Show)

instance Getter ScriptDeploymentConfig ContractsConfig where
  getter ContractsConfig {ccCardanoAlwaysFalsePaymentScriptHash, ccCardanoAlwaysFalseStakeScriptHash} =
    ScriptDeploymentConfig
      { sdcPaymentCred = ccCardanoAlwaysFalsePaymentScriptHash
      , sdcMaybeStakeCred = CA.StakeCredentialByScript <$> ccCardanoAlwaysFalseStakeScriptHash
      }

instance Getter WalletUtxoConfig ContractsConfig where
  getter ContractsConfig {ccOperationalConfig = IndexOperationalConfig {iocBatcherPubKeyHash = RawBytesHex pkh}} =
    WalletUtxoConfig
      { wucPaymentCredential = pkh
      }

instance Getter PoolConfig ContractsConfig where
  getter ContractsConfig {ccPoolNFT} = PoolConfig ccPoolNFT

instance Getter ManagerConfig ContractsConfig where
  getter ContractsConfig {ccManagerNFT} = ManagerConfig ccManagerNFT

instance Getter AccountConfig ContractsConfig where
  getter ContractsConfig {ccAccountNFT} = AccountConfig ccAccountNFT

instance Getter OracleConfig ContractsConfig where
  getter ContractsConfig {ccOracleNFT} = OracleConfig ccOracleNFT

instance Getter LiquidationConfig ContractsConfig where
  getter ContractsConfig {ccAccountNFT} = LiquidationConfig ccAccountNFT

instance Getter OperatorConfig ContractsConfig where
  getter ContractsConfig {ccManagerOperatorNft, ccOracleOperatorNft, ccMigrationOperatorNft, ccPoolOperatorNft} =
    OperatorConfig
      { ocAuthTokens =
          Map.fromList
            [ (OperatorManagerNft, ccManagerOperatorNft)
            , (OperatorOracleNft, ccOracleOperatorNft)
            , (OperatorMigrationNft, ccMigrationOperatorNft)
            , (OperatorPoolNft, ccPoolOperatorNft)
            ]
      }

instance Getter ChainPointContext ContractsConfig where
  getter = const ChainPointContext

loadContractConfig :: IndexOperationalConfig -> Map LendingScript CA.ScriptHash -> IO ContractsConfig
loadContractConfig opConfig scriptHashMap =
  ContractsConfig
    <$> getScriptHash AlwaysFalse
    <*> pure Nothing
    <*> getAssetId ManagerAuthToken
    <*> getAssetId AccountAuthToken
    <*> getAssetId PoolAuthToken
    <*> getAssetId OracleAuthToken
    <*> getAssetId OperatorManagerNft
    <*> getAssetId OperatorOracleNft
    <*> getAssetId OperatorMigrationNft
    <*> getAssetId OperatorPoolNft
    <*> pure opConfig
  where
    getAssetId script = flip CA.AssetId "" . CA.PolicyId <$> getScriptHash script
    getScriptHash script =
      maybe
        (Exception.throwIO (ScriptNotFound script))
        pure
        (Map.lookup script scriptHashMap)
