{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Api.Config (ApiConfigException (..), ContractsConfigApi (..), loadContractConfigApi) where

import Cardano.Api qualified as CA
import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Yaml qualified as Yaml
import GHC.Generics qualified as Generics
import Plutus.V2.Ledger.Api (TxOutRef)
import Ply (ScriptRole (MintingPolicyRole), TypedScript)
import Ply qualified
import System.FilePath ((</>))

import Lending.Functions
  ( LendingFunction
      ( LendingFunctionBorrowApy
      , LendingFunctionCalculateAccountValue
      , LendingFunctionCalculateLiquidationResult
      , LendingFunctionInterestRate
      , LendingFunctionProcessAccount
      , LendingFunctionSupplyApy
      , LendingFunctionUpdatePool
      , LendingFunctionUtilization
      )
  )
import Lending.Scripts
  ( LendingScript
      ( Account
      , AccountAuthToken
      , AccountOwnerNft
      , Manager
      , ManagerAuthToken
      , OperatorManagerNft
      , OperatorMigrationNft
      , OperatorOracleNft
      , OperatorPoolNft
      , Oracle
      , OracleAuthToken
      , OracleCheckerToken
      , Pool
      , PoolAuthToken
      )
  , scriptFileName
  , scriptHashMapFileName
  )
import Plutarch.Import.Functions (FunctionTypedScript, loadFunction)

newtype ApiConfigException = ScriptNotFound LendingScript
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Data type of contracts config for lending-api
data ContractsConfigApi = ContractsConfigApi
  { ccaAccountScriptHash :: CA.ScriptHash
  -- ^ Account payment script hash
  , ccaAccountAuthToken :: CA.AssetId
  -- ^ Account auth token asset id
  , ccaManagerScriptHash :: CA.ScriptHash
  -- ^ Manager payment script hash
  , ccaManagerAuthToken :: CA.AssetId
  -- ^ Manager auth token asset id
  , ccaOracleScriptHash :: CA.ScriptHash
  -- ^ Oracle payment script hash
  , ccaOracleAuthToken :: CA.AssetId
  -- ^ Oracle auth token asset id
  , ccaPoolScriptHash :: CA.ScriptHash
  -- ^ Pool payment script hash
  , ccaPoolAuthToken :: CA.AssetId
  -- ^ Pool auth token asset id
  , ccaAccountOwnerNftScript :: TypedScript 'MintingPolicyRole '[TxOutRef]
  -- ^ Parameterized account owner NFT script
  , ccaOperatorManagerNft :: CA.AssetId
  -- ^ Operator Manager NFT
  , ccaOperatorOracleNft :: CA.AssetId
  -- ^ Operator Oracle NFT
  , ccaOperatorMigrationNft :: CA.AssetId
  -- ^ Operator Migration NFT
  , ccaOperatorPoolNft :: CA.AssetId
  -- ^ Operator Pool NFT
  , ccaOracleCheckerToken :: CA.AssetId
  , ccaLendingFunctionInterestRate :: FunctionTypedScript 'LendingFunctionInterestRate
  -- ^ Exported function calculating new interest rates
  , ccaLendingFunctionUtilization :: FunctionTypedScript 'LendingFunctionUtilization
  -- ^ Exported function calculating utilization
  , ccaLendingFunctionSupplyApy :: FunctionTypedScript 'LendingFunctionSupplyApy
  -- ^ Exported function calculating supply APY
  , ccaLendingFunctionBorrowApy :: FunctionTypedScript 'LendingFunctionBorrowApy
  -- ^ Exported function calculating borrow APY
  , ccaLendingFunctionCalculateLiquidationResult :: FunctionTypedScript 'LendingFunctionCalculateLiquidationResult
  -- ^ Exported function calculating liquidation result
  , ccaLendingFunctionCalculateAccountValue :: FunctionTypedScript 'LendingFunctionCalculateAccountValue
  -- ^ Exported function calculating value an account
  , ccaLendingFunctionProcessAccount :: FunctionTypedScript 'LendingFunctionProcessAccount
  -- ^ Exported function processing an account
  , ccaLendingFunctionUpdatePool :: FunctionTypedScript 'LendingFunctionUpdatePool
  -- ^ Exported function updating pool
  }
  deriving stock (Generics.Generic, Show)

loadContractConfigApi :: FilePath -> IO ContractsConfigApi
loadContractConfigApi scriptDir = do
  scriptHashMap <- Yaml.decodeFileThrow (scriptDir </> scriptHashMapFileName) :: IO (Map LendingScript CA.ScriptHash)
  let getScriptHash script =
        maybe
          (Exception.throwIO (ScriptNotFound script))
          pure
          (Map.lookup script scriptHashMap)
      getAssetId script = flip CA.AssetId "" . CA.PolicyId <$> getScriptHash script
      getScript script = Ply.readTypedScript (scriptDir </> scriptFileName script)
  ContractsConfigApi
    <$> getScriptHash Account
    <*> getAssetId AccountAuthToken
    <*> getScriptHash Manager
    <*> getAssetId ManagerAuthToken
    <*> getScriptHash Oracle
    <*> getAssetId OracleAuthToken
    <*> getScriptHash Pool
    <*> getAssetId PoolAuthToken
    <*> getScript AccountOwnerNft
    <*> getAssetId OperatorManagerNft
    <*> getAssetId OperatorOracleNft
    <*> getAssetId OperatorMigrationNft
    <*> getAssetId OperatorPoolNft
    <*> getAssetId OracleCheckerToken
    <*> loadFunction scriptDir LendingFunctionInterestRate
    <*> loadFunction scriptDir LendingFunctionUtilization
    <*> loadFunction scriptDir LendingFunctionSupplyApy
    <*> loadFunction scriptDir LendingFunctionBorrowApy
    <*> loadFunction scriptDir LendingFunctionCalculateLiquidationResult
    <*> loadFunction scriptDir LendingFunctionCalculateAccountValue
    <*> loadFunction scriptDir LendingFunctionProcessAccount
    <*> loadFunction scriptDir LendingFunctionUpdatePool
