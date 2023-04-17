{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Services.Config
  ( ServiceContractsConfig (..)
  , loadServiceContractConfig
  )
where

import Cardano.Api qualified as CA
import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Data.Map qualified as Map
import Data.Yaml qualified as Yaml
import GHC.Generics qualified as Generics
import System.FilePath ((</>))

import Lending.Functions
  ( LendingFunction (LendingFunctionInterestRate, LendingFunctionProcessAccount, LendingFunctionUpdatePool)
  )
import Lending.Scripts (LendingScript (Account, OracleCheckerToken, Pool), scriptHashMapFileName)
import Plutarch.Import.Functions (FunctionTypedScript, loadFunction)

newtype ServiceConfigException = ScriptNotFound LendingScript
  deriving stock (Show)
  deriving anyclass (Exception)

data ServiceContractsConfig = ServiceContractsConfig
  { sccAccountPaymentScriptHash :: CA.ScriptHash
  , sccPoolPaymentScriptHash :: CA.ScriptHash
  , sccLendingFunctionInterestRate :: FunctionTypedScript 'LendingFunctionInterestRate
  , sccLendingFunctionProcessAccount :: FunctionTypedScript 'LendingFunctionProcessAccount
  , sccLendingFunctionUpdatePool :: FunctionTypedScript 'LendingFunctionUpdatePool
  , sccOracleCheckerToken :: CA.AssetId
  }
  deriving stock (Generics.Generic, Show)

loadServiceContractConfig :: FilePath -> IO ServiceContractsConfig
loadServiceContractConfig scriptDir = do
  scriptHashMap <- Yaml.decodeFileThrow (scriptDir </> scriptHashMapFileName)
  let getScriptHash script =
        maybe
          (Exception.throwIO (ScriptNotFound script))
          pure
          (Map.lookup script scriptHashMap)
      getAssetId script = flip CA.AssetId "" . CA.PolicyId <$> getScriptHash script

  ServiceContractsConfig
    <$> getScriptHash Account
    <*> getScriptHash Pool
    <*> loadFunction scriptDir LendingFunctionInterestRate
    <*> loadFunction scriptDir LendingFunctionProcessAccount
    <*> loadFunction scriptDir LendingFunctionUpdatePool
    <*> getAssetId OracleCheckerToken
