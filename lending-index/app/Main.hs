{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.ByteString.Char8 qualified as Char8
import Data.Yaml qualified as Yaml
import System.Environment qualified as Environment

import Cardano.Index.App (runIndex)
import Cardano.Index.Context (AppEnvContext (AppEnvContext), AppFileContext (AppFileContext, fctxConfig))
import Cardano.Index.Extra.ScriptDeployment (CardanoScriptDeployment)
import Cardano.Index.Extra.WalletUtxo (CardanoWalletUtxo)
import Lending.Index.Account (Account)
import Lending.Index.Config (ContractsConfig, loadContractConfig)
import Lending.Index.Liquidation (Liquidation)
import Lending.Index.Manager (Manager)
import Lending.Index.Operator (Operator)
import Lending.Index.Oracle (Oracle)
import Lending.Index.Pool (Pool)
import Service.Runner (runApp)

type AllHandlers = [CardanoScriptDeployment, CardanoWalletUtxo, Liquidation, Manager, Pool, Account, Oracle, Operator]

run :: IO ()
run = do
  rawAppFileContext@AppFileContext {fctxConfig = opConfig} <-
    Environment.getEnv "INDEX_CONFIG_FILE" >>= Yaml.decodeFileThrow
  contractConfigs <- Environment.getEnv "SCRIPT_HASH_MAP_FILE" >>= Yaml.decodeFileThrow >>= loadContractConfig opConfig
  let appFileContext =
        rawAppFileContext
          { fctxConfig = contractConfigs
          }
  appEnvContext <-
    AppEnvContext
      <$> (Char8.pack <$> Environment.getEnv "DB_CONNECTION_STRING")
      <*> Environment.getEnv "CARDANO_NODE_SOCKET_PATH"
  runIndex @AllHandlers @ContractsConfig appFileContext appEnvContext

main :: IO ()
main = runApp run
