{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lending.Migration.Common
  ( queryApi
  , getScriptHash
  , getAssetId
  , migrationRetryOption
  , getSenderConstraint
  )
where

import Cardano.Api qualified as CA
import Cardano.Api.Extra.Wait (RetryOption (RetryOption, roDelayInSeconds, roMaxRetries))
import Control.Exception qualified as Exception
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader qualified as Reader
import Data.Map qualified as Map
import Data.Yaml qualified as Yaml
import Lending.Core.Utils qualified as Utils
import Lending.Index.Query.Operator (getOperatorNftUtxo)
import Lending.Migration.Env (MigrationConfig (nodeConnection, scriptDir), runSqlM)
import Lending.Migration.Exception (MigrationException (MigrationQueryOldApiError, ScriptNotFound))
import Lending.Scripts (LendingScript (OperatorMigrationNft), scriptHashMapFileName)
import Servant.Client (ClientEnv)
import Servant.Client.Internal.HttpClient (ClientM)
import System.FilePath ((</>))
import TxBuilder.Api (BuildConstraints, UtxoInput (UtxoInput, uiTxOut))

queryApi :: ClientEnv -> ClientM response -> IO response
queryApi = Utils.queryApi (Catch.throwM . MigrationQueryOldApiError)

getScriptHash :: FilePath -> LendingScript -> IO CA.ScriptHash
getScriptHash dir script = do
  scriptHashMap <-
    Yaml.decodeFileThrow (dir </> scriptHashMapFileName) :: IO (Map.Map LendingScript CA.ScriptHash)
  maybe
    (Exception.throwIO (ScriptNotFound script))
    pure
    (Map.lookup script scriptHashMap)

getAssetId :: FilePath -> LendingScript -> IO CA.AssetId
getAssetId dir script = do
  policy <- getScriptHash dir script
  pure $ CA.AssetId (CA.PolicyId policy) ""

migrationRetryOption :: RetryOption
migrationRetryOption = RetryOption {roMaxRetries = 30, roDelayInSeconds = 10}

getSenderConstraint :: CA.Lovelace -> ReaderT MigrationConfig (LoggingT IO) BuildConstraints
getSenderConstraint estimatedTxFee = do
  operatorMigrationNft <- Reader.asks scriptDir >>= MonadIO.liftIO . flip getAssetId OperatorMigrationNft
  UtxoInput {uiTxOut = CA.TxOut operatorMigrationAddress _ _ _} <- runSqlM (getOperatorNftUtxo OperatorMigrationNft)
  (userUtxos, collateralUtxos) <-
    Reader.withReaderT nodeConnection $
      Reader.mapReaderT MonadIO.liftIO $
        Utils.queryUserInputsAndCollateralBaseOnSpendingValue
          operatorMigrationAddress
          (CA.lovelaceToValue estimatedTxFee <> CA.valueFromList [(operatorMigrationNft, 1)])
  pure (Utils.toTxSenderConstraints userUtxos collateralUtxos operatorMigrationAddress)
