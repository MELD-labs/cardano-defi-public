{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Migration.Manager (migrateManager) where

import Cardano.Api qualified as CA
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger (LoggingT)
import Control.Monad.Logger qualified as Logger
import Control.Monad.Reader qualified as Reader
import Control.Monad.Trans.Reader (ReaderT)
import Data.List qualified as List
import Database.Esqueleto.PostgreSQL.JSON (JSONB (unJSONB))
import Database.Persist ((==.))
import Database.Persist qualified as Persist
import Plutarch.Extra.AssetClass (AssetClass, toPlutusAssetClass)

import Cardano.Api.Extra.Adapters (fromCardanoAddressInEra)
import Cardano.Api.Extra.Tx (signSubmitAndWaitTx)
import Lending.Api.Client (migrateManagerClient)
import Lending.Api.Common (addressFromScriptHashes)
import Lending.Api.Types.Manager (UpdateManagerResponse (UpdateManagerResponse, umTx))
import Lending.Index.Manager (EntityField (ManagerClosingSlotNo, ManagerSlotNo), Manager (managerDatum, managerSlotNo))
import Lending.Index.Query.Manager (getManagerUtxo)
import Lending.Migration.Common (getAssetId, getScriptHash, migrationRetryOption, queryApi)
import Lending.Migration.Env
  ( MigrationConfig
      ( MigrationConfig
      , apiClientEnv
      , migrationOperatorSigningKey
      , nodeConnection
      , scriptDir
      )
  , runSqlM
  )
import Lending.Scripts (LendingScript (Account, AccountAuthToken))
import Lending.Types (ManagerDatum (ManagerDatum, mdAccountAddress, mdAccountAuthToken))
import TxBuilder.Api (UtxoInputWithDatum (UtxoInputWithDatum, uiwdDatum))

getOldDatum :: AssetClass -> ReaderT MigrationConfig (LoggingT IO) ManagerDatum
getOldDatum newAccountAuthToken = runSqlM $ do
  allManagerRows <- (Persist.entityVal <$>) <$> Persist.selectList [] [Persist.Asc ManagerSlotNo]
  let migrationRow =
        List.find ((newAccountAuthToken ==) . mdAccountAuthToken . unJSONB . managerDatum) allManagerRows
  migrationSlotNo <- maybe (fail "Unable to get migration slot no") (pure . managerSlotNo) migrationRow
  oldManagerRow <- Persist.selectFirst [ManagerClosingSlotNo ==. Just migrationSlotNo] []
  maybe (fail "Unable to extract old manager") (pure . unJSONB . managerDatum . Persist.entityVal) oldManagerRow

migrateManager :: ReaderT MigrationConfig (LoggingT IO) ManagerDatum
migrateManager = do
  Logger.logInfoN "Checking Manager"
  MigrationConfig {scriptDir, migrationOperatorSigningKey} <- Reader.ask
  UtxoInputWithDatum
    { uiwdDatum = currentDatum@ManagerDatum {mdAccountAuthToken = currentAccountAuthToken}
    } <-
    runSqlM getManagerUtxo
  newAccountAuthToken <- MonadIO.liftIO $ toPlutusAssetClass <$> getAssetId scriptDir AccountAuthToken
  if currentAccountAuthToken == newAccountAuthToken
    then Logger.logInfoN "Skipping Manager migration" >> getOldDatum currentAccountAuthToken
    else do
      Logger.logInfoN "Migrating Manager"
      newAccountAddress <-
        Reader.asks (addressFromScriptHashes . CA.localNodeNetworkId . nodeConnection)
          <*> MonadIO.liftIO (getScriptHash scriptDir Account)
          <*> pure Nothing
      let newDatum =
            currentDatum
              { mdAccountAuthToken = newAccountAuthToken
              , mdAccountAddress = fromCardanoAddressInEra newAccountAddress
              }
      UpdateManagerResponse {umTx} <-
        Reader.asks apiClientEnv >>= MonadIO.liftIO . flip queryApi (migrateManagerClient newDatum)
      _ <-
        Reader.withReaderT nodeConnection $
          signSubmitAndWaitTx (Just migrationRetryOption) migrationOperatorSigningKey umTx
      Logger.logInfoN "Migrated Manager successfully"
      pure currentDatum
