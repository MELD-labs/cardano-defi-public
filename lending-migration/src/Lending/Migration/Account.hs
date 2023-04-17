{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Migration.Account (migrateAccounts) where

import Cardano.Api qualified as CA
import Cardano.Api.Extra.Query qualified as CAE
import Control.Concurrent qualified as Concurrent
import Control.Monad qualified as Monad
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Foldable qualified as Foldable
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Plutus.V2.Ledger.Api (Address (Address), Credential (ScriptCredential))

import Cardano.Api.Extra.Adapters (toAddressAny, toCardanoAddressInEra, toCardanoScriptHash)
import Cardano.Api.Extra.Tx (signSubmitAndWaitTx)
import Lending.Api.Common (addressFromScriptHashes)
import Lending.Core.Utils (fromTxOutDatumUtxoToTx)
import Lending.Index.Query.ScriptDeployment (queryScriptDeployment)
import Lending.Migration.Common (getAssetId, getScriptHash, getSenderConstraint, migrationRetryOption)
import Lending.Migration.Env
  ( MigrationConfig
      ( MigrationConfig
      , migrationOperatorSigningKey
      , networkParams
      , nodeConnection
      , numberAccountsPerBatch
      , scriptDir
      )
  , runSqlM
  )
import Lending.Scripts (LendingScript (Account, AccountAuthToken))
import Lending.Types.Account (AccountRedeemer (AccountMigrateRedeemer), AccountTokenRedeemer (Migrate))
import Lending.Types.Manager (ManagerDatum (ManagerDatum, mdAccountAddress, mdAccountAuthToken))
import Plutarch.Extra.AssetClass (AssetClass, fromPlutusAssetClass)
import TxBuilder.Api
  ( UtxoInput (UtxoInput, uiTxOut)
  , WitnessScript (RefScript)
  , buildM
  , mustMintValue
  , mustPayTo
  , mustSpendFromScript
  )

isValidAccount :: CA.AssetId -> CA.TxOut CA.CtxUTxO CA.BabbageEra -> Bool
isValidAccount accountAuthToken (CA.TxOut _ val _ _) = CA.selectAsset (CA.txOutValueToValue val) accountAuthToken > 0

getAllAccounts :: ManagerDatum -> ReaderT MigrationConfig (LoggingT IO) [UtxoInput]
getAllAccounts ManagerDatum {mdAccountAddress = oldAccountAddress, mdAccountAuthToken} = do
  networkId <- Reader.asks (CA.localNodeNetworkId . nodeConnection)
  oldAccountCardanoAddress <-
    either Catch.throwM (pure . toAddressAny) (toCardanoAddressInEra networkId oldAccountAddress)
  utxos <-
    Reader.withReaderT nodeConnection $
      CAE.executeShelleyQuery @CA.BabbageEra $
        CA.QueryUTxO (CA.QueryUTxOByAddress (Set.fromList [oldAccountCardanoAddress]))
  accountAuthToken <- maybe (fail "Unable to parse auth token") pure (fromPlutusAssetClass mdAccountAuthToken)
  pure (uncurry UtxoInput <$> Map.toList (Map.filter (isValidAccount accountAuthToken) (CA.unUTxO utxos)))

migrateAccounts :: ManagerDatum -> ReaderT MigrationConfig (LoggingT IO) ()
migrateAccounts managerMigrationResult =
  List.chunksOf <$> Reader.asks numberAccountsPerBatch <*> getAllAccounts managerMigrationResult
    >>= Foldable.traverse_ (process managerMigrationResult)

estimatedTxFee :: CA.Lovelace
estimatedTxFee = 6_000_000

extractValidatorScriptHash :: Address -> ReaderT MigrationConfig (LoggingT IO) CA.ScriptHash
extractValidatorScriptHash (Address (ScriptCredential valHash) _) =
  either (const (fail "Unable to extract script hash")) pure (toCardanoScriptHash valHash)
extractValidatorScriptHash _ = fail "Address is not script address"

extractToken :: AssetClass -> ReaderT MigrationConfig (LoggingT IO) CA.AssetId
extractToken = maybe (fail "Unable to extract token") pure . fromPlutusAssetClass

loadScript :: (FilePath -> LendingScript -> IO a) -> LendingScript -> ReaderT MigrationConfig (LoggingT IO) a
loadScript f script = Reader.asks scriptDir >>= MonadIO.liftIO . flip f script

process :: ManagerDatum -> [UtxoInput] -> ReaderT MigrationConfig (LoggingT IO) ()
process
  ManagerDatum {mdAccountAddress = oldAccountAddress, mdAccountAuthToken = oldAccountAuthToken}
  accountInputs = do
    MigrationConfig {migrationOperatorSigningKey} <- Reader.ask
    networkP <- Reader.asks networkParams >>= MonadIO.liftIO . Concurrent.readMVar

    oldAuthTokenAsset@(CA.AssetId oldAccountAuthTokenPolicyId _) <- extractToken oldAccountAuthToken
    oldAuthTokenRefUtxo <- runSqlM $ queryScriptDeployment (CA.unPolicyId oldAccountAuthTokenPolicyId)
    oldAccountAddressRefUtxo <- extractValidatorScriptHash oldAccountAddress >>= runSqlM . queryScriptDeployment

    newAuthTokenAsset <- loadScript getAssetId AccountAuthToken
    newAccountAuthTokenRefUtxo <- loadScript getScriptHash AccountAuthToken >>= runSqlM . queryScriptDeployment
    newAccountAddress <-
      Reader.asks (addressFromScriptHashes . CA.localNodeNetworkId . nodeConnection)
        <*> loadScript getScriptHash Account
        <*> pure Nothing

    senderConstraint <- getSenderConstraint estimatedTxFee
    let numberOfAccounts = toInteger $ length accountInputs
        mintConstraint =
          mustMintValue
            newAuthTokenAsset
            numberOfAccounts
            (RefScript CA.PlutusScriptV2 newAccountAuthTokenRefUtxo)
            Migrate
        burnConstraint =
          mustMintValue
            oldAuthTokenAsset
            (-numberOfAccounts)
            (RefScript CA.PlutusScriptV2 oldAuthTokenRefUtxo)
            Migrate
        getSpendConstraint input =
          mustSpendFromScript
            input
            (RefScript CA.PlutusScriptV2 oldAccountAddressRefUtxo)
            CA.InlineScriptDatum
            AccountMigrateRedeemer
        getPayConstraint UtxoInput {uiTxOut = CA.TxOut _ oldValue datum _} =
          mustPayTo
            newAccountAddress
            (CA.txOutValueToValue oldValue <> CA.valueFromList [(oldAuthTokenAsset, -1), (newAuthTokenAsset, 1)])
            (fromTxOutDatumUtxoToTx datum)
        getAccountConstraint input = getSpendConstraint input <> getPayConstraint input
        accountConstraints = foldMap getAccountConstraint accountInputs
        constraints = mintConstraint <> burnConstraint <> accountConstraints <> senderConstraint

    tx <- buildM constraints networkP (Just 1)
    Monad.void $
      Reader.withReaderT nodeConnection $
        signSubmitAndWaitTx (Just migrationRetryOption) migrationOperatorSigningKey tx
