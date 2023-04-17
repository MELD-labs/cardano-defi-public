{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Lending.Migration.Exception (MigrationException (..)) where

import Cardano.Api qualified as CA
import Control.Exception (Exception)
import Servant.Client (ClientError)

import Lending.Scripts (LendingScript)

data MigrationException
  = MigrationQueryOldApiError ClientError
  | ScriptNotFound LendingScript
  | MigrationBuildTxError CA.TxBodyErrorAutoBalance
  | TooManyUtxoInRequest Int
  deriving anyclass (Exception)

instance Show MigrationException where
  show (MigrationQueryOldApiError e) = "Api query failed with error: " <> show e
  show (ScriptNotFound script) = "Can not find script hash " <> show script
  show (MigrationBuildTxError e) = "Build migration tx error: " <> show e
  show (TooManyUtxoInRequest numUtxo) =
    "Migrate TooManyUtxoInRequest with " <> show numUtxo <> " UTxOs"
