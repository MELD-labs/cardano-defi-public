{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Lending.Index.Exception (IndexException (..)) where

import Cardano.Api qualified as CA
import Control.Exception (Exception)
import Lending.Scripts (LendingScript)

data IndexException
  = IndexAccountUtxoNotFoundException CA.TxIn
  | IndexUtxoNotFoundException LendingScript
  | IndexFindScriptException CA.ScriptHash
  | IndexChainPointNotFound
  deriving anyclass (Exception)

instance Show IndexException where
  show (IndexAccountUtxoNotFoundException accId) = "Unable to find account with id: " <> show accId
  show (IndexUtxoNotFoundException script) = "Unable to find UTXO for script " <> show script
  show (IndexFindScriptException scriptHash) = "Unable to find script deployment for script hash " <> show scriptHash
  show IndexChainPointNotFound = "Unable to query latest chain point from indexer"
