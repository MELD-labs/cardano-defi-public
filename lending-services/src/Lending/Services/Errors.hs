{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Lending.Services.Errors (BatchingServiceError (..)) where

import Cardano.Api qualified as CA
import Control.Exception (Exception)
import Ouroboros.Consensus.HardFork.History.Qry qualified as Qry

import Plutarch.Extra.AssetClass (AssetClass)
import TxBuilder.Api (UtxoInput)

data BatchingServiceError
  = BatchingServiceConvertTimeError Qry.PastHorizonException
  | BatchingServiceParseOracleCheckerError AssetClass
  | BatchingServiceParsePoolDatumError [UtxoInput]
  | BatchingServiceBalanceTxError CA.TxBodyErrorAutoBalance
  | BatchingServiceNoneBatcherUtxosError
  | BatchingServiceQueryBatcherUtxosCoinSelectionError String
  | BatchingServicePoolValueError CA.Value
  deriving anyclass (Exception)

instance Show BatchingServiceError where
  show (BatchingServiceConvertTimeError exception) =
    "Convert slot time to POSIX time and get slot length error" <> show exception
  show (BatchingServiceParseOracleCheckerError assetClass) = "Unable to parse oracle checker" <> show assetClass
  show (BatchingServiceParsePoolDatumError utxos) = "Unable to parse new pool datum" <> show utxos
  show (BatchingServiceBalanceTxError err) = "Build batching balanced Tx body error: " <> show err
  show BatchingServiceNoneBatcherUtxosError = "Cannot find any wallet utxos"
  show (BatchingServiceQueryBatcherUtxosCoinSelectionError err) = "Not have desirable wallet's utxo: " <> show err
  show (BatchingServicePoolValueError err) = "Cannot withdraw more than current Pool value: " <> show err
