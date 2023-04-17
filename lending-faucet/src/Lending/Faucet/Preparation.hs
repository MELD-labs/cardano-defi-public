{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

module Lending.Faucet.Preparation (prepare, prepareApi) where

import Cardano.Api qualified as CA
import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async.Lifted qualified as Async
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.State qualified as State
import Data.List.Extra qualified as List
import Data.Semigroup qualified as Semigroup
import Data.Tuple.Extra qualified as Tuple
import Servant (ServerT)

import Cardano.Api.Extra.Node (NodeIO)
import Cardano.Api.Extra.Tx (signSubmitAndWaitTx)
import Lending.Core.Utils (getChangeAddress, queryUserInputsAndCollaterals)
import Lending.Core.Utils qualified as Utils
import Lending.Faucet.Common (getPacketValue, isPacketUtxo, reloadFaucetUtxos)
import Lending.Faucet.Env (AppEnv (aeFaucetConfig, aeNetworkParams, aeNodeConnection, aeOperatorSigningKey), AppM)
import Lending.Faucet.Types
  ( PrepareFaucetApi
  , PrepareFaucetRequest (PrepareFaucetRequest, pfrFirstLayer, pfrSecondLayer)
  , PrepareFaucetResponse (PrepareFaucetResponse, pfrFirstLayerTxId, pfrSecondLayerTxIds)
  )
import Lending.Types.Orphans ()
import TxBuilder.Api (UtxoInput (UtxoInput, uiTxIn, uiTxOut), buildM)
import TxBuilder.Api qualified as TB

phase2Fee :: CA.Value
phase2Fee = CA.lovelaceToValue 4_000_000

runNodeIO :: NodeIO a -> AppM a
runNodeIO = Reader.withReaderT aeNodeConnection . Reader.mapReaderT MonadIO.liftIO

prepare :: PrepareFaucetRequest -> AppM PrepareFaucetResponse
prepare
  PrepareFaucetRequest
    { pfrFirstLayer
    , pfrSecondLayer
    } = do
    phase1RootUtxos <- queryNonPacketUtxos
    phase2Value <- Reader.asks (getPacketValue . aeFaucetConfig)
    let phase1Value =
          CA.valueFromList (Tuple.second (* CA.Quantity pfrSecondLayer) <$> CA.valueToList phase2Value)
    (phase1TxBody, phase1Outputs) <- deployUtxoPhase pfrFirstLayer (phase1Value <> phase2Fee) phase1RootUtxos
    phase2Results <- traverse (deployUtxoPhase pfrSecondLayer phase2Value . pure) phase1Outputs
    faucetSigningKey <- Reader.asks aeOperatorSigningKey
    _ <-
      Async.async $ do
        _ <- runNodeIO $ Logger.runStdoutLoggingT $ signSubmitAndWaitTx Nothing faucetSigningKey phase1TxBody
        Async.forConcurrently_ phase2Results $ \(phase2TxBody, phase2Outputs) -> do
          _ <- runNodeIO $ Logger.runStdoutLoggingT $ signSubmitAndWaitTx Nothing faucetSigningKey phase2TxBody
          reloadFaucetUtxos (State.modify (<> (uiTxIn <$> phase2Outputs)))
    pure
      PrepareFaucetResponse
        { pfrFirstLayerTxId = CA.getTxId phase1TxBody
        , pfrSecondLayerTxIds = CA.getTxId . fst <$> phase2Results
        }

queryNonPacketUtxos :: AppM [UtxoInput]
queryNonPacketUtxos = do
  faucetAddress <-
    Reader.asks (getChangeAddress . CA.localNodeNetworkId . aeNodeConnection)
      <*> Reader.asks aeOperatorSigningKey
  packetValue <- Reader.asks (getPacketValue . aeFaucetConfig)
  (phase1RootUtxos, _) <- runNodeIO (queryUserInputsAndCollaterals faucetAddress)
  pure (filter (not . isPacketUtxo packetValue . uiTxOut) phase1RootUtxos)

constructUtxoInput :: CA.TxId -> CA.TxIx -> CA.TxOut CA.CtxTx CA.BabbageEra -> UtxoInput
constructUtxoInput txId txIx txOut =
  UtxoInput
    { uiTxIn = CA.TxIn txId txIx
    , uiTxOut = CA.toCtxUTxOTxOut txOut
    }

deployUtxoPhase :: Integer -> CA.Value -> [UtxoInput] -> AppM (CA.TxBody CA.BabbageEra, [UtxoInput])
deployUtxoPhase numUtxos valuePerUtxo inputs = do
  networkParams <- Reader.asks aeNetworkParams >>= MonadIO.liftIO . Concurrent.readMVar
  networkId <- Reader.asks (CA.localNodeNetworkId . aeNodeConnection)
  faucetSigningKey <- Reader.asks aeOperatorSigningKey
  let faucetAddress = getChangeAddress networkId faucetSigningKey
      utxoConstraints = Semigroup.stimesMonoid numUtxos (TB.mustPayTo faucetAddress valuePerUtxo CA.TxOutDatumNone)
      senderConstraints = Utils.toTxSenderConstraints inputs [] faucetAddress
      finalConstraints = utxoConstraints <> senderConstraints
  txBody@(CA.TxBody CA.TxBodyContent {CA.txOuts}) <- buildM finalConstraints networkParams (Just 1)
  let txId = CA.getTxId txBody
      outputs = List.zipWithFrom (constructUtxoInput txId) (CA.TxIx 0) (take (fromInteger numUtxos) txOuts)
  pure (txBody, outputs)

prepareApi :: ServerT PrepareFaucetApi AppM
prepareApi = prepare
