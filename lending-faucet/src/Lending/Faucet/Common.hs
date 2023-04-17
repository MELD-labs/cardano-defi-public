{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Faucet.Common
  ( toValue
  , getPacketValue
  , reloadFaucetUtxos
  , queryPacketUtxos
  , isPacketUtxo
  )
where

import Cardano.Api qualified as CA
import Control.Arrow ((***))
import Control.Concurrent qualified as Concurrent
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger qualified as Logger
import Control.Monad.Reader qualified as Reader
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.State qualified as State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import Cardano.Api.Extra.AssetId (AssetIdText (unAssetIdText))
import Cardano.Api.Extra.Query (executeShelleyQuery)
import Lending.Core.Utils (getChangeAddress)
import Lending.Faucet.Env
  ( AppEnv (aeFaucetUtxo)
  , AppM
  , FaucetConfig (FaucetConfig, fcCollateral, fcFeeAndChange, fcSpent)
  )
import Lending.Faucet.Exceptions (FaucetException (NotEnoughUtxoForUserRequest))
import Plutarch.Extra.FixedDecimal (FixedDecimal, fixedNumerator)

toValue :: Map AssetIdText (FixedDecimal 6) -> [(CA.AssetId, CA.Quantity)]
toValue = ((unAssetIdText *** CA.Quantity . fixedNumerator) <$>) . Map.toList

getPacketValue :: FaucetConfig -> CA.Value
getPacketValue FaucetConfig {fcSpent, fcCollateral, fcFeeAndChange} =
  CA.valueFromList (toValue fcSpent)
    <> CA.lovelaceToValue (CA.Lovelace (fixedNumerator (fcCollateral + fcFeeAndChange)))

reloadFaucetUtxos :: StateT [CA.TxIn] Maybe a -> AppM a
reloadFaucetUtxos onOutput = do
  mVar <- Reader.asks aeFaucetUtxo
  currentState <- MonadIO.liftIO (Concurrent.takeMVar mVar)
  let onFailure :: AppM a
      onFailure = MonadIO.liftIO (Concurrent.putMVar mVar currentState) >> Catch.throwM NotEnoughUtxoForUserRequest
      onSuccess (output, newState) = do
        Logger.logInfoN $ "Faucet has " <> Text.pack (show (length newState)) <> " TxIn in MVar"
        MonadIO.liftIO (Concurrent.putMVar mVar newState)
        pure output
  maybe onFailure onSuccess (State.runStateT onOutput currentState)

isPacketUtxo :: CA.Value -> CA.TxOut CA.CtxUTxO CA.BabbageEra -> Bool
isPacketUtxo packetValue (CA.TxOut _ txOutValue _ _) = CA.txOutValueToValue txOutValue == packetValue

queryPacketUtxos
  :: CA.SigningKey CA.PaymentExtendedKey -> FaucetConfig -> CA.LocalNodeConnectInfo CA.CardanoMode -> IO [CA.TxIn]
queryPacketUtxos signingKey faucetConfig nodeConnection = do
  let packetValue = getPacketValue faucetConfig
      getAddressAny (CA.AddressInEra _ addr) = CA.toAddressAny addr
      faucetAddress = getAddressAny (getChangeAddress (CA.localNodeNetworkId nodeConnection) signingKey)
      utxoQuery = CA.QueryUTxO (CA.QueryUTxOByAddress (Set.singleton faucetAddress))
  utxos <- Reader.runReaderT (executeShelleyQuery @CA.BabbageEra utxoQuery) nodeConnection
  pure (Map.keys (Map.filter (isPacketUtxo packetValue) (CA.unUTxO utxos)))
