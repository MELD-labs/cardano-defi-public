module Lending.Services.Runner.Batching (runBatching) where

import Cardano.Api qualified as CA
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Data.Foldable qualified as Foldable
import Data.Text qualified as T

import Cardano.Api.Extra.Tx (signAndSubmitTx)
import Lending.Services.AppEnv
  ( AppEnv (aeNodeConnection, aeSigningKey)
  , AppM
  )

runBatching :: [CA.TxBody CA.BabbageEra] -> AppM ()
runBatching txBodies = do
  signingKey <- Reader.asks aeSigningKey
  Logger.logInfoN $
    T.pack $
      "Run Batching with " <> show (length txBodies) <> " transactions"
  Foldable.traverse_ (Reader.withReaderT aeNodeConnection . signAndSubmitTx signingKey) txBodies
