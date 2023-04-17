{-# LANGUAGE OverloadedStrings #-}

module Lending.Oracle.Service.Runner (runUpdating) where

import Cardano.Api qualified as CA
import Control.Monad qualified as Monad
import Control.Monad.Trans.Reader qualified as Reader

import Cardano.Api.Extra.Tx (signAndSubmitTx)
import Control.Monad.Logger qualified as Logger
import Lending.Oracle.Service.AppEnv
  ( AppEnv (aeNodeConnection, aeSigningKey)
  , AppM
  )

runUpdating :: CA.TxBody CA.BabbageEra -> AppM ()
runUpdating txBody = do
  Logger.logInfoN "Sign and submit transaction update oracle"
  signingKey <- Reader.asks aeSigningKey
  Monad.void $ Reader.withReaderT aeNodeConnection $ signAndSubmitTx signingKey txBody
