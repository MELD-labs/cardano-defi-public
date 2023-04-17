{-# LANGUAGE NamedFieldPuns #-}

module Lending.Faucet.Status (faucetStatusApi) where

import Control.Concurrent qualified as Concurrent
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Reader qualified as Reader
import Servant (HasServer (ServerT))

import Lending.Faucet.Env (AppEnv (AppEnv, aeFaucetUtxo), AppM)
import Lending.Faucet.Types
  ( FaucetStatusApi
  , FaucetStatusResponse (FaucetStatusResponse, fsrMVarUtxoCount, fsrMVarUtxos)
  )

faucetStatusApi :: ServerT FaucetStatusApi AppM
faucetStatusApi = faucetStatusH

faucetStatusH :: AppM FaucetStatusResponse
faucetStatusH = do
  AppEnv {aeFaucetUtxo} <- Reader.ask
  listOfFaucetUtxo <- MonadIO.liftIO $ Concurrent.readMVar aeFaucetUtxo
  pure $
    FaucetStatusResponse
      { fsrMVarUtxoCount = length listOfFaucetUtxo
      , fsrMVarUtxos = listOfFaucetUtxo
      }
