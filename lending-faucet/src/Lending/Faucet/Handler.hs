{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Main api module tying things together.
module Lending.Faucet.Handler (apiServer) where

import Control.Concurrent qualified as Concurrent
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Data.Yaml qualified as Yaml
import Servant qualified as S
import System.Environment (getEnv)

import Cardano.Api.Extra.Key (readPaymentSigningKey)
import Cardano.Api.Extra.NetworkId (NetworkIdText (NetworkIdText))
import Cardano.Api.Extra.NetworkParams qualified as CAE
import Cardano.Api.Extra.Node qualified as CAE
import Lending.Core.Api (handleError)
import Lending.Faucet.Common (queryPacketUtxos)
import Lending.Faucet.Env
  ( AppEnv
      ( AppEnv
      , aeFaucetConfig
      , aeFaucetUtxo
      , aeNetworkParams
      , aeNodeConnection
      , aeOperatorSigningKey
      )
  , AppM
  , FaucetConfig
    ( FaucetConfig
    , fcNetworkId
    , fcNetworkParamsFetchDelay
    )
  )
import Lending.Faucet.MintToken (mintTokenApi)
import Lending.Faucet.Preparation (prepareApi)
import Lending.Faucet.Status (faucetStatusApi)
import Lending.Faucet.Types (FaucetApi, FaucetApiWithHealthcheck, HealthcheckApi)
import Servant.Api.Handler (getTlsSettings, runAppServer)

-- | All endpoints the api provides
faucetApiServer :: S.ServerT FaucetApi AppM
faucetApiServer = mintTokenApi S.:<|> faucetStatusApi S.:<|> prepareApi

healthcheck :: S.ServerT HealthcheckApi AppM
healthcheck = pure S.NoContent

-- | Faucet server endpoints together with the documentation
faucetServer :: S.ServerT FaucetApiWithHealthcheck AppM
faucetServer = healthcheck S.:<|> faucetApiServer

-- | Read all env vars and run the 'AppM' action using constructed environment
apiServer :: IO ()
apiServer = do
  tls <- getTlsSettings
  cardanoNodeSocketPath <- getEnv "CARDANO_NODE_SOCKET_PATH"
  faucetConfig@FaucetConfig
    { fcNetworkId = NetworkIdText cardanoNetworkId
    , fcNetworkParamsFetchDelay
    } <-
    getEnv "FAUCET_CONFIG_FILE" >>= Yaml.decodeFileThrow
  operatorSigningKey <- readPaymentSigningKey "FAUCET_SIGNING_KEY"
  let nodeConnection = CAE.mkNodeConnectInfo cardanoNetworkId cardanoNodeSocketPath
  faucetUtxoMvar <- queryPacketUtxos operatorSigningKey faucetConfig nodeConnection >>= Concurrent.newMVar
  networkParamsRef <- liftIO $ CAE.fetchNetworkParamsPeriodically nodeConnection fcNetworkParamsFetchDelay
  let env =
        AppEnv
          { aeNodeConnection = nodeConnection
          , aeNetworkParams = networkParamsRef
          , aeOperatorSigningKey = operatorSigningKey
          , aeFaucetUtxo = faucetUtxoMvar
          , aeFaucetConfig = faucetConfig
          }
  runAppServer @FaucetApiWithHealthcheck 3001 tls faucetServer $
    handleError . Logger.runStdoutLoggingT . flip Reader.runReaderT env
