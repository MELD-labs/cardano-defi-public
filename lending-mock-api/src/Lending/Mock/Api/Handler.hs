{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Mock.Api.Handler (apiServer) where

import Control.Concurrent qualified as MVar
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Data.Map qualified as Map
import Data.Yaml qualified as Yaml
import Servant qualified as S
import Servant.Api.Handler (getTlsSettings, runAppServer)
import System.Environment qualified as Environment

import Lending.Core.Api (handleError)
import Lending.Mock.Api.AppEnv
  ( AppEnv (AppEnv, aeAssetsAndSlugs, aeAssetsPrices)
  , AppM
  , MockApiConfig (MockApiConfig, ocTokens)
  )
import Lending.Mock.Api.Handler.FeedPrice (feedPriceApi)
import Lending.Mock.Api.Handler.Quote (quoteApi)
import Lending.Oracle.Api.Types (LendingOracleApi)

oracleApiServer :: S.ServerT LendingOracleApi AppM
oracleApiServer = quoteApi S.:<|> feedPriceApi

apiServer :: IO ()
apiServer = do
  tls <- getTlsSettings
  assetPrices <- MVar.newMVar Map.empty
  MockApiConfig {ocTokens} <- Environment.getEnv "ORACLE_SERVICE_CONFIG_FILE" >>= Yaml.decodeFileThrow
  runAppServer @LendingOracleApi 3003 tls oracleApiServer $ \runner ->
    handleError $
      Logger.runStdoutLoggingT $
        Reader.runReaderT
          runner
          AppEnv
            { aeAssetsPrices = assetPrices
            , aeAssetsAndSlugs = ocTokens
            }
