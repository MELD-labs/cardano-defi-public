{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent qualified as Concurrent
import Control.Monad qualified as Monad
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Data.Text qualified as T

import Lending.Services.AppEnv (AppEnv (aePeriodCycle), AppM, runWithEnv)
import Lending.Services.Runner.Batching (runBatching)
import Lending.Services.Runner.GetBatchingConstraints (runGetBatchingInput)
import Service.Runner (runApp)

loop :: Int -> AppM () -> AppM ()
loop delay app =
  Monad.forever $
    Catch.handleAll (Logger.logErrorN . T.pack . Catch.displayException) app
      >> liftIO (Concurrent.threadDelay delay)

main :: IO ()
main = runApp $ runWithEnv serviceRunner

serviceRunner :: AppM ()
serviceRunner =
  Reader.asks aePeriodCycle
    >>= flip loop (runGetBatchingInput >>= runBatching) . (* 1_000_000)
