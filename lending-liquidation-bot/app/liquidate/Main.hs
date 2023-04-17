{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Monad (void)

import LiquidationBot.Handler (getEnvAndRun, liquidateAccount, liquidationBotRunner)
import Service.Runner (runApp)

main :: IO ()
main = runApp $ void $ getEnvAndRun $ liquidationBotRunner liquidateAccount
