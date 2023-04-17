{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import LiquidationBot.Handler (closeAccount, getEnvAndRun, liquidationBotRunner)
import Service.Runner (runApp)

main :: IO ()
main = runApp $ getEnvAndRun $ liquidationBotRunner closeAccount
