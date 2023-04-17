module Main (main) where

import Lending.Api.Handler (apiServer)
import Service.Runner (runApp)

main :: IO ()
main = runApp apiServer
