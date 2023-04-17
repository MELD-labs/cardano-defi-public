module Main (main) where

import Lending.Api.Docs (apiServer)
import Service.Runner (runApp)

main :: IO ()
main = runApp apiServer
