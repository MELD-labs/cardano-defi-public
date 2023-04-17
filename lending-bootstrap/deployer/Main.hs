module Main (main) where

import Cardano.Api.Deployer (deploy)
import Lending.Scripts (LendingScript (AlwaysFalse))
import Service.Runner (runApp)

main :: IO ()
main = runApp (deploy AlwaysFalse)
