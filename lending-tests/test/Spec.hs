{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad ((>=>))
import Data.Foldable qualified as Foldable
import System.Environment qualified as Environment
import Test.Tasty qualified as Tasty

import Lending.Test.Functional (functionalTests)
import Lending.Test.Integ (integTests)

main :: IO ()
main = do
  functionalTestPath <- Foldable.find (not . null) <$> Environment.lookupEnv "FUNCTIONAL_TEST"
  maybe
    (Tasty.defaultMain integTests)
    (functionalTests >=> Tasty.defaultMain)
    functionalTestPath
