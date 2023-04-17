{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

import Spec.Account qualified as Account
import Spec.AccountAuthToken qualified as AAT
import Spec.InterestRate qualified as IR
import Spec.Manager qualified as PP
import Spec.Map qualified as Map
import Spec.Nft qualified as Nft
import Spec.Pool qualified as Pool

main :: IO ()
main = do
  defaultMain tests

-- | Number of successful tests for each hedgehog property.
--
-- The default is 100 but we use a smaller number here in order to speed up
-- the test suite.
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

tests :: TestTree
tests =
  localOption limit $
    testGroup
      "Contracts"
      [PP.specs, Nft.specs, AAT.specs, Account.specs, Pool.specs, Map.specs, IR.specs]
