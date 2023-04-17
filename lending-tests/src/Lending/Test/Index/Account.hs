{-# LANGUAGE NamedFieldPuns #-}

module Lending.Test.Index.Account (indexAccountUpdated) where

import Control.Monad.IO.Class qualified as MonadIO
import Test.Tasty.HUnit qualified as Tasty

import Lending.Index.Account (Account (Account, accountOriginalRef, accountRef))
import Lending.Test.Common (getLatestStateExtract)
import Lending.Test.Env (IntegTest, TestEnv (TestEnv, dbConnection))

indexAccountUpdated :: IntegTest
indexAccountUpdated TestEnv {dbConnection} =
  getLatestStateExtract dbConnection [] $ \Account {accountRef, accountOriginalRef} ->
    MonadIO.liftIO $
      Tasty.assertBool "Account is not updated" (accountRef /= accountOriginalRef)
