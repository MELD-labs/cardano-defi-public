{-# LANGUAGE NamedFieldPuns #-}

module Lending.Test.Api.SyncStatus (testSyncStatusApi) where

import Control.Monad.IO.Class qualified as MonadIO
import Test.Tasty.HUnit qualified as Tasty

import Data.Map (fromList)
import Lending.Api.Client (clientSyncStatus)
import Lending.Api.Types.SyncStatus
  ( AmountAsset (AmountAsset)
  , SyncStatusResponse (ssrAmountInProtocol, ssrAssetAnomaly)
  )
import Lending.Test.Common (queryApi)
import Lending.Test.Env (IntegTest, TestEnv (TestEnv, apiClientEnv))

testSyncStatusApi :: IntegTest
testSyncStatusApi
  TestEnv
    { apiClientEnv
    } = do
    response <-
      queryApi apiClientEnv clientSyncStatus
    MonadIO.liftIO $
      Tasty.assertBool
        "Sync Status wrong (supply or borrowing amount wrong)"
        ( ssrAmountInProtocol response
            == fromList
              [ (0, AmountAsset 1798751 199999 1798751 199999)
              , (1, AmountAsset 2000000 900000 2000000 900000)
              ]
            && null (ssrAssetAnomaly response)
        )
