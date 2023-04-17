{-# LANGUAGE NamedFieldPuns #-}

module Lending.Test.Faucet.MintToken (testMintToken, testFaucetStatus, testPrepareFaucet) where

import Control.Monad.IO.Class qualified as MonadIO
import Data.Foldable qualified as Foldable
import Test.Tasty.HUnit qualified as Tasty

import Cardano.Api.Extra.Wait (waitForTxIdConfirmation)
import Lending.Faucet.Types
  ( FaucetStatusResponse (FaucetStatusResponse, fsrMVarUtxoCount)
  , PrepareFaucetRequest (PrepareFaucetRequest, pfrFirstLayer, pfrSecondLayer)
  , PrepareFaucetResponse (PrepareFaucetResponse, pfrSecondLayerTxIds)
  )
import Lending.Test.Common (faucetStatusClient, prepareFaucetClient, queryApi, retry, retryOption)
import Lending.Test.Env
  ( IntegTest
  , SingleUserIntegTest
  , TestEnv (TestEnv, faucetClientEnv, nodeConnection)
  , runOnNodeConnectionPool
  )
import Lending.Test.InitAccount (mintTestTokens)

firstLayerCount :: Integer
firstLayerCount = 1

secondLayerCount :: Integer
secondLayerCount = 50

testMintToken :: SingleUserIntegTest
testMintToken = mintTestTokens

testFaucetStatus :: IntegTest
testFaucetStatus TestEnv {faucetClientEnv} =
  retry "Query faucet status" $ do
    FaucetStatusResponse {fsrMVarUtxoCount} <- queryApi faucetClientEnv faucetStatusClient
    MonadIO.liftIO $
      Tasty.assertEqual "Faucet status wrong" (fromInteger (firstLayerCount * secondLayerCount)) fsrMVarUtxoCount

testPrepareFaucet :: IntegTest
testPrepareFaucet TestEnv {faucetClientEnv, nodeConnection} = do
  PrepareFaucetResponse {pfrSecondLayerTxIds} <-
    queryApi faucetClientEnv $
      prepareFaucetClient
        PrepareFaucetRequest
          { pfrFirstLayer = firstLayerCount
          , pfrSecondLayer = secondLayerCount
          }
  runOnNodeConnectionPool nodeConnection $
    Foldable.traverse_ (flip (waitForTxIdConfirmation retryOption) 1) pfrSecondLayerTxIds
