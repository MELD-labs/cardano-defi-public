{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Lending.Test.LiquidationBot (liquidateAccountSpec) where

import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as Map
import Data.Pool qualified as Pool
import Test.Tasty.HUnit qualified as Tasty

import Cardano.Api.Extra.NetworkId (NetworkIdText (NetworkIdText))
import Lending.Api.Client (queryExceedLtvAccountsClient)
import Lending.Api.Types.Account
  ( ExceedLtvAccount (elaId)
  , QueryExceedLtvAccountsResponse (QueryExceedLtvAccountsResponse, qelarAccountList)
  )
import Lending.Test.Common (queryApi)
import Lending.Test.Env
  ( SingleUserIntegTest
  , TestEnv (TestEnv, apiClientEnv, dbConnection, nodeConnection)
  , TestUserCredential (TestUserCredential, tucAddress, tucSigningKey)
  , testNetwork
  )
import LiquidationBot.Env
  ( AppEnv (AppEnv)
  , LiquidatingCollateralConfig (PriorityOrder)
  , LiquidationBotConfig (LiquidationBotConfig)
  )
import LiquidationBot.Handler (clearSupply, getLiquidatorAccountId, liquidateAccount, runWithEnv)

liquidateAccountSpec :: SingleUserIntegTest
liquidateAccountSpec
  TestUserCredential {tucSigningKey, tucAddress}
  TestEnv {nodeConnection, apiClientEnv, dbConnection} = do
    connection <- liftIO $ Pool.withResource nodeConnection pure
    QueryExceedLtvAccountsResponse {qelarAccountList} <-
      queryApi apiClientEnv queryExceedLtvAccountsClient
    accId <- case qelarAccountList of
      account : _ -> pure $ elaId account
      _ -> liftIO (Tasty.assertFailure "No liquidatable account")
    let debt = Map.fromList [(1, 100_000)]
        collateral = PriorityOrder [0]
        botConfig =
          LiquidationBotConfig 10_000_000 accId 0.1 (NetworkIdText testNetwork) debt collateral 10
        mockEnv = AppEnv tucSigningKey tucAddress botConfig connection dbConnection
    liftIO $ runWithEnv mockEnv $ liquidateAccount apiClientEnv
    liquidatorAccountId <- liftIO $ runWithEnv mockEnv $ getLiquidatorAccountId accId
    liftIO $ runWithEnv mockEnv $ clearSupply apiClientEnv liquidatorAccountId
