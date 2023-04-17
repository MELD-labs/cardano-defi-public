{-# LANGUAGE NamedFieldPuns #-}

module Lending.Test.Api.Pool
  ( getPoolApiSpec
  , testTreasuryPoolSpec
  , testMigratePoolSpec
  )
where

import Control.Monad qualified as Monad
import Data.Map qualified as Map

import Cardano.Api.Extra.Tx (signSubmitAndWaitTx)
import Lending.Api.Client
  ( migratePoolInputClient
  , queryPoolClient
  , updateTreasuryPoolClient
  )

import Lending.Api.Types.Pool
  ( UpdateTreasuryPoolRequest (UpdateTreasuryPoolRequest)
  , UpdateTreasuryPoolResponse (UpdateTreasuryPoolResponse, utprTx)
  )
import Lending.Test.Common
  ( queryApi
  )
import Lending.Test.Env
  ( IntegTest
  , TestEnv (TestEnv, apiClientEnv, nodeConnection, poolOperator, stakeKey)
  , TestStakingCredential (TestStakingCredential)
  , TestUserCredential (TestUserCredential, tucSigningKey)
  , adaAsset
  , runOnNodeConnectionPool
  )
import Lending.Types.Exchange (Actual)

getPoolApiSpec :: IntegTest
getPoolApiSpec TestEnv {apiClientEnv} = Monad.void $ queryApi apiClientEnv queryPoolClient

testTreasuryPoolSpec :: Actual -> IntegTest
testTreasuryPoolSpec
  amountAda
  TestEnv
    { nodeConnection
    , apiClientEnv
    , poolOperator = TestUserCredential {tucSigningKey}
    , stakeKey = TestStakingCredential _ stakePubKeyHash
    } = do
    UpdateTreasuryPoolResponse {utprTx} <-
      queryApi apiClientEnv $
        updateTreasuryPoolClient $
          UpdateTreasuryPoolRequest
            (Map.singleton adaAsset amountAda)
            (Just stakePubKeyHash)
    Monad.void $ runOnNodeConnectionPool nodeConnection $ signSubmitAndWaitTx Nothing tucSigningKey utprTx

testMigratePoolSpec :: IntegTest
testMigratePoolSpec
  TestEnv
    { apiClientEnv
    } = Monad.void $ queryApi apiClientEnv migratePoolInputClient
