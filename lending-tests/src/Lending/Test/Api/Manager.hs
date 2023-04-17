{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Test.Api.Manager (testUpdateManagerSpec, getManagerApiSpec, testMigrateManagerSpec) where

import Control.Monad qualified as Monad
import Data.Map qualified as Map
import Data.Tagged (Tagged (Tagged))
import Servant.Client (ClientM)

import Cardano.Api.Extra.Tx (signSubmitAndWaitTx)
import Lending.Api.Client (migrateManagerClient, queryManagerClient, updateManagerClient)
import Lending.Api.Types.Manager
  ( UpdateManagerResponse (UpdateManagerResponse, umTx)
  )
import Lending.Index.Query.Manager (getManagerUtxo)
import Lending.Test.Common
  ( queryApi
  , retry
  )
import Lending.Test.Env
  ( IntegTest
  , TestEnv (TestEnv, apiClientEnv, dbConnection, managerOperator, migrationOperator, nodeConnection)
  , TestUserCredential (tucSigningKey)
  , adaAsset
  , runOnNodeConnectionPool
  , runSqlM
  )
import Lending.Types.Manager
  ( ManagerDatum (ManagerDatum, mdRiskParameters, mdRiskParamsOperatorNft, mdTreasuryOperatorNft)
  , RiskParameters (RiskParameters)
  )
import Lending.Types.Percent (percent)
import Plutarch.Extra.AssetClass (AssetClass (AssetClass))
import TxBuilder.Api (UtxoInputWithDatum (UtxoInputWithDatum))

testModifyingManager
  :: (TestEnv -> TestUserCredential)
  -> (ManagerDatum -> ClientM UpdateManagerResponse)
  -> (ManagerDatum -> ManagerDatum)
  -> IntegTest
testModifyingManager
  getUser
  api
  modify
  env@TestEnv
    { nodeConnection
    , apiClientEnv
    , dbConnection
    } = do
    UtxoInputWithDatum _ managerDatum <- retry "Getting Manager Utxo" $ runSqlM getManagerUtxo dbConnection
    UpdateManagerResponse {umTx} <-
      queryApi apiClientEnv $
        api (modify managerDatum)
    -- Submit tx and confirm tx in onchain
    Monad.void $ runOnNodeConnectionPool nodeConnection $ signSubmitAndWaitTx Nothing (tucSigningKey $ getUser env) umTx

getManagerApiSpec :: IntegTest
getManagerApiSpec TestEnv {apiClientEnv} = Monad.void $ queryApi apiClientEnv queryManagerClient

testUpdateManagerSpec :: IntegTest
testUpdateManagerSpec = do
  let riskParams =
        RiskParameters
          (Tagged (percent 75))
          (Tagged (percent 85))
          100_000_000_000
          600_000_000
          (AssetClass "" "")
          (percent 5)
          (percent 45)
          0
          (percent 4)
          (percent 300)
  testModifyingManager managerOperator updateManagerClient $ \oldDatum@ManagerDatum {mdRiskParameters} ->
    oldDatum {mdRiskParameters = Map.insert adaAsset riskParams mdRiskParameters}

testMigrateManagerSpec :: IntegTest
testMigrateManagerSpec =
  testModifyingManager
    migrationOperator
    migrateManagerClient
    $ \oldDatum@ManagerDatum {mdRiskParamsOperatorNft, mdTreasuryOperatorNft} ->
      oldDatum
        { mdRiskParamsOperatorNft = mdTreasuryOperatorNft
        , mdTreasuryOperatorNft = mdRiskParamsOperatorNft
        }
