{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Lending.Api.Handler.Manager (managerApi, queryManagerApi, updateManagerApi) where

import Cardano.Api qualified as CA
import Control.Concurrent qualified as Concurrent
import Control.Monad qualified as Monad
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Trans.Reader qualified as Reader
import Servant (ServerT)
import Servant qualified as S

import Lending.Api.Common (addressFromScriptHashes)
import Lending.Api.Config
  ( ContractsConfigApi (ContractsConfigApi, ccaManagerScriptHash, ccaOperatorManagerNft, ccaOperatorMigrationNft)
  )
import Lending.Api.Env
  ( AppEnv (AppEnv, aeContractsConfig, aeNetworkParams, aeNodeConnection, aeNormalConnectionPool)
  , AppM
  , runSqlM
  )
import Lending.Api.Types.Exception (UserError (TooManyUtxoInRequest))
import Lending.Api.Types.Manager
  ( ManagerApi
  , MigrateManagerApi
  , QueryManagerApi
  , QueryManagerResponse (QueryManagerResponse)
  , UpdateManagerApi
  , UpdateManagerResponse (UpdateManagerResponse)
  )
import Lending.Core.Api (utxoCountLimit)
import Lending.Core.Utils qualified as Utils
import Lending.Index.Query.Manager (getManagerUtxo)
import Lending.Index.Query.Operator (getOperatorNftUtxo)
import Lending.Index.Query.ScriptDeployment (queryScriptDeployment)
import Lending.Scripts (LendingScript (OperatorManagerNft, OperatorMigrationNft))
import Lending.Types.Manager (ManagerDatum, ManagerRedeemer (UpdateParamsRedeemer))
import TxBuilder.Api (UtxoInputWithDatum (UtxoInputWithDatum), buildM)
import TxBuilder.Api qualified as TB
import TxBuilder.Api.Types (NetworkParams (pparams), UtxoInput (UtxoInput, uiTxOut))

managerApi :: ServerT ManagerApi AppM
managerApi = updateManagerApi S.:<|> queryManagerApi S.:<|> migrateManagerApi

updateManagerApi :: ServerT UpdateManagerApi AppM
updateManagerApi = updateManagerApiH OperatorManagerNft ccaOperatorManagerNft

migrateManagerApi :: ServerT MigrateManagerApi AppM
migrateManagerApi = updateManagerApiH OperatorMigrationNft ccaOperatorMigrationNft

updateManagerApiH :: LendingScript -> (ContractsConfigApi -> CA.AssetId) -> ManagerDatum -> AppM UpdateManagerResponse
updateManagerApiH script getOperator managerDatum = do
  AppEnv
    { aeNodeConnection = CA.LocalNodeConnectInfo {CA.localNodeNetworkId = networkId}
    , aeNetworkParams
    , aeContractsConfig = contractsConfig@ContractsConfigApi {ccaManagerScriptHash}
    } <-
    Reader.ask
  networkParams <- MonadIO.liftIO $ Concurrent.readMVar aeNetworkParams
  UtxoInputWithDatum managerInput _ <- runSqlM getManagerUtxo aeNormalConnectionPool
  managerDeploymentUtxo <- runSqlM (queryScriptDeployment ccaManagerScriptHash) aeNormalConnectionPool
  let managerAddress = addressFromScriptHashes networkId ccaManagerScriptHash Nothing
      estimatedUserTxFeeValue = CA.lovelaceToValue 2_000_000 -- TODO: Calculate tx fee
      managerValue = TB.valueInTxOut (uiTxOut managerInput)
      newManagerValue =
        Utils.calculateUtxoValueSatisfyMinAda
          (pparams networkParams)
          managerAddress
          managerValue
          (TB.toTxOutInlineDatum managerDatum)
      userSpendingValue = estimatedUserTxFeeValue <> newManagerValue <> CA.negateValue managerValue

  UtxoInput {uiTxOut = CA.TxOut userAddress _ _ _} <-
    runSqlM (getOperatorNftUtxo script) aeNormalConnectionPool

  (userInputs, collateralUtxos) <-
    Reader.withReaderT aeNodeConnection $
      Reader.mapReaderT MonadIO.liftIO $
        Utils.queryUserInputsAndCollateralBaseOnSpendingValue
          userAddress
          (userSpendingValue <> CA.valueFromList [(getOperator contractsConfig, 1)])

  Monad.when (length userInputs > utxoCountLimit) $
    Catch.throwM (TooManyUtxoInRequest $ length userInputs)

  let userSpendConstraint = Utils.toTxSenderConstraints userInputs collateralUtxos userAddress
      scriptSpendConstraint =
        TB.mustSpendFromScript
          managerInput
          (TB.RefScript CA.PlutusScriptV2 managerDeploymentUtxo)
          CA.InlineScriptDatum
          UpdateParamsRedeemer
      payToScriptConstraint = TB.mustPayTo managerAddress newManagerValue (TB.toTxOutInlineDatum managerDatum)

  let finalConstraints =
        userSpendConstraint
          <> scriptSpendConstraint
          <> payToScriptConstraint

  UpdateManagerResponse <$> buildM finalConstraints networkParams (Just 1)

queryManagerApi :: ServerT QueryManagerApi AppM
queryManagerApi = do
  UtxoInputWithDatum {TB.uiwdDatum = currentParams} <- runSqlM getManagerUtxo aeNormalConnectionPool
  return $ QueryManagerResponse currentParams
