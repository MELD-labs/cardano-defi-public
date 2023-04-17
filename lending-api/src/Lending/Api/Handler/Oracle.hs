{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Lending.Api.Handler.Oracle (oracleApi) where

import Cardano.Api qualified as CA
import Control.Concurrent qualified as Concurrent
import Control.Monad qualified as Monad
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader qualified as Reader
import Servant (ServerT)

import Lending.Api.Common (addressFromScriptHashes)
import Lending.Api.Config
  ( ContractsConfigApi
      ( ContractsConfigApi
      , ccaOperatorOracleNft
      , ccaOracleScriptHash
      )
  )
import Lending.Api.Env
  ( AppEnv (AppEnv, aeContractsConfig, aeNetworkParams, aeNodeConnection, aeNormalConnectionPool)
  , AppM
  , hoistScriptError
  , runSqlM
  )
import Lending.Api.Transactions.UpdateOracle
  ( UpdateOracleInput
      ( UpdateOracleInput
      , uoiOracleDatum
      , uoiOracleInput
      , uoiOracleOperatorInput
      , uoiOracleScriptAddress
      , uoiOracleScriptRefUtxo
      , uoiOracleValue
      )
  , updateOracleConstraints
  )
import Lending.Api.Types.Exception (UserError (TooManyUtxoInRequest))
import Lending.Api.Types.Oracle
  ( OracleApi
  , UpdateOracleRequest (UpdateOracleRequest, uorTokenPrices)
  , UpdateOracleResponse (UpdateOracleResponse)
  )
import Lending.Core.Utils (queryUserInputsAndCollateralBaseOnSpendingValue)
import Lending.Core.Utils qualified as Utils
import Lending.Index.Query.Operator (getOperatorNftUtxo)
import Lending.Index.Query.Oracle (getOracleUtxo)
import Lending.Index.Query.ScriptDeployment (queryScriptDeployment)
import Lending.Scripts (LendingScript (OperatorOracleNft, OperatorPoolNft))
import Lending.Types.Oracle (OracleDatum (OracleDatum))
import TxBuilder.Api
  ( NetworkParams (pparams)
  , UtxoInput (UtxoInput, uiTxOut)
  , UtxoInputWithDatum (UtxoInputWithDatum, uiwdUtxo)
  , buildM
  , toTxOutInlineDatum
  , valueInTxOut
  )

utxoCountLimit :: Int
utxoCountLimit = 200

oracleApi :: ServerT OracleApi AppM
oracleApi UpdateOracleRequest {uorTokenPrices} = do
  AppEnv
    { aeNodeConnection = CA.LocalNodeConnectInfo {CA.localNodeNetworkId = networkId}
    , aeNetworkParams
    , aeContractsConfig =
      ContractsConfigApi
        { ccaOracleScriptHash
        , ccaOperatorOracleNft
        }
    } <-
    Reader.ask

  networkParams <- liftIO $ Concurrent.readMVar aeNetworkParams

  UtxoInputWithDatum {uiwdUtxo = oracleUtxoInput@(UtxoInput _ oracleTxOutInput)} <-
    runSqlM getOracleUtxo aeNormalConnectionPool
  oracleOperatorInput <- runSqlM (getOperatorNftUtxo OperatorOracleNft) aeNormalConnectionPool

  oracleScriptRef <- runSqlM (queryScriptDeployment ccaOracleScriptHash) aeNormalConnectionPool
  let oracleScriptAddress = addressFromScriptHashes networkId ccaOracleScriptHash Nothing
      oldOracleValue = valueInTxOut oracleTxOutInput
      newOracleValue =
        Utils.calculateUtxoValueSatisfyMinAda
          (pparams networkParams)
          oracleScriptAddress
          oldOracleValue
          (toTxOutInlineDatum uorTokenPrices)
      estimatedTxFee = CA.valueFromList [(CA.AdaAssetId, 2_000_000)] -- Estimate trancsation fee
      userSpendingValue = estimatedTxFee <> newOracleValue <> CA.negateValue oldOracleValue
  UtxoInput {uiTxOut = CA.TxOut walletAddress _ _ _} <-
    runSqlM (getOperatorNftUtxo OperatorPoolNft) aeNormalConnectionPool
  (userUtxos, collateralUtxos) <-
    Reader.withReaderT aeNodeConnection $
      Reader.mapReaderT liftIO $
        queryUserInputsAndCollateralBaseOnSpendingValue
          walletAddress
          (userSpendingValue <> CA.valueFromList [(ccaOperatorOracleNft, 1)])

  Monad.when (length userUtxos > utxoCountLimit) $
    Catch.throwM (TooManyUtxoInRequest $ length userUtxos)

  let updateOracleInput =
        UpdateOracleInput
          { uoiOracleInput = oracleUtxoInput
          , uoiOracleOperatorInput = oracleOperatorInput
          , uoiOracleScriptRefUtxo = oracleScriptRef
          , uoiOracleDatum = OracleDatum uorTokenPrices
          , uoiOracleValue = newOracleValue
          , uoiOracleScriptAddress = oracleScriptAddress
          }

  oracleScriptConstraints <- hoistScriptError $ updateOracleConstraints updateOracleInput

  let finalConstraints =
        oracleScriptConstraints
          <> Utils.toTxSenderConstraints userUtxos collateralUtxos walletAddress

  UpdateOracleResponse <$> buildM finalConstraints networkParams (Just 1)
