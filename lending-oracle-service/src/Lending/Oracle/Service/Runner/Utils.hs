{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

module Lending.Oracle.Service.Runner.Utils
  ( buildUpdateOracleTxBody
  , getOracleServiceWalletInputs
  , queryOperatorWalletUtxos
  )
where

import Cardano.Api.Extra.NetworkParams (pparams)
import Cardano.Api.Shelley qualified as CA
import Control.Concurrent qualified as Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Map qualified as Map
import TxBuilder.Api
  ( UtxoInput (UtxoInput)
  , uiTxOut
  )
import TxBuilder.Api qualified as TB

import Lending.Core.Utils (queryUserInputsAndCollateralBaseOnSpendingValue)
import Lending.Core.Utils qualified as Utils
import Lending.Index.Query.Operator (getOperatorNftUtxo)
import Lending.Index.Query.Oracle (getOracleUtxo)
import Lending.Index.Query.ScriptDeployment (queryScriptDeployment)
import Lending.Oracle.Service.AppEnv
  ( AppEnv
      ( AppEnv
      , aeContractsConfig
      , aeNetworkParams
      , aeNodeConnection
      )
  , AppM
  , runSqlM
  )
import Lending.Oracle.Service.Config
  ( OracleServiceContractsConfig
      ( OracleServiceContractsConfig
      , occOraclePaymentScriptHash
      )
  )
import Lending.Oracle.Service.Transactions.UpdateOracle
  ( UpdateOracleInput (UpdateOracleInput)
  , updateOracleConstraints
  )
import Lending.Scripts (LendingScript (OperatorOracleNft))
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)
import Lending.Types.Oracle (OracleDatum (OracleDatum))

queryOperatorWalletUtxos :: AppM UtxoInput
queryOperatorWalletUtxos = runSqlM $ getOperatorNftUtxo OperatorOracleNft

getOracleServiceWalletInputs :: CA.Value -> AppM ([UtxoInput], [UtxoInput])
getOracleServiceWalletInputs spendingValue = do
  utxo@UtxoInput {uiTxOut = CA.TxOut walletAddress _ _ _} <- queryOperatorWalletUtxos
  (selectedUserInputs, collateralInputs) <-
    Reader.withReaderT aeNodeConnection $
      Reader.mapReaderT liftIO $
        queryUserInputsAndCollateralBaseOnSpendingValue walletAddress spendingValue
  pure (if utxo `elem` selectedUserInputs then selectedUserInputs else utxo : selectedUserInputs, collateralInputs)

-- TODO: estimate transaction fee
feeAmount :: CA.Quantity
feeAmount = 5_000_000

buildUpdateOracleTxBody :: Map.Map Asset Price -> AppM (CA.TxBody CA.BabbageEra)
buildUpdateOracleTxBody prices = do
  AppEnv
    { aeContractsConfig =
      OracleServiceContractsConfig
        { occOraclePaymentScriptHash
        }
    , aeNetworkParams
    } <-
    Reader.ask
  TB.UtxoInputWithDatum {TB.uiwdUtxo = oracleUtxoInput@UtxoInput {uiTxOut = oracleTxOut}} <- runSqlM getOracleUtxo
  oracleScript <- runSqlM $ queryScriptDeployment occOraclePaymentScriptHash
  let spendingValue = CA.valueFromList [(CA.AdaAssetId, feeAmount)]
  (walletUtxos@(UtxoInput {uiTxOut = CA.TxOut walletAddress _ _ _} : _), collateralUtxos) <-
    getOracleServiceWalletInputs spendingValue
  networkParams <- liftIO $ Concurrent.readMVar aeNetworkParams
  let newOracleDatum = OracleDatum prices
      oldOracleValue = TB.valueInTxOut oracleTxOut
      newOracleValue =
        Utils.calculateUtxoValueSatisfyMinAda
          (pparams networkParams)
          (Utils.utxoAddress oracleUtxoInput)
          oldOracleValue
          (TB.toTxOutInlineDatum newOracleDatum)
      constructUpdateOracleInput =
        UpdateOracleInput
          oracleUtxoInput
          walletUtxos
          oracleScript
          newOracleDatum
          newOracleValue
          collateralUtxos
          walletAddress
          (TB.pparams networkParams)
          spendingValue
  TB.buildM (updateOracleConstraints constructUpdateOracleInput) networkParams (Just 1)
