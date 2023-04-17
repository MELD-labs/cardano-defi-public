{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

module Lending.Api.Handler.Pool (PoolApi, poolApi) where

import Cardano.Api qualified as CA
import Control.Applicative (liftA2)
import Control.Concurrent qualified as Concurrent
import Control.Monad qualified as Monad
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Either.Extra (eitherToMaybe)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Tagged (Tagged (unTagged))
import PlutusLedgerApi.V2 (StakingCredential (StakingHash))
import Servant (ServerT, (:<|>) ((:<|>)))

import Cardano.Api.Extra.Adapters
  ( fromCardanoAddressInEra
  , fromCardanoStakeCredential
  , stakeAddrToStakeCredential
  , toCardanoAddressInEra
  )
import Cardano.Index.Data.AddressText (AddressText (AddressText))
import Lending.Api.Common (getCurrentTime)
import Lending.Api.Config
  ( ContractsConfigApi
      ( ContractsConfigApi
      , ccaLendingFunctionInterestRate
      , ccaOperatorPoolNft
      , ccaPoolScriptHash
      )
  )
import Lending.Api.Env
  ( AppEnv (AppEnv, aeContractsConfig, aeNetworkParams, aeNodeConnection, aeNormalConnectionPool)
  , AppM
  , hoistScriptError
  , runSqlM
  )
import Lending.Api.Transactions.UpdateTreasuryPool
  ( UpdateTreasuryPoolInput
      ( UpdateTreasuryPoolInput
      , wpiPoolDatum
      , wpiPoolInput
      , wpiPoolScriptAddress
      , wpiPoolScriptRefUtxo
      , wpiPoolValue
      )
  , updateTreasuryPoolConstraints
  )
import Lending.Api.Types.Exception
  ( UserError (TooManyUtxoInRequest, UnableToDecodeAsset)
  )
import Lending.Api.Types.Pool
  ( MigratePoolApi
  , MigratePoolInput (MigratePoolInput, mpiPoolInput, mpiPoolScriptRefUtxo)
  , MigratePoolInputApi
  , MigratePoolInputResponse (MigratePoolInputResponse)
  , PoolApi
  , QueryPoolApi
  , QueryPoolResponse (QueryPoolResponse)
  , UpdateTreasuryPoolApi
  , UpdateTreasuryPoolRequest
    ( UpdateTreasuryPoolRequest
    , utprNewStakingCredential
    , utprUpdateRequest
    )
  , UpdateTreasuryPoolResponse (UpdateTreasuryPoolResponse)
  )
import Lending.Core.AccountValue (getDeltaTime, tryUpdateInterestRate)
import Lending.Core.Api (utxoCountLimit)
import Lending.Core.Utils (queryUserInputsAndCollateralBaseOnSpendingValue)
import Lending.Core.Utils qualified as Utils
import Lending.Index.Query.Manager (getManagerUtxo)
import Lending.Index.Query.Operator (getOperatorNftUtxo)
import Lending.Index.Query.Pool (getPoolUtxo)
import Lending.Index.Query.ScriptDeployment (queryScriptDeployment)
import Lending.Scripts (LendingScript (OperatorMigrationNft, OperatorPoolNft))
import Lending.Types
  ( ManagerDatum (ManagerDatum, mdRiskParameters)
  , PoolDatum (PoolDatum, pdAssets, pdLastUpdatedTime)
  , RiskParameters (rpAssetClassData)
  )
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual)
import Plutarch.Extra.AssetClass (fromPlutusAssetClass)
import Plutus.V1.Ledger.Address (Address (addressStakingCredential))
import TxBuilder.Api
  ( UtxoInput (UtxoInput)
  , UtxoInputWithDatum (UtxoInputWithDatum, uiwdDatum, uiwdUtxo)
  , buildM
  , mustReferenceInput
  , mustSpendFromWallet
  , valueInTxOut
  )
import TxBuilder.Api.Types (UtxoInput (uiTxOut))

poolApi :: ServerT PoolApi AppM
poolApi = queryPoolApi :<|> updateTreasuryPoolApi :<|> migratePoolApi

queryPoolApi :: ServerT QueryPoolApi AppM
queryPoolApi = queryPoolH

updateTreasuryPoolApi :: ServerT UpdateTreasuryPoolApi AppM
updateTreasuryPoolApi = updateTreasuryPoolH

migratePoolApi :: ServerT MigratePoolApi AppM
migratePoolApi = migratePoolInputApi

migratePoolInputApi :: ServerT MigratePoolInputApi AppM
migratePoolInputApi = migratePoolInputH

queryPoolH :: AppM QueryPoolResponse
queryPoolH = do
  AppEnv
    { aeContractsConfig = ContractsConfigApi {ccaLendingFunctionInterestRate}
    } <-
    Reader.ask
  UtxoInputWithDatum _ PoolDatum {pdAssets, pdLastUpdatedTime} <-
    runSqlM getPoolUtxo aeNormalConnectionPool
  UtxoInputWithDatum {uiwdDatum = ManagerDatum {mdRiskParameters}} <-
    runSqlM getManagerUtxo aeNormalConnectionPool
  curTime <- getCurrentTime
  currentParams <-
    tryUpdateInterestRate
      ccaLendingFunctionInterestRate
      mdRiskParameters
      pdAssets
      (getDeltaTime pdLastUpdatedTime curTime)
  return $ QueryPoolResponse (PoolDatum currentParams curTime)

updateTreasuryPoolH :: UpdateTreasuryPoolRequest -> AppM UpdateTreasuryPoolResponse
updateTreasuryPoolH
  UpdateTreasuryPoolRequest
    { utprUpdateRequest
    , utprNewStakingCredential
    } = do
    AppEnv
      { aeNodeConnection = CA.LocalNodeConnectInfo {CA.localNodeNetworkId = networkId}
      , aeNetworkParams
      , aeContractsConfig =
        ContractsConfigApi
          { ccaPoolScriptHash
          , ccaOperatorPoolNft
          }
      } <-
      Reader.ask

    networkParams <- liftIO $ Concurrent.readMVar aeNetworkParams

    UtxoInputWithDatum
      referenceManagerInput
      ManagerDatum {mdRiskParameters} <-
      runSqlM getManagerUtxo aeNormalConnectionPool

    UtxoInputWithDatum
      { uiwdUtxo = poolUtxoInput@(UtxoInput _ poolTxOutInput@(CA.TxOut poolAddress _ _ _))
      , uiwdDatum = poolDatum
      } <-
      runSqlM getPoolUtxo aeNormalConnectionPool
    poolOperatorInput@UtxoInput
      { uiTxOut = CA.TxOut poolOperatorAddress _ _ _
      } <-
      runSqlM (getOperatorNftUtxo OperatorPoolNft) aeNormalConnectionPool

    (withdrawTreasuryValue, supplyTreasuryValue) <-
      Map.foldlWithKey
        (\mResult -> (liftA2 appendPairValue mResult .) . valueFromWithdrawRequest mdRiskParameters)
        (pure (mempty, mempty))
        utprUpdateRequest

    poolScriptRef <- runSqlM (queryScriptDeployment ccaPoolScriptHash) aeNormalConnectionPool

    let plutusPoolAddress = fromCardanoAddressInEra poolAddress
        changePoolStakingCredential newStakeCre =
          eitherToMaybe $
            toCardanoAddressInEra networkId plutusPoolAddress {addressStakingCredential = Just newStakeCre}
        poolScriptAddress =
          fromMaybe
            poolAddress
            ( utprNewStakingCredential
                >>= changePoolStakingCredential . StakingHash . fromCardanoStakeCredential . stakeAddrToStakeCredential
            )
        oldPoolValue = valueInTxOut poolTxOutInput
        newPoolValue = oldPoolValue <> CA.negateValue withdrawTreasuryValue <> supplyTreasuryValue

        estimatedTxFee = CA.valueFromList [(CA.AdaAssetId, 2_000_000)] -- TODO: Estimate transaction fee
        userSpendingValue = estimatedTxFee

    (userUtxos, collateralUtxos) <-
      Reader.withReaderT aeNodeConnection $
        Reader.mapReaderT liftIO $
          queryUserInputsAndCollateralBaseOnSpendingValue
            poolOperatorAddress
            (userSpendingValue <> CA.valueFromList [(ccaOperatorPoolNft, 1)] <> supplyTreasuryValue)

    Monad.when (length userUtxos > utxoCountLimit) $
      Catch.throwM (TooManyUtxoInRequest $ length userUtxos)

    let userTotalValue = Utils.sumUtxoValue userUtxos
        updateTreasuryPoolInput =
          UpdateTreasuryPoolInput
            { wpiPoolInput = poolUtxoInput
            , wpiPoolScriptRefUtxo = poolScriptRef
            , wpiPoolDatum = poolDatum
            , wpiPoolValue = newPoolValue
            , wpiPoolScriptAddress = poolScriptAddress
            }

        spendOperatorInputConstraint =
          if CA.selectAsset userTotalValue ccaOperatorPoolNft == CA.Quantity 1
            then mempty
            else mustSpendFromWallet poolOperatorInput

    poolScriptConstraints <- hoistScriptError $ updateTreasuryPoolConstraints updateTreasuryPoolInput

    let finalConstraints =
          poolScriptConstraints
            <> spendOperatorInputConstraint
            <> Utils.toTxSenderConstraints userUtxos collateralUtxos poolOperatorAddress
            <> mustReferenceInput referenceManagerInput

    UpdateTreasuryPoolResponse <$> buildM finalConstraints networkParams (Just 1)

-- We consume the old pool as input, collateral, and utxos, including OperatorMigrationNft and fee.
migratePoolInputH :: AppM MigratePoolInputResponse
migratePoolInputH = do
  AppEnv
    { aeContractsConfig =
      ContractsConfigApi
        { ccaPoolScriptHash
        }
    } <-
    Reader.ask
  UtxoInputWithDatum {uiwdUtxo = poolUtxoInput@(UtxoInput _ poolTxOutInput), uiwdDatum = oldPoolDatum} <-
    runSqlM getPoolUtxo aeNormalConnectionPool
  poolScriptRef <- runSqlM (queryScriptDeployment ccaPoolScriptHash) aeNormalConnectionPool
  let oldPoolValue = valueInTxOut poolTxOutInput
      migratePoolInput =
        MigratePoolInput
          { mpiPoolInput = poolUtxoInput
          , mpiPoolScriptRefUtxo = poolScriptRef
          }
  UtxoInput {uiTxOut = CA.TxOut operatorMigrateAddress _ _ _} <-
    runSqlM (getOperatorNftUtxo OperatorMigrationNft) aeNormalConnectionPool
  return $ MigratePoolInputResponse migratePoolInput oldPoolDatum oldPoolValue (AddressText operatorMigrateAddress)

valueFromWithdrawRequest :: Map Asset RiskParameters -> Asset -> Actual -> AppM (CA.Value, CA.Value)
valueFromWithdrawRequest riskParams asset amount = do
  value <-
    maybe
      (Catch.throwM (UnableToDecodeAsset asset))
      (pure . CA.valueFromList . pure . (,CA.Quantity (unTagged amount)))
      (Map.lookup asset riskParams >>= fromPlutusAssetClass . rpAssetClassData)
  pure $ if amount >= 0 then (value, mempty) else (mempty, CA.negateValue value)

appendPairValue :: (CA.Value, CA.Value) -> (CA.Value, CA.Value) -> (CA.Value, CA.Value)
appendPairValue (v1, v2) (v3, v4) = (v1 <> v2, v3 <> v4)
