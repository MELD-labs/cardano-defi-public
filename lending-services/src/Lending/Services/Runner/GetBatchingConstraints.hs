{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Services.Runner.GetBatchingConstraints (runGetBatchingInput) where

import Cardano.Api.Shelley qualified as CA
import Cardano.Slotting.Time qualified as Slotting
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent qualified as Concurrent
import Control.Exception (SomeException)
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger qualified as Logger
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Lazy qualified as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT)
import Data.Bifunctor (bimap)
import Data.Foldable qualified as Foldable
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import PlutusLedgerApi.V2 (POSIXTime (getPOSIXTime))

import Cardano.Api.Extra.Query qualified as CAE
import Cardano.Api.Extra.Time (getSlotTime)
import Lending.Core.Errors
  ( eitherE
  , maybeE
  )
import Lending.Core.Utils (fromTxOutInlineDatum, posixTimeFromUTCTime, toSlotNo)
import Lending.Index.Query.Manager (getManagerUtxo)
import Lending.Index.Query.Oracle (getOracleUtxo)
import Lending.Index.Query.Pool (getPoolUtxo)
import Lending.Index.Query.ScriptDeployment (queryScriptDeployment)
import Lending.Services.AppEnv
  ( AppEnv
      ( AppEnv
      , aeContractsConfig
      , aeMinRequestsPerAttemptedTx
      , aeMinRequestsPerTx
      , aeNetworkParams
      , aeNodeConnection
      )
  , AppM
  , runSqlM
  )
import Lending.Services.Config
  ( ServiceContractsConfig
      ( ServiceContractsConfig
      , sccAccountPaymentScriptHash
      , sccPoolPaymentScriptHash
      )
  )
import Lending.Services.Errors
  ( BatchingServiceError
      ( BatchingServiceConvertTimeError
      , BatchingServiceParseOracleCheckerError
      , BatchingServiceParsePoolDatumError
      )
  )
import Lending.Services.Runner.Utils
  ( getServiceWalletUtxos
  , queryAllAccountInputs
  )
import Lending.Services.Transactions.Batcher (batchingConstraints)
import Lending.Services.Transactions.Types
  ( BatchingInput
      ( BatchingInput
      , biAccountInputs
      , biBatcherChangeAddress
      , biBatcherWalletUtxos
      , biManagerInput
      , biPoolInput
      )
  )
import Lending.Types.Account
  ( AccountDatum (AccountDatum, adClearRequests, adCollateralUpdate, adNormalRequests, adUserNft)
  )
import Lending.Types.Manager (ManagerDatum (ManagerDatum, mdMaxValidityDuration, mdOracleCheckerToken, mdPoolNft))
import Plutarch.Extra.AssetClass (fromPlutusAssetClass, renderAssetClass)
import TxBuilder.Api
  ( BuildConstraints (BuildConstraints, tpCollateral, tpInputs, tpRefInputs)
  , NetworkParams (NetworkParams, eraHis, pparams, sysStart)
  , UtxoInput (UtxoInput, uiTxIn, uiTxOut)
  , UtxoInputWithDatum (UtxoInputWithDatum, uiwdDatum)
  )
import TxBuilder.Api qualified as TB

runGetBatchingInput :: AppM [CA.TxBody CA.BabbageEra]
runGetBatchingInput = do
  AppEnv
    { aeContractsConfig =
      ServiceContractsConfig
        { sccAccountPaymentScriptHash
        , sccPoolPaymentScriptHash
        }
    , aeNodeConnection
    } <-
    Reader.ask
  Logger.logInfoN "Query data from database to build batching transaction."
  accountInputs <- queryAllAccountInputs
  managerInput@UtxoInputWithDatum {uiwdDatum = managerDatum} <- runSqlM getManagerUtxo
  oracleInput <- runSqlM getOracleUtxo
  poolInput <- runSqlM getPoolUtxo
  accountScript <- runSqlM $ queryScriptDeployment sccAccountPaymentScriptHash
  poolScript <- runSqlM $ queryScriptDeployment sccPoolPaymentScriptHash
  Logger.logInfoN "Query batcher wallet's utxos"
  (walletUtxos@(UtxoInput {uiTxOut = CA.TxOut walletAddress _ _ _} : _), collateralUtxos) <-
    getServiceWalletUtxos (mdOracleCheckerToken managerDatum)
  Logger.logInfoN "Query slot time from node while building batching transaction"
  CA.SlotNo slotNo <- getCurrentSlotNo
  slotToUTCTime <- liftIO $ Reader.runReaderT getSlotTime aeNodeConnection
  (currentPOSIXTime, slotLength) <-
    eitherE
      BatchingServiceConvertTimeError
      (bimap posixTimeFromUTCTime Slotting.slotLengthToMillisec <$> slotToUTCTime (CA.SlotNo slotNo))
  let ManagerDatum {mdMaxValidityDuration, mdOracleCheckerToken} = managerDatum
      expiredPeriod = fromInteger @Word64 (getPOSIXTime mdMaxValidityDuration `div` slotLength)
      validateRange =
        ( CA.TxValidityLowerBound CA.ValidityLowerBoundInBabbageEra (CA.SlotNo slotNo)
        , CA.TxValidityUpperBound CA.ValidityUpperBoundInBabbageEra (CA.SlotNo $ slotNo + expiredPeriod)
        )
  CA.AssetId oracleCheckerPolicyId _ <-
    maybeE (BatchingServiceParseOracleCheckerError mdOracleCheckerToken) (fromPlutusAssetClass mdOracleCheckerToken)
  oracleCheckerScript <- runSqlM $ queryScriptDeployment (CA.unPolicyId oracleCheckerPolicyId)
  let initialInput =
        BatchingInput
          managerInput
          oracleInput
          poolInput
          []
          poolScript
          accountScript
          oracleCheckerScript
          walletUtxos
          collateralUtxos
          walletAddress
          validateRange
          currentPOSIXTime
  processAll initialInput accountInputs
  where
    getCurrentSlotNo :: AppM CA.SlotNo
    getCurrentSlotNo =
      Reader.withReaderT aeNodeConnection $
        Reader.mapReaderT lift $
          toSlotNo <$> CAE.executeQueryInMode (CA.QueryChainPoint CA.CardanoMode)

data BatchingState = BatchingState
  { bsInput :: BatchingInput
  , bsBuiltTx :: Maybe (CA.TxBody CA.BabbageEra)
  }

newtype ExUnitWrapper = ExUnitWrapper CA.ExecutionUnits
  deriving stock (Eq, Show)

instance Semigroup ExUnitWrapper where
  ExUnitWrapper (CA.ExecutionUnits step1 mem1) <> ExUnitWrapper (CA.ExecutionUnits step2 mem2) =
    ExUnitWrapper (CA.ExecutionUnits (step1 + step2) (mem1 + mem2))

instance Monoid ExUnitWrapper where
  mempty = ExUnitWrapper (CA.ExecutionUnits 0 0)

isExceeding :: ExUnitWrapper -> CA.ExecutionUnits -> Bool
isExceeding (ExUnitWrapper (CA.ExecutionUnits txStep txMem)) (CA.ExecutionUnits stepLimit memLimit) =
  txStep > stepLimit || txMem > memLimit

tryAccount :: UtxoInputWithDatum AccountDatum -> BatchingInput -> BatchingInput
tryAccount account input@BatchingInput {biAccountInputs} = input {biAccountInputs = account : biAccountInputs}

addAccount :: UtxoInputWithDatum AccountDatum -> Maybe (CA.TxBody CA.BabbageEra) -> BatchingState -> BatchingState
addAccount account txBody BatchingState {bsInput, bsBuiltTx} =
  BatchingState (tryAccount account bsInput) (txBody <|> bsBuiltTx)

isExUnitExceeded :: CA.TxBody CA.BabbageEra -> CA.UTxO CA.BabbageEra -> AppM Bool
isExUnitExceeded txBody utxos = do
  NetworkParams {pparams, eraHis, sysStart} <- Reader.asks aeNetworkParams >>= liftIO . Concurrent.readMVar
  exUnitMap <-
    either
      (liftIO . CA.throwErrorAsException)
      (traverse (either (liftIO . CA.throwErrorAsException) (pure . ExUnitWrapper)))
      (CA.evaluateTransactionExecutionUnits CA.BabbageEraInCardanoMode sysStart eraHis pparams utxos txBody)
  let totalExUnit = Foldable.fold exUnitMap
  pure (all (isExceeding totalExUnit) (CA.protocolParamMaxTxExUnits pparams))

recordTx
  :: UtxoInputWithDatum AccountDatum
  -> CA.TxBody CA.BabbageEra
  -> StateT BatchingState AppM (CA.TxBody CA.BabbageEra)
recordTx
  account
  txBody@(CA.TxBody txBodyContent) = do
    oldInput@BatchingInput
      { biManagerInput = UtxoInputWithDatum {uiwdDatum = ManagerDatum {mdPoolNft}}
      , biBatcherWalletUtxos
      , biBatcherChangeAddress
      } <-
      State.gets bsInput
    State.lift (logBatch oldInput)
    let txId = CA.getTxId txBody
        buildUtxoInput idx = UtxoInput (CA.TxIn txId (CA.TxIx idx)) . CA.toCtxUTxOTxOut
        newOutputs = List.zipWithFrom buildUtxoInput 0 (CA.txOuts txBodyContent)
        spentInputs = fst <$> CA.txIns txBodyContent
        -- Get new wallet UTXOs
        isWalletUtxo (CA.TxOut address _ _ _) = address == biBatcherChangeAddress
        remainingWalletUtxos = filter ((`notElem` spentInputs) . uiTxIn) biBatcherWalletUtxos
        newWalletUtxos = filter (isWalletUtxo . uiTxOut) newOutputs
        -- Get new Pool
        isPoolUtxo (CA.TxOut _ txOutValue _ _) =
          (CA.selectAsset (CA.txOutValueToValue txOutValue) <$> fromPlutusAssetClass mdPoolNft) == Just 1
        toUtxoInputWithDatum utxoInput@UtxoInput {uiTxOut = CA.TxOut _ _ datum _} =
          UtxoInputWithDatum utxoInput <$> fromTxOutInlineDatum datum
    newPoolUtxo <-
      maybeE
        (BatchingServiceParsePoolDatumError newOutputs)
        (List.find (isPoolUtxo . uiTxOut) newOutputs >>= toUtxoInputWithDatum)
    State.put $
      BatchingState
        { bsInput =
            oldInput
              { biPoolInput = newPoolUtxo
              , biAccountInputs = [account]
              , biBatcherWalletUtxos = remainingWalletUtxos <> newWalletUtxos
              }
        , bsBuiltTx = Nothing
        }
    pure txBody

buildTx :: BuildConstraints -> AppM (CA.TxBody CA.BabbageEra, CA.UTxO CA.BabbageEra)
buildTx constraints@BuildConstraints {tpInputs, tpRefInputs, tpCollateral} = do
  networkParams <- Reader.asks aeNetworkParams >>= liftIO . Concurrent.readMVar
  txBody <- TB.buildM constraints networkParams (Just 1)
  let utxos =
        CA.UTxO $
          foldMap
            (uncurry Map.singleton . (uiTxIn &&& uiTxOut))
            ((fst <$> tpInputs) <> tpRefInputs <> tpCollateral)
  pure (txBody, utxos)

makeDecision :: UtxoInputWithDatum AccountDatum -> StateT BatchingState AppM (Maybe (CA.TxBody CA.BabbageEra))
makeDecision account@UtxoInputWithDatum {uiwdDatum = AccountDatum {adUserNft}} = do
  minSize <- Reader.asks aeMinRequestsPerAttemptedTx
  newInput <- State.gets (tryAccount account . bsInput)
  State.lift $ Logger.logInfoN $ "Trying account with owner NFT: " <> renderAssetClass adUserNft
  constraints <- State.lift (batchingConstraints newInput)
  State.lift $ Logger.logInfoN $ "Setting account with owner NFT: " <> renderAssetClass adUserNft <> " to be processed"
  (txBody, shouldSubmit) <-
    if getSize newInput >= minSize
      then State.lift $ do
        (txBody, utxos) <- buildTx constraints
        (Just txBody,) <$> isExUnitExceeded txBody utxos
      else pure (Nothing, False)
  if shouldSubmit
    then State.gets bsBuiltTx >>= traverse (recordTx account)
    else State.modify (addAccount account txBody) >> pure Nothing

onAccount :: UtxoInputWithDatum AccountDatum -> StateT BatchingState AppM (Maybe (CA.TxBody CA.BabbageEra))
onAccount account =
  Catch.catch @_ @SomeException (makeDecision account) ((>> pure Nothing) . Logger.logWarnN . Text.pack . show)

processAll :: BatchingInput -> [UtxoInputWithDatum AccountDatum] -> AppM [CA.TxBody CA.BabbageEra]
processAll initialInput accounts = do
  Logger.logInfoN "Process all to filter valid accounts for batching transaction."
  minSize <- Reader.asks aeMinRequestsPerTx
  (maybeTxBodies, BatchingState {bsInput = finalInput}) <-
    State.runStateT (traverse onAccount accounts) (BatchingState initialInput Nothing)
  let txBodies = Maybe.catMaybes maybeTxBodies
  Logger.logInfoN "Check if the remaining accounts can be included in a transaction."
  if getSize finalInput >= minSize
    then logBatch finalInput >> List.snoc txBodies . fst <$> (batchingConstraints finalInput >>= buildTx)
    else pure txBodies

getSize :: BatchingInput -> Int
getSize BatchingInput {biAccountInputs} = sum (getAccountSize . uiwdDatum <$> biAccountInputs)

getAccountSize :: AccountDatum -> Int
getAccountSize AccountDatum {adNormalRequests, adClearRequests, adCollateralUpdate} =
  if (length adNormalRequests + Map.size adClearRequests) == 0
    then maybe 0 (const 1) adCollateralUpdate
    else length adNormalRequests + Map.size adClearRequests

logBatch :: BatchingInput -> AppM ()
logBatch input =
  Logger.logInfoN $
    "Batching the "
      <> printInt (getSize input)
      <> " requests in "
      <> printInt (length (biAccountInputs input))
      <> " accounts"
  where
    printInt :: Int -> Text
    printInt = Text.pack . show
