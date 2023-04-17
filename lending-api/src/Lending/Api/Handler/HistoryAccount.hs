{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Api.Handler.HistoryAccount (historyAccountH) where

import Cardano.Api qualified as CA
import Control.Monad.Catch qualified as Catch
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Time qualified as Time
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB, unJSONB))

import Lending.Api.Env
  ( AppEnv (AppEnv, aeNodeConnection, aeNormalConnectionPool)
  , AppM
  , runSqlM
  )
import Lending.Api.Types.Account
  ( HistoryAccountRequest (HistoryAccountRequest, harAccountId, harCount, harPage)
  , HistoryAccountResponse (HistoryAccountResponse)
  , HistoryAccountTx (HistoryAccountTx)
  )
import Lending.Api.Types.Exception
  ( ServerError
      ( InvalidClosingRef
      , InvalidClosingTime
      , InvalidRedeemerHistory
      , InvalidTimeTx
      , NotSupportClosingRedeemerHistoryAccount
      , NotSupportRedeemerHistoryAccount
      )
  )
import Lending.Api.Types.Request
  ( HistoryAccountAction
      ( ClearRequestAction
      , CreateAccountAction
      , LiquidateAccountAction
      , NormalRequestAction
      , SetCollateralAction
      )
  , StatusTx (Cancel, Done, Processing)
  , TxIdAndTimeTx (TxIdAndTimeTx)
  , fromAccountLiquidateRedeemerData
  , fromClearRequest
  , fromRequest
  )
import Lending.Core.Utils (getTxIdFromTxIn)
import Lending.Index.Account qualified as IA
import Lending.Index.Query.Account (queryHistoryAccountByAccountId)
import Lending.Types
  ( AccountDatum (AccountDatum)
  , AccountRedeemer
    ( AccountCloseRedeemer
    , AccountLiquidateRedeemer
    , AccountMigrateRedeemer
    , AccountUpdateRedeemer
    )
  )
import Lending.Types.Account
  ( AccountDatum (adClearRequests, adCollateralUpdate, adNormalRequests)
  , AccountRedeemer (AccountApplyRedeemer)
  )

historyAccountH :: HistoryAccountRequest -> AppM HistoryAccountResponse
historyAccountH HistoryAccountRequest {harAccountId, harCount, harPage} = do
  Logger.logInfoN $ "Get history account has ID: " <> Text.pack (show harAccountId)
  AppEnv
    { aeNodeConnection = CA.LocalNodeConnectInfo {CA.localNodeNetworkId = networkId}
    } <-
    Reader.ask
  let count = Maybe.fromMaybe defaultCount harCount
      page = Maybe.fromMaybe defaultPage harPage
      limit = toEnum count
      offset = toEnum (count * (page - 1))
  historyAccountData <- runSqlM (queryHistoryAccountByAccountId harAccountId limit offset) aeNormalConnectionPool
  historyAccount <- mapM (getHistoryAccount networkId) historyAccountData
  return $ HistoryAccountResponse historyAccount

getHistoryAccount
  :: CA.NetworkId
  -> (IA.Account, Maybe (Maybe (JSONB AccountRedeemer)), Maybe CA.TxIn, Maybe Time.UTCTime, Maybe Time.UTCTime)
  -> AppM HistoryAccountTx
getHistoryAccount networkId (accountData, mRedeemer, mClosingRef, mTxTime, mClosingTime) = do
  listHistoryAction <-
    fromDatumToListHistoryAction networkId (unJSONB (IA.accountDatum accountData))
  timeTx <- maybe (Catch.throwM (InvalidTimeTx accountData)) pure mTxTime
  let txIdAndTimeTx = toTxIdAndTimeTx (IA.accountRef accountData) timeTx

  if Maybe.isNothing (IA.accountClosingSlotNo accountData)
    then do
      let liquidationAction = case IA.accountRedeemer accountData of
            (Just (JSONB (AccountLiquidateRedeemer liquidateRedeemerData))) -> do
              liquidateRedeemerDataApi <-
                either
                  Catch.throwM
                  pure
                  (fromAccountLiquidateRedeemerData networkId liquidateRedeemerData)
              [LiquidateAccountAction liquidateRedeemerDataApi]
            _ -> []
      pure $ HistoryAccountTx (listHistoryAction <> liquidationAction) Processing txIdAndTimeTx
    else do
      closingRef <- maybe (Catch.throwM (InvalidClosingRef accountData)) pure mClosingRef
      closingTime <- maybe (Catch.throwM (InvalidClosingTime accountData)) pure mClosingTime
      closingRedeemer <-
        maybe (Catch.throwM (InvalidRedeemerHistory accountData)) pure mRedeemer
          >>= maybe (Catch.throwM (InvalidRedeemerHistory accountData)) (pure . unJSONB)
      txStatus <- getStatusFromStateData closingRedeemer closingRef closingTime
      maybe
        ( pure $ HistoryAccountTx (listHistoryAction <> [CreateAccountAction]) txStatus txIdAndTimeTx
        )
        ( \(JSONB curRedeemer) ->
            case curRedeemer of
              AccountUpdateRedeemer ->
                pure $ HistoryAccountTx listHistoryAction txStatus txIdAndTimeTx
              AccountLiquidateRedeemer liquidateRedeemerData -> do
                liquidateRedeemerDataApi <-
                  either Catch.throwM pure (fromAccountLiquidateRedeemerData networkId liquidateRedeemerData)
                pure $
                  HistoryAccountTx
                    (listHistoryAction <> [LiquidateAccountAction liquidateRedeemerDataApi])
                    txStatus
                    txIdAndTimeTx
              AccountApplyRedeemer ->
                Catch.throwM $ NotSupportRedeemerHistoryAccount AccountApplyRedeemer accountData
              AccountMigrateRedeemer ->
                Catch.throwM $ NotSupportRedeemerHistoryAccount AccountMigrateRedeemer accountData
              AccountCloseRedeemer ->
                Catch.throwM $ NotSupportRedeemerHistoryAccount AccountCloseRedeemer accountData
        )
        (IA.accountRedeemer accountData)

getStatusFromStateData :: AccountRedeemer -> CA.TxIn -> Time.UTCTime -> AppM StatusTx
getStatusFromStateData AccountApplyRedeemer txIn closingTime =
  pure $ Done $ TxIdAndTimeTx (getTxIdFromTxIn txIn) closingTime
getStatusFromStateData AccountUpdateRedeemer txIn closingTime =
  pure $ Cancel $ TxIdAndTimeTx (getTxIdFromTxIn txIn) closingTime
getStatusFromStateData (AccountLiquidateRedeemer _) txIn closingTime =
  pure $ Cancel $ TxIdAndTimeTx (getTxIdFromTxIn txIn) closingTime
getStatusFromStateData AccountCloseRedeemer _ _ =
  Catch.throwM $ NotSupportClosingRedeemerHistoryAccount AccountCloseRedeemer
getStatusFromStateData AccountMigrateRedeemer _ _ =
  Catch.throwM $ NotSupportClosingRedeemerHistoryAccount AccountMigrateRedeemer

fromDatumToListHistoryAction :: CA.NetworkId -> AccountDatum -> AppM [HistoryAccountAction]
fromDatumToListHistoryAction
  networkId
  AccountDatum
    { adClearRequests
    , adNormalRequests
    , adCollateralUpdate
    } = do
    clearRequestListApi <-
      either Catch.throwM pure $ traverse (fromClearRequest networkId) adClearRequests
    requestListApi <-
      either Catch.throwM pure $ traverse (fromRequest networkId) adNormalRequests
    let normalRequest = NormalRequestAction requestListApi
        clearRequest = ClearRequestAction (Map.toList clearRequestListApi)
        setCollateralRequest =
          SetCollateralAction (foldMap Map.toList adCollateralUpdate)
    pure [normalRequest, clearRequest, setCollateralRequest]

defaultCount :: Int
defaultCount = 50

defaultPage :: Int
defaultPage = 1

toTxIdAndTimeTx :: CA.TxIn -> Time.UTCTime -> TxIdAndTimeTx
toTxIdAndTimeTx = TxIdAndTimeTx . getTxIdFromTxIn
