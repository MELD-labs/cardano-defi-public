{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Api.Handler.Account (AccountApi, accountApi, calculateAccountState) where

import Cardano.Api qualified as CA
import Cardano.Api.Shelley qualified as CA
import Control.Concurrent qualified as Concurrent
import Control.Exception (SomeException)
import Control.Monad qualified as Monad
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Data.Either (rights)
import Data.Foldable qualified as Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import Plutarch.Extra.AssetClass (fromPlutusAssetClass, toPlutusAssetClass)
import PlutusLedgerApi.V2 (TxId (TxId), TxOutRef (TxOutRef))
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as PlutusTx
import Ply ((#))
import Ply qualified
import Ply.Core.Serialize.Script qualified as Ply
import Servant (ServerT, (:<|>) ((:<|>)))

import Cardano.Api.Extra.Adapters (toCardanoTxOut, toCardanoValue)
import Cardano.Api.Extra.AssetId (AssetIdText (AssetIdText))
import Cardano.Index.Data.AddressText (AddressText (unAddressText))
import Lending.Api.Common (addressFromScriptHashes, getCurrentTime)
import Lending.Api.Config
  ( ContractsConfigApi
      ( ContractsConfigApi
      , ccaAccountAuthToken
      , ccaAccountOwnerNftScript
      , ccaAccountScriptHash
      , ccaLendingFunctionCalculateAccountValue
      , ccaLendingFunctionCalculateLiquidationResult
      , ccaLendingFunctionInterestRate
      , ccaLendingFunctionProcessAccount
      , ccaLendingFunctionUpdatePool
      , ccaOracleCheckerToken
      )
  )
import Lending.Api.Env
  ( AppEnv
      ( AppEnv
      , aeContractsConfig
      , aeNetworkParams
      , aeNodeConnection
      , aeNormalConnectionPool
      )
  , AppM
  , hoistScriptError
  , runSqlM
  )
import Lending.Api.Handler.HistoryAccount (historyAccountH)
import Lending.Api.Transactions.CloseAccount
  ( CloseAccountInput
      ( CloseAccountInput
      , claiAccountAuthToken
      , claiAccountAuthTokenScript
      , claiAccountInput
      , claiAccountRedeemer
      , claiAccountRefScriptUtxo
      , claiReferenceManagerInput
      )
  , closeAccountConstraints
  )
import Lending.Api.Transactions.CreateAccount
  ( CreateAccountInput
      ( CreateAccountInput
      , craiAccountDatum
      , craiAccountScriptAddress
      , craiAccountTokenAssetId
      , craiAccountTokenScript
      , craiAccountValue
      , craiNftAssetId
      , craiNftScript
      )
  , createAccountConstraints
  )
import Lending.Api.Transactions.LiquidateAccount
  ( LiquidateAccountInput
      ( LiquidateAccountInput
      , laiAccountInput
      , laiAccountRedeemer
      , laiAccountRefScriptUtxo
      , laiAccountScriptAddress
      , laiAccountTokenAssetId
      , laiAccountTokenScript
      , laiLiquidatedAccountDatum
      , laiLiquidatedAccountValue
      , laiNewAccountDatum
      , laiNewAccountValue
      , laiNftAssetId
      , laiNftScript
      )
  , liquidateAccountConstraints
  )
import Lending.Api.Transactions.UpdateAccount
  ( UpdateAccountInput
      ( UpdateAccountInput
      , uaiAccountDatum
      , uaiAccountInput
      , uaiAccountRedeemer
      , uaiAccountRefScriptUtxo
      , uaiAccountScriptAddress
      , uaiAccountValue
      , uaiReferenceManagerInput
      )
  , updateAccountConstraints
  )
import Lending.Api.Types.Account
  ( AccountApi
  , AccountInformation
    ( AccountInformation
    , aiBorrowings
    , aiCollaterals
    , aiLoanToValue
    , aiSupplies
    )
  , AccountState (AccountState)
  , AppliedAccount (AppliedAccount, AppliedAccountError)
  , AssetValue (AssetValue)
  , CloseAccountApi
  , CloseAccountRequest
    ( CloseAccountRequest
    , clarChangeAddress
    , clarRef
    , clarUtxos
    )
  , CloseAccountResponse (CloseAccountResponse)
  , CreateAccountApi
  , CreateAccountRequest
    ( CreateAccountRequest
    , crarChangeAddress
    , crarCollateralUpdate
    , crarNormalRequests
    , crarUtxos
    )
  , CreateAccountResponse (CreateAccountResponse)
  , ExceedLtvAccount
    ( ExceedLtvAccount
    , elaBorrowings
    , elaCollaterals
    , elaCurrentLoanToValue
    , elaId
    , elaRef
    , elaSupplies
    , elaUserNft
    )
  , HistoryAccountApi
  , LiquidateAccountApi
  , LiquidateAccountRequest
    ( LiquidateAccountRequest
    , larChangeAddress
    , larClearRequests
    , larLiquidatingCollateral
    , larLiquidatingDebt
    , larRef
    , larUtxos
    )
  , LiquidateAccountResponse (LiquidateAccountResponse)
  , MigrateAccountApi
  , MigrateAccountInputResponse
    ( MigrateAccountInputResponse
    )
  , QueryAccountsApi
  , QueryAccountsByAddressApi
  , QueryAccountsResponse (QueryAccountsResponse)
  , QueryExceedLtvAccountsApi
  , QueryExceedLtvAccountsResponse (QueryExceedLtvAccountsResponse)
  , UpdateAccountApi
  , UpdateAccountRequest
    ( UpdateAccountRequest
    , uarChangeAddress
    , uarClearRequests
    , uarCollateralUpdate
    , uarNormalRequests
    , uarRef
    , uarUtxos
    )
  , UpdateAccountResponse (UpdateAccountResponse)
  , Utxos (Utxos)
  , uarClearRequests
  )
import Lending.Api.Types.Exception
  ( ServerError
      ( ProcessingAccountNotFound
      , UnableToDeserializeOwnerNftScript
      , UnableToParseUserNft
      )
  , UserError
    ( AccountOwnerNftNotFound
    , InvalidAccountOwnerNft
    , TooManyUtxoInRequest
    , UnclosableAccount
    , UserUtxoNotFound
    )
  )
import Lending.Api.Types.Request
  ( fromClearRequest
  , fromRequest
  , toClearRequest
  , toRequest
  )
import Lending.Core.AccountValue
  ( AccountValue (AccountValue, avBorrowings, avSupplies)
  , ExtractedAccount (ExtractedAccount, eaAccountId, eaAccountRef, eaAccountUtxo)
  , checkLiquidationThreshold
  , fillNewAssets
  , getDeltaTime
  , getLiquidationResult
  , getLoanToValue
  , getValueFromAccount
  , tryProcessAccount
  , tryUpdateInterestRate
  , tryUpdatePool
  )
import Lending.Core.Api (extractUserResponse, utxoCountLimit)
import Lending.Core.Errors (eitherE)
import Lending.Core.MinAdaUtxo (calculateMinAdaAccountUtxo)
import Lending.Core.Utils (calculateAmount, queryInputsAndCollaterals)
import Lending.Core.Utils qualified as Utils
import Lending.Index.Query.Account
  ( getAccountUtxoByAccountRef
  , getAllAccountUtxos
  , getMultipleAccountUtxosByOwnerNft
  )
import Lending.Index.Query.Manager (getManagerUtxo)
import Lending.Index.Query.Oracle (getOracleUtxo)
import Lending.Index.Query.Pool (getPoolUtxo)
import Lending.Index.Query.ScriptDeployment (queryScriptDeployment)
import Lending.Types.Account
  ( AccountDatum
      ( AccountDatum
      , adBorrowings
      , adClearRequests
      , adCollateralAssets
      , adCollateralUpdate
      , adExtraLovelace
      , adNormalRequests
      , adProtocolIncentive
      , adSupplies
      , adUserNft
      )
  , AccountLiquidateRedeemerData (AccountLiquidateRedeemerData, alrUserNft)
  , AccountRedeemer
    ( AccountCloseRedeemer
    , AccountLiquidateRedeemer
    , AccountUpdateRedeemer
    )
  , Request
    ( BorrowRequest
    , RepayRequest
    , SupplyRequest
    , WithdrawRequest
    , brAsset
    , rrAsset
    , srAsset
    , wrAsset
    )
  )
import Lending.Types.Account.OffChain
  ( LiquidationResultData
      ( LiquidationResultData
      , lrContinuingAccountDatum
      , lrNewAccountDatum
      , lrNewAccountValue
      )
  )
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, LtvRatio, Price, convertFrom)
import Lending.Types.Manager
  ( GlobalRiskParameters (grpMinAdaUtxo)
  , ManagerDatum (ManagerDatum, mdAccountAuthToken, mdGlobalRiskParameters, mdRiskParameters)
  , RiskParameters
  )
import Lending.Types.Oracle (OracleDatum (OracleDatum, odAssetPrices))
import Lending.Types.OracleCheckerToken (OracleCheckerTokenRedeemer (OracleCheckerTokenRedeemer))
import Lending.Types.Pool
  ( AssetInformation (aiCumulatedInterestRateBorrowing, aiCumulatedInterestRateSupplying)
  , PoolDatum (PoolDatum, pdAssets, pdLastUpdatedTime)
  )
import Lending.Types.Pool.OffChain
  ( MatcherContextData
      ( MatcherContextData
      , mcdAccountAuthToken
      , mcdAssetPrices
      , mcdAssets
      , mcdMinAdaUtxo
      , mcdRiskParameters
      )
  , RequestResultData (RequestResultData)
  )
import TxBuilder.Api
  ( UtxoInput (UtxoInput, uiTxIn, uiTxOut)
  , UtxoInputWithDatum (UtxoInputWithDatum, uiwdDatum, uiwdUtxo)
  , buildM
  , toTxOutInlineDatum
  )
import TxBuilder.Api qualified as TB
import TxBuilder.Api.Types (NetworkParams (pparams))

type AccountTxOut = CA.TxOut CA.CtxUTxO CA.BabbageEra

accountApi :: ServerT AccountApi AppM
accountApi =
  createAccountApi
    :<|> closeAccountApi
    :<|> updateAccountApi
    :<|> liquidateAccountApi
    :<|> queryAccountsApi
    :<|> queryAccountsByAddressApi
    :<|> queryExceedLtvAccountsApi
    :<|> migrateAccountApi
    :<|> historyAccountApi

createAccountApi :: ServerT CreateAccountApi AppM
createAccountApi = createAccountH

closeAccountApi :: ServerT CloseAccountApi AppM
closeAccountApi = closeAccountH

updateAccountApi :: ServerT UpdateAccountApi AppM
updateAccountApi = updateAccountH

queryAccountsApi :: ServerT QueryAccountsApi AppM
queryAccountsApi = queryAccountsH

queryAccountsByAddressApi :: ServerT QueryAccountsByAddressApi AppM
queryAccountsByAddressApi = queryAccountsByAddress

liquidateAccountApi :: ServerT LiquidateAccountApi AppM
liquidateAccountApi = liquidateAccountH

queryExceedLtvAccountsApi :: ServerT QueryExceedLtvAccountsApi AppM
queryExceedLtvAccountsApi = queryExceedLtvAccountsH

migrateAccountApi :: ServerT MigrateAccountApi AppM
migrateAccountApi = migrateAccountInputH

historyAccountApi :: ServerT HistoryAccountApi AppM
historyAccountApi = historyAccountH

getUserUtxosAndCollateralUtxos
  :: TB.NetworkParams
  -> Maybe Utxos
  -> Maybe CA.Value
  -> AddressText (CA.AddressInEra CA.BabbageEra)
  -> AppM ([UtxoInput], [UtxoInput])
getUserUtxosAndCollateralUtxos networkParams utxoData userSpendingValue changeAddress =
  Reader.withReaderT aeNodeConnection $
    Reader.mapReaderT MonadIO.liftIO $
      case utxoData of
        Just (Utxos userUtxos collateralUtxo) -> do
          (inputs, cols) <-
            queryInputsAndCollaterals userUtxos collateralUtxo $
              pparams networkParams
          (,cols)
            <$> case userSpendingValue of
              Just val ->
                Utils.selectUserUtxos
                  val
                  changeAddress
                  inputs
                  (pparams networkParams)
              _ -> pure inputs
        Nothing ->
          maybe
            (Utils.queryUserInputsAndCollaterals (unAddressText changeAddress))
            (Utils.queryUserInputsAndCollateralBaseOnSpendingValue (unAddressText changeAddress))
            userSpendingValue

createAccountH :: CreateAccountRequest -> AppM CreateAccountResponse
createAccountH
  CreateAccountRequest
    { crarChangeAddress
    , crarUtxos
    , crarNormalRequests
    , crarCollateralUpdate
    } = do
    Logger.logInfoN $ "Create account for address :: " <> Text.pack (show crarChangeAddress)
    UtxoInputWithDatum
      referenceManagerInput
      managerDatum <-
      runSqlM getManagerUtxo aeNormalConnectionPool
    let requestList = foldMap (toRequest <$>) crarNormalRequests
        referConstraint = TB.mustReferenceInput referenceManagerInput
        estimatedUserTxFeeValue = CA.lovelaceToValue 2_000_000
        accountMinLovelace = calculateMinAdaAccountUtxo mempty
        accountDatumWithoutOwnerNft =
          AccountDatum
            { adSupplies = mempty
            , adBorrowings = mempty
            , adCollateralAssets = Foldable.fold crarCollateralUpdate
            , adUserNft = toPlutusAssetClass CA.AdaAssetId
            , adNormalRequests = requestList
            , adCollateralUpdate = Nothing
            , adProtocolIncentive = Nothing
            , adClearRequests = mempty
            , adExtraLovelace = accountMinLovelace
            }
    AppEnv
      { aeNodeConnection = CA.LocalNodeConnectInfo {CA.localNodeNetworkId = networkId}
      , aeNetworkParams
      , aeContractsConfig =
        ContractsConfigApi
          { ccaAccountAuthToken = accountAuthToken@(CA.AssetId accountPolicyId _)
          , ccaAccountOwnerNftScript
          , ccaAccountScriptHash
          , ccaLendingFunctionCalculateAccountValue
          }
      } <-
      Reader.ask
    networkParams <- MonadIO.liftIO $ Concurrent.readMVar aeNetworkParams

    accountValue <-
      getValueFromAccount ccaLendingFunctionCalculateAccountValue managerDatum accountDatumWithoutOwnerNft
        >>= eitherE id . toCardanoValue

    let negateAccAuthTokenValue = CA.negateValue $ CA.valueFromList [(accountAuthToken, CA.Quantity 1)]
        userSpendingValue = accountValue <> estimatedUserTxFeeValue <> negateAccAuthTokenValue
    (userInputs, collateralUtxos) <-
      getUserUtxosAndCollateralUtxos networkParams crarUtxos (Just userSpendingValue) crarChangeAddress

    Monad.when (length userInputs > utxoCountLimit) $
      Catch.throwM (TooManyUtxoInRequest $ length userInputs)

    UtxoInput {uiTxIn = CA.TxIn mintTxId (CA.TxIx mintTxIx)} <-
      maybe (Catch.throwM $ UserUtxoNotFound crarChangeAddress) pure $ Maybe.listToMaybe userInputs

    accountTokenScriptHash <- runSqlM (queryScriptDeployment (CA.unPolicyId accountPolicyId)) aeNormalConnectionPool

    let mintTxOutRef = TxOutRef (fromCardanoTxId mintTxId) (toInteger mintTxIx)
        ownerNftMintingPolicy = ccaAccountOwnerNftScript # mintTxOutRef
    ownerNftScript <-
      maybe (Catch.throwM UnableToDeserializeOwnerNftScript) pure $
        CA.deserialiseFromRawBytes (CA.AsPlutusScript CA.AsPlutusScriptV2) $
          Ply.serializeScript (Ply.toScript ownerNftMintingPolicy)

    let ownerNftAssetId = CA.AssetId (CA.scriptPolicyId (CA.PlutusScript CA.PlutusScriptV2 ownerNftScript)) ""
        ownerNftAssetClass = toPlutusAssetClass ownerNftAssetId
        accountDatum = accountDatumWithoutOwnerNft {adUserNft = ownerNftAssetClass}
        accScriptAddress = addressFromScriptHashes networkId ccaAccountScriptHash Nothing

        -- Check pending requests
        accountTxOut =
          CA.toCtxUTxOTxOut $
            CA.TxOut
              accScriptAddress
              (CA.TxOutValue CA.MultiAssetInBabbageEra accountValue)
              (toTxOutInlineDatum accountDatum)
              CA.ReferenceScriptNone
    checkPendingRequestsOfAccount managerDatum (accountTxOut, accountDatum)

    let createAccountInput =
          CreateAccountInput
            { craiAccountScriptAddress = accScriptAddress
            , craiAccountDatum = accountDatum
            , craiAccountValue = accountValue
            , craiNftAssetId = ownerNftAssetId
            , craiNftScript = ownerNftScript
            , craiAccountTokenAssetId = accountAuthToken
            , craiAccountTokenScript = accountTokenScriptHash
            }
    initialConstraints <- hoistScriptError $ createAccountConstraints createAccountInput

    let finalConstraints =
          initialConstraints
            <> Utils.toTxSenderConstraints userInputs collateralUtxos (unAddressText crarChangeAddress)
            <> referConstraint

    CreateAccountResponse <$> buildM finalConstraints networkParams (Just 1)

closeAccountH :: CloseAccountRequest -> AppM CloseAccountResponse
closeAccountH
  CloseAccountRequest
    { clarRef
    , clarChangeAddress
    , clarUtxos
    } =
    do
      Logger.logInfoN $ "Close account with ref : " <> Text.pack (show clarRef)
      UtxoInputWithDatum referenceManagerInput _ <-
        runSqlM getManagerUtxo aeNormalConnectionPool
      AppEnv
        { aeNodeConnection = CA.LocalNodeConnectInfo {CA.localNodeNetworkId = _}
        , aeNetworkParams
        , aeContractsConfig =
          ContractsConfigApi
            { ccaAccountScriptHash
            , ccaAccountAuthToken = accountAuthToken@(CA.AssetId accountPolicyId _)
            }
        } <-
        Reader.ask
      networkParams <- MonadIO.liftIO $ Concurrent.readMVar aeNetworkParams
      accountRefScriptUtxo <- runSqlM (queryScriptDeployment ccaAccountScriptHash) aeNormalConnectionPool
      accountTokenScriptHash <- runSqlM (queryScriptDeployment (CA.unPolicyId accountPolicyId)) aeNormalConnectionPool

      UtxoInputWithDatum
        accountInput
        AccountDatum {adSupplies, adBorrowings, adUserNft} <-
        runSqlM (getAccountUtxoByAccountRef clarRef) aeNormalConnectionPool

      ownerNftAcId <-
        maybe (Catch.throwM $ InvalidAccountOwnerNft adUserNft) pure $ fromPlutusAssetClass adUserNft

      Monad.unless
        (all (== 0) adSupplies && all (== 0) adBorrowings)
        $ Catch.throwM (UnclosableAccount adSupplies adBorrowings)
      let estimatedUserTxFeeValue = CA.lovelaceToValue 2_000_000
          userSpendingValue = estimatedUserTxFeeValue <> CA.valueFromList [(ownerNftAcId, 1)]
      (userInputs, collateralUtxos) <-
        getUserUtxosAndCollateralUtxos networkParams clarUtxos (Just userSpendingValue) clarChangeAddress
      Monad.when (length userInputs > utxoCountLimit) $
        Catch.throwM (TooManyUtxoInRequest $ length userInputs)

      let userTotalValue = Utils.sumUtxoValue userInputs

      Monad.when (CA.selectAsset userTotalValue ownerNftAcId /= CA.Quantity 1) $
        Catch.throwM $
          AccountOwnerNftNotFound (AssetIdText ownerNftAcId)

      let closeAccountIntput =
            CloseAccountInput
              { claiAccountInput = accountInput
              , claiAccountRedeemer = AccountCloseRedeemer
              , claiAccountRefScriptUtxo = accountRefScriptUtxo
              , claiReferenceManagerInput = referenceManagerInput
              , claiAccountAuthToken = accountAuthToken
              , claiAccountAuthTokenScript = accountTokenScriptHash
              }
      initialConstraints <- hoistScriptError $ closeAccountConstraints closeAccountIntput

      let finalConstraints =
            initialConstraints
              <> Utils.toTxSenderConstraints userInputs collateralUtxos (unAddressText clarChangeAddress)

      CloseAccountResponse <$> buildM finalConstraints networkParams (Just 1)

getAsset :: Request -> Asset
getAsset SupplyRequest {srAsset} = srAsset
getAsset WithdrawRequest {wrAsset} = wrAsset
getAsset BorrowRequest {brAsset} = brAsset
getAsset RepayRequest {rrAsset} = rrAsset

updateAccountH :: UpdateAccountRequest -> AppM UpdateAccountResponse
updateAccountH
  UpdateAccountRequest
    { uarRef
    , uarChangeAddress
    , uarNormalRequests
    , uarCollateralUpdate
    , uarUtxos
    , uarClearRequests
    } = do
    Logger.logInfoN $ "Update account with ref : " <> Text.pack (show uarRef)
    UtxoInputWithDatum
      referenceManagerInput
      managerDatum <-
      runSqlM getManagerUtxo aeNormalConnectionPool
    let requestList = toRequest <$> uarNormalRequests
        clearRequestList = toClearRequest <$> uarClearRequests
    UtxoInputWithDatum
      accountInput
      oldAccountDatum@AccountDatum
        { adSupplies
        , adBorrowings
        , adUserNft
        } <-
      runSqlM (getAccountUtxoByAccountRef uarRef) aeNormalConnectionPool

    ownerNftAcId <-
      maybe (Catch.throwM $ InvalidAccountOwnerNft adUserNft) pure $ fromPlutusAssetClass adUserNft

    AppEnv
      { aeNodeConnection = CA.LocalNodeConnectInfo {CA.localNodeNetworkId = networkId}
      , aeNetworkParams
      , aeContractsConfig =
        ContractsConfigApi
          { ccaAccountScriptHash
          , ccaLendingFunctionCalculateAccountValue
          }
      } <-
      Reader.ask
    networkParams <- liftIO $ Concurrent.readMVar aeNetworkParams

    accountRefScriptUtxo <- runSqlM (queryScriptDeployment ccaAccountScriptHash) aeNormalConnectionPool

    let estimatedUserTxFeeValue = CA.lovelaceToValue 2_000_000
        oldAccountUtxoValue = Utils.sumUtxoValue [accountInput]
        newAccountExtraLovelace =
          calculateMinAdaAccountUtxo
            (Map.keysSet adSupplies <> Map.keysSet adBorrowings <> Set.fromList (getAsset <$> requestList))
        newAccountDatum =
          oldAccountDatum
            { adNormalRequests = requestList
            , adCollateralUpdate = uarCollateralUpdate
            , adClearRequests = clearRequestList
            , adExtraLovelace = newAccountExtraLovelace
            }
        accScriptAddress = addressFromScriptHashes networkId ccaAccountScriptHash Nothing
    newAccountUtxoValue <-
      getValueFromAccount ccaLendingFunctionCalculateAccountValue managerDatum newAccountDatum
        >>= eitherE id . toCardanoValue

    -- Check pending requests
    let accountTxOut =
          CA.toCtxUTxOTxOut $
            CA.TxOut
              accScriptAddress
              (CA.TxOutValue CA.MultiAssetInBabbageEra newAccountUtxoValue)
              (toTxOutInlineDatum newAccountDatum)
              CA.ReferenceScriptNone
    checkPendingRequestsOfAccount managerDatum (accountTxOut, newAccountDatum)

    let userSpendingValue = newAccountUtxoValue <> CA.negateValue oldAccountUtxoValue

    (userInputs, collateralUtxos) <-
      getUserUtxosAndCollateralUtxos
        networkParams
        uarUtxos
        (Just (userSpendingValue <> CA.valueFromList [(ownerNftAcId, 1)] <> estimatedUserTxFeeValue))
        uarChangeAddress

    Monad.when (length userInputs >= utxoCountLimit) $
      Catch.throwM (TooManyUtxoInRequest $ length userInputs)

    let userTotalValue = Utils.sumUtxoValue userInputs

    Monad.when (CA.selectAsset userTotalValue ownerNftAcId /= CA.Quantity 1) $
      Catch.throwM $
        AccountOwnerNftNotFound (AssetIdText ownerNftAcId)

    let updateAccountInput =
          UpdateAccountInput
            { uaiAccountInput = accountInput
            , uaiAccountRedeemer = AccountUpdateRedeemer
            , uaiAccountRefScriptUtxo = accountRefScriptUtxo
            , uaiAccountDatum = newAccountDatum
            , uaiAccountValue = newAccountUtxoValue
            , uaiAccountScriptAddress = accScriptAddress
            , uaiReferenceManagerInput = referenceManagerInput
            }

    initialConstraints <- hoistScriptError $ updateAccountConstraints updateAccountInput

    let finalConstraints =
          initialConstraints
            <> Utils.toTxSenderConstraints userInputs collateralUtxos (unAddressText uarChangeAddress)

    UpdateAccountResponse <$> buildM finalConstraints networkParams (Just 1)

mapToAssocMap :: forall k v. Map k v -> AssocMap.Map k v
mapToAssocMap = AssocMap.fromList . Map.toList

liquidateAccountH :: LiquidateAccountRequest -> AppM LiquidateAccountResponse
liquidateAccountH
  LiquidateAccountRequest
    { larRef
    , larChangeAddress
    , larUtxos
    , larClearRequests
    , larLiquidatingDebt
    , larLiquidatingCollateral
    } = do
    Logger.logInfoN $ "Liquidate account with owner NFT : " <> Text.pack (show larRef)
    UtxoInputWithDatum referenceManagerInput managerDatum <- runSqlM getManagerUtxo aeNormalConnectionPool
    UtxoInputWithDatum
      accountInput@UtxoInput {uiTxOut = accountTxOutInput}
      oldAccountDatum@AccountDatum {adExtraLovelace} <-
      runSqlM (getAccountUtxoByAccountRef larRef) aeNormalConnectionPool
    UtxoInputWithDatum {uiwdDatum = OracleDatum {odAssetPrices}, uiwdUtxo = referOracleInput} <-
      runSqlM getOracleUtxo aeNormalConnectionPool

    let clearRequestList = toClearRequest <$> larClearRequests

    AppEnv
      { aeNodeConnection = CA.LocalNodeConnectInfo {CA.localNodeNetworkId = networkId}
      , aeNetworkParams
      , aeContractsConfig =
        ContractsConfigApi
          { ccaAccountScriptHash
          , ccaAccountAuthToken = accountAuthToken@(CA.AssetId accountPolicyId _)
          , ccaAccountOwnerNftScript
          , ccaOracleCheckerToken = oracleCheckerToken@(CA.AssetId oracleCheckerPolicyId _)
          , ccaLendingFunctionCalculateLiquidationResult
          }
      } <-
      Reader.ask
    UtxoInputWithDatum {uiwdDatum = PoolDatum {pdAssets}, uiwdUtxo = referencePoolInput} <-
      runSqlM getPoolUtxo aeNormalConnectionPool

    networkParams <- liftIO $ Concurrent.readMVar aeNetworkParams

    (userUtxos, collateralUtxos) <-
      getUserUtxosAndCollateralUtxos networkParams larUtxos Nothing larChangeAddress

    accountRefScriptUtxo <- runSqlM (queryScriptDeployment ccaAccountScriptHash) aeNormalConnectionPool

    let estimatedUserTxFeeValue = CA.lovelaceToValue 2_000_000
        mockOwnerNft = toPlutusAssetClass CA.AdaAssetId
        -- Note: If userInputs contains oracleCheckerToken then burn it else mint 1.
        liquidateRedeemerDataWithMockUserNft =
          AccountLiquidateRedeemerData
            larLiquidatingDebt
            larLiquidatingCollateral
            clearRequestList
            adExtraLovelace
            mockOwnerNft
    LiquidationResultData
      { lrContinuingAccountDatum
      , lrNewAccountDatum
      , lrNewAccountValue
      } <-
      getLiquidationResult
        ccaLendingFunctionCalculateLiquidationResult
        managerDatum
        (mapToAssocMap odAssetPrices)
        (mapToAssocMap pdAssets)
        oldAccountDatum
        liquidateRedeemerDataWithMockUserNft

    newAccountValue <- either Catch.throwM pure (toCardanoValue lrNewAccountValue)

    let oracleCheckerTokenAmt = CA.selectAsset (Utils.sumUtxoValue userUtxos) oracleCheckerToken
        spendingOracleCheckerToken = CA.valueFromList [(oracleCheckerToken, oracleCheckerTokenAmt)]
        negateAccAuthTokenValue = CA.negateValue $ CA.valueFromList [(accountAuthToken, CA.Quantity 1)]
        userSpendingValue = newAccountValue <> estimatedUserTxFeeValue <> negateAccAuthTokenValue
    userInputs <-
      Utils.selectUserUtxos
        (userSpendingValue <> spendingOracleCheckerToken)
        larChangeAddress
        userUtxos
        (pparams networkParams)

    Monad.when (length userInputs >= utxoCountLimit) $
      Catch.throwM (TooManyUtxoInRequest $ length userInputs)

    UtxoInput {uiTxIn = CA.TxIn mintTxId (CA.TxIx mintTxIx)} <-
      maybe (Catch.throwM $ UserUtxoNotFound larChangeAddress) pure $ Maybe.listToMaybe userInputs

    let mintTxOutRef = TxOutRef (fromCardanoTxId mintTxId) (toInteger mintTxIx)
        ownerNftMintingPolicy = ccaAccountOwnerNftScript # mintTxOutRef

    ownerNftScript <-
      maybe (Catch.throwM UnableToDeserializeOwnerNftScript) pure $
        CA.deserialiseFromRawBytes (CA.AsPlutusScript CA.AsPlutusScriptV2) $
          Ply.serializeScript (Ply.toScript ownerNftMintingPolicy)

    let ownerNftAssetId = CA.AssetId (CA.scriptPolicyId (CA.PlutusScript CA.PlutusScriptV2 ownerNftScript)) ""
        ownerNftAssetClass = toPlutusAssetClass ownerNftAssetId
        newAccountWithOwnerNft = lrNewAccountDatum {adUserNft = ownerNftAssetClass}
        liquidateRedeemerData = liquidateRedeemerDataWithMockUserNft {alrUserNft = ownerNftAssetClass}

    oracleCheckerTokenScriptHash <-
      runSqlM (queryScriptDeployment (CA.unPolicyId oracleCheckerPolicyId)) aeNormalConnectionPool
    let octScript = TB.RefScript CA.PlutusScriptV2 oracleCheckerTokenScriptHash
        CA.Quantity mintedOctAmt = if oracleCheckerTokenAmt == 0 then 1 else negate oracleCheckerTokenAmt
        mintOracleCheckerToken =
          TB.mustMintValue oracleCheckerToken mintedOctAmt octScript (OracleCheckerTokenRedeemer odAssetPrices)

    accountTokenScriptHash <- runSqlM (queryScriptDeployment (CA.unPolicyId accountPolicyId)) aeNormalConnectionPool

    let liquidateAccountInput =
          LiquidateAccountInput
            { laiAccountInput = accountInput
            , laiAccountRedeemer = AccountLiquidateRedeemer liquidateRedeemerData
            , laiAccountRefScriptUtxo = accountRefScriptUtxo
            , laiAccountScriptAddress = addressFromScriptHashes networkId ccaAccountScriptHash Nothing
            , laiLiquidatedAccountDatum = lrContinuingAccountDatum
            , laiLiquidatedAccountValue = TB.valueInTxOut accountTxOutInput
            , laiNewAccountDatum = newAccountWithOwnerNft
            , laiNewAccountValue = newAccountValue
            , laiNftAssetId = ownerNftAssetId
            , laiNftScript = ownerNftScript
            , laiAccountTokenAssetId = accountAuthToken
            , laiAccountTokenScript = accountTokenScriptHash
            }

    initialConstraints <- hoistScriptError $ liquidateAccountConstraints liquidateAccountInput

    let referenceInputConstraints =
          TB.mustReferenceInput referenceManagerInput
            <> TB.mustReferenceInput referencePoolInput
            <> TB.mustReferenceInput referOracleInput

        finalConstraints =
          initialConstraints
            <> Utils.toTxSenderConstraints userInputs collateralUtxos (unAddressText larChangeAddress)
            <> referenceInputConstraints
            <> mintOracleCheckerToken

    LiquidateAccountResponse <$> buildM finalConstraints networkParams (Just 1)

queryAccountsH :: [AssetIdText] -> AppM QueryAccountsResponse
queryAccountsH ownerNfts = do
  UtxoInputWithDatum {uiwdDatum = managerDatum@ManagerDatum {mdRiskParameters}} <-
    runSqlM getManagerUtxo aeNormalConnectionPool
  UtxoInputWithDatum {uiwdDatum = OracleDatum {odAssetPrices = assetPrices}} <-
    runSqlM getOracleUtxo aeNormalConnectionPool
  accountsUtxoInputs <-
    runSqlM (getMultipleAccountUtxosByOwnerNft ownerNfts) aeNormalConnectionPool
  UtxoInputWithDatum {uiwdDatum = PoolDatum {pdAssets = currentAssetMap, pdLastUpdatedTime}} <-
    runSqlM getPoolUtxo aeNormalConnectionPool
  curTime <- getCurrentTime
  functionInterestRate <- Reader.asks (ccaLendingFunctionInterestRate . aeContractsConfig)
  let dTime = getDeltaTime pdLastUpdatedTime curTime
  updatedAssetMap <-
    fillNewAssets (uiwdDatum . eaAccountUtxo <$> accountsUtxoInputs)
      <$> tryUpdateInterestRate functionInterestRate mdRiskParameters currentAssetMap dTime
  (`QueryAccountsResponse` curTime)
    <$> traverse
      (calculateAccountState managerDatum currentAssetMap updatedAssetMap dTime assetPrices)
      accountsUtxoInputs

checkNft :: (CA.AssetId, CA.Quantity) -> Bool
checkNft (CA.AssetId _ "", 1) = True
checkNft _ = False

queryAccountsByAddress :: AddressText (CA.AddressInEra CA.BabbageEra) -> AppM QueryAccountsResponse
queryAccountsByAddress address = do
  (utxos, _) <-
    Reader.withReaderT aeNodeConnection $
      Reader.mapReaderT MonadIO.liftIO $
        Utils.queryUserInputsAndCollaterals (unAddressText address)
  let maybeNfts = ((AssetIdText . fst) <$>) . filter checkNft . CA.valueToList $ Utils.sumUtxoValue utxos
  queryAccountsH maybeNfts

queryExceedLtvAccountsH :: AppM QueryExceedLtvAccountsResponse
queryExceedLtvAccountsH = do
  UtxoInputWithDatum {uiwdDatum = PoolDatum {pdAssets = mapAssets}} <-
    runSqlM getPoolUtxo aeNormalConnectionPool
  UtxoInputWithDatum {uiwdDatum = ManagerDatum {mdRiskParameters}} <- runSqlM getManagerUtxo aeNormalConnectionPool
  UtxoInputWithDatum {uiwdDatum = OracleDatum {odAssetPrices}} <- runSqlM getOracleUtxo aeNormalConnectionPool
  allAccountList <- runSqlM getAllAccountUtxos aeNormalConnectionPool
  let liquidatedAccList = foldr (filterAccount mapAssets mdRiskParameters odAssetPrices) [] allAccountList
  pure $ QueryExceedLtvAccountsResponse liquidatedAccList
  where
    filterAccount
      :: Map Asset AssetInformation
      -> Map Asset RiskParameters
      -> Map Asset Price
      -> ExtractedAccount
      -> [ExceedLtvAccount]
      -> [ExceedLtvAccount]
    filterAccount
      mapAssets
      riskParams
      assetPrices
      ExtractedAccount {eaAccountId, eaAccountRef, eaAccountUtxo}
      result = do
        let UtxoInputWithDatum
              { uiwdDatum =
                AccountDatum
                  { adSupplies
                  , adBorrowings
                  , adCollateralAssets
                  , adProtocolIncentive
                  , adUserNft
                  }
              } = eaAccountUtxo
            currentSupplyValue = calculateAmount mapAssets adSupplies aiCumulatedInterestRateSupplying
            currentBorrowValue = calculateAmount mapAssets adBorrowings aiCumulatedInterestRateBorrowing
            accountValue =
              AccountValue
                currentSupplyValue
                currentBorrowValue
            suppliedCollaterals =
              Map.filterWithKey (const . flip (Map.findWithDefault False) adCollateralAssets) currentSupplyValue
         in if isJust adProtocolIncentive
              || checkLiquidationThreshold riskParams assetPrices suppliedCollaterals currentBorrowValue
              then result
              else
                let newAccs =
                      case fromPlutusAssetClass adUserNft of
                        Nothing -> [] -- Note: If can not parse the owner nft, just skip that account
                        Just assetId ->
                          [ ExceedLtvAccount
                              { elaSupplies = currentSupplyValue
                              , elaBorrowings = currentBorrowValue
                              , elaCollaterals = adCollateralAssets
                              , elaCurrentLoanToValue = loanToValue accountValue adCollateralAssets assetPrices
                              , elaUserNft = AssetIdText assetId
                              , elaId = eaAccountId
                              , elaRef = eaAccountRef
                              }
                          ]
                 in newAccs <> result

migrateAccountInputH :: AppM MigrateAccountInputResponse
migrateAccountInputH = do
  AppEnv {aeContractsConfig = ContractsConfigApi {ccaAccountScriptHash, ccaAccountAuthToken}} <-
    Reader.ask
  flip runSqlM aeNormalConnectionPool $
    MigrateAccountInputResponse
      <$> ((eaAccountUtxo <$>) <$> getAllAccountUtxos)
      <*> queryScriptDeployment ccaAccountScriptHash
      <*> pure (AssetIdText ccaAccountAuthToken)

calculateAccountState
  :: ManagerDatum
  -> Map Asset AssetInformation
  -> Map Asset AssetInformation
  -> Integer
  -> Map Asset Price
  -> ExtractedAccount
  -> AppM AccountState
calculateAccountState
  ManagerDatum {mdRiskParameters, mdGlobalRiskParameters, mdAccountAuthToken}
  currentAssetMap
  updatedAssetMap
  dTime
  prices
  ExtractedAccount {eaAccountId, eaAccountRef = eaAccountRef@(CA.TxIn txId _), eaAccountUtxo} = do
    AppEnv
      { aeNodeConnection = CA.LocalNodeConnectInfo {CA.localNodeNetworkId = networkId}
      } <-
      Reader.ask

    -- Current account
    let accDatum@AccountDatum
          { adNormalRequests
          , adCollateralUpdate
          , adProtocolIncentive
          , adClearRequests
          , adUserNft
          , adExtraLovelace
          } = uiwdDatum eaAccountUtxo
        currentAcc = calculateAccountInfo prices currentAssetMap accDatum

    -- Updated account
    let updatedAcc = calculateAccountInfo prices updatedAssetMap accDatum

        -- Applied account
        ctx =
          MatcherContextData
            { mcdAssetPrices = prices
            , mcdAssets = updatedAssetMap
            , mcdRiskParameters = mdRiskParameters
            , mcdMinAdaUtxo = grpMinAdaUtxo mdGlobalRiskParameters
            , mcdAccountAuthToken = mdAccountAuthToken
            }
    newAccDatum <-
      Catch.tryJust @_ @SomeException (Just . extractUserResponse . show) $
        applyNormalRequests currentAssetMap dTime ctx (uiTxOut $ uiwdUtxo eaAccountUtxo, accDatum)
    let appliedAcc = calculateAccountInfo prices updatedAssetMap <$> newAccDatum

    -- Other fields
    let pendingCollateralUpdate = Set.fromList . Map.keys . Map.filter id <$> adCollateralUpdate
        requestListApi = rights $ fromRequest networkId <$> adNormalRequests
    clearRequestListApi <- either Catch.throwM pure $ traverse (fromClearRequest networkId) adClearRequests
    userNft <-
      maybe
        (Catch.throwM $ UnableToParseUserNft adUserNft)
        (pure . AssetIdText)
        (fromPlutusAssetClass adUserNft)
    pure $
      AccountState
        eaAccountId
        eaAccountRef
        txId
        userNft
        currentAcc
        updatedAcc
        (either AppliedAccountError AppliedAccount appliedAcc)
        requestListApi
        clearRequestListApi
        pendingCollateralUpdate
        (isJust adProtocolIncentive)
        adExtraLovelace

calculateAccountInfo :: Map Asset Price -> Map Asset AssetInformation -> AccountDatum -> AccountInformation
calculateAccountInfo prices assetMap AccountDatum {adSupplies, adBorrowings, adCollateralAssets} =
  let supplyActual = calculateAmount assetMap adSupplies aiCumulatedInterestRateSupplying
      borrowActual = calculateAmount assetMap adBorrowings aiCumulatedInterestRateBorrowing
      accountValue =
        AccountValue
          supplyActual
          borrowActual
      supplyValue = Map.mapWithKey (calculateValue prices) supplyActual
      borrowValue = Map.mapWithKey (calculateValue prices) borrowActual
      setCollateral = Set.fromList $ Map.keys $ Map.filter id adCollateralAssets
      currentLTV = loanToValue accountValue adCollateralAssets prices
   in AccountInformation
        { aiSupplies = supplyValue
        , aiBorrowings = borrowValue
        , aiCollaterals = setCollateral
        , aiLoanToValue = currentLTV
        }

calculateValue :: Map Asset Price -> Asset -> Actual -> AssetValue
calculateValue prices asset actual = AssetValue actual (flip convertFrom actual $ Map.findWithDefault 0 asset prices)

loanToValue :: AccountValue -> Map Asset Bool -> Map Asset Price -> Maybe LtvRatio
loanToValue AccountValue {avSupplies, avBorrowings} collaterals priceMap =
  getLoanToValue priceMap suppliedCollaterals avBorrowings
  where
    suppliedCollaterals = Map.filterWithKey (const . flip (Map.findWithDefault False) collaterals) avSupplies

fromCardanoTxId :: CA.TxId -> TxId
fromCardanoTxId txId = TxId $ PlutusTx.toBuiltin $ CA.serialiseToRawBytes txId

applyNormalRequests
  :: Map Asset AssetInformation -> Integer -> MatcherContextData -> (AccountTxOut, AccountDatum) -> AppM AccountDatum
applyNormalRequests oldAssetMap dTime ctx (accTxOut, accDatum) = do
  AppEnv
    { aeNodeConnection = CA.LocalNodeConnectInfo {CA.localNodeNetworkId = networkId}
    , aeContractsConfig = ContractsConfigApi {ccaLendingFunctionProcessAccount, ccaLendingFunctionUpdatePool}
    } <-
    Reader.ask
  RequestResultData outputs stateChange <-
    tryProcessAccount
      ccaLendingFunctionProcessAccount
      ctx
      accTxOut
  _ <- tryUpdatePool ccaLendingFunctionUpdatePool (mcdRiskParameters ctx) stateChange dTime oldAssetMap
  CA.TxOut _ _ newAccDatum _ <-
    case outputs of
      [] -> Catch.throwM $ ProcessingAccountNotFound $ adUserNft accDatum
      (acc : _) -> either Catch.throwM pure $ toCardanoTxOut networkId acc
  maybe
    (Catch.throwM $ ProcessingAccountNotFound $ adUserNft accDatum)
    pure
    $ Utils.fromTxOutInlineDatum newAccDatum

checkPendingRequestsOfAccount :: ManagerDatum -> (AccountTxOut, AccountDatum) -> AppM ()
checkPendingRequestsOfAccount
  ManagerDatum {mdRiskParameters, mdGlobalRiskParameters, mdAccountAuthToken}
  accountUtxo =
    do
      AppEnv
        { aeContractsConfig = ContractsConfigApi {ccaLendingFunctionInterestRate}
        } <-
        Reader.ask
      UtxoInputWithDatum {uiwdDatum = OracleDatum {odAssetPrices = assetPrices}} <-
        runSqlM getOracleUtxo aeNormalConnectionPool
      UtxoInputWithDatum {uiwdDatum = PoolDatum {pdAssets = currentAssetMap, pdLastUpdatedTime}} <-
        runSqlM getPoolUtxo aeNormalConnectionPool
      curTime <- getCurrentTime
      let dTime = getDeltaTime pdLastUpdatedTime curTime
      -- Update pool
      updatedAssetMap <-
        fillNewAssets [snd accountUtxo]
          <$> tryUpdateInterestRate ccaLendingFunctionInterestRate mdRiskParameters currentAssetMap dTime

      let ctx =
            MatcherContextData
              { mcdAssetPrices = assetPrices
              , mcdAssets = updatedAssetMap
              , mcdRiskParameters = mdRiskParameters
              , mcdMinAdaUtxo = grpMinAdaUtxo mdGlobalRiskParameters
              , mcdAccountAuthToken = mdAccountAuthToken
              }

      Monad.void $ applyNormalRequests currentAssetMap dTime ctx accountUtxo
