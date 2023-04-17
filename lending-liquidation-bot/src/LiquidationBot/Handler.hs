{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module LiquidationBot.Handler
  ( runWithEnv
  , liquidationBotRunner
  , liquidateAccount
  , closeAccount
  , getEnvAndRun
  , chooseCollateral
  , fiatValue
  , clearSupply
  , getLiquidatorAccountId
  )
where

import Cardano.Api qualified as CA
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.Catch (SomeException)
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.State.Lazy qualified as State
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.State (StateT)
import Data.ByteString.Char8 qualified as C8
import Data.Either (isRight)
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Monoid (Sum (Sum))
import Data.Tagged (Tagged (Tagged, unTagged))
import Data.Yaml qualified as Yaml
import Database.Esqueleto.PostgreSQL.JSON (JSONB (unJSONB))
import Database.Persist qualified as Persist
import GHC.TypeLits (Symbol)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClient
import Servant.Client (ClientEnv, ClientM)
import Servant.Client qualified as Client
import System.Environment qualified as Environment

import Cardano.Api.Extra.Adapters (fromCardanoAddressInEra, toAddressAny)
import Cardano.Api.Extra.NetworkId (NetworkIdText (NetworkIdText, unNetworkIdText))
import Cardano.Api.Extra.Node qualified as CAE
import Cardano.Api.Extra.Tx (signSubmitAndWaitTx)
import Cardano.Index.Data.AddressText (AddressText (AddressText, unAddressText))
import Cardano.Index.PersistLens (view)
import Lending.Api.Client (closeAccountClient, liquidateAccountClient, updateAccountClient)
import Lending.Api.Types.Account
  ( CloseAccountRequest (CloseAccountRequest)
  , CloseAccountResponse (CloseAccountResponse, clarTx)
  , LiquidateAccountRequest (LiquidateAccountRequest)
  , LiquidateAccountResponse (LiquidateAccountResponse, larTx)
  , UpdateAccountRequest (UpdateAccountRequest)
  , UpdateAccountResponse (UpdateAccountResponse, uarTx)
  , Utxos (Utxos)
  )
import Lending.Api.Types.Request (ApiClearRequest, fromClearRequest)
import Lending.Core.AccountValue (AccountId (AccountId))
import Lending.Core.Utils qualified as Utils
import Lending.Index.Account qualified as IA
import Lending.Index.Liquidation qualified as IL
import Lending.Index.Query.Account
  ( getExtractedAccountByAccountRef
  , queryAccountByAccountId
  )
import Lending.Index.Query.Liquidation (queryLatestLiquidations)
import Lending.Index.Query.Manager (getManagerUtxo)
import Lending.Index.Query.Oracle (getOracleUtxo)
import Lending.Index.Query.Pool (getPoolUtxo)
import Lending.Index.Query.Types (SqlM)
import Lending.Types.Account
  ( AccountDatum (AccountDatum, adClearRequests, adCollateralAssets, adSupplies)
  , ClearRequest (ClearBorrowing, ClearSupplying)
  )
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, Decimal, Fiat, Price, Receipt, convertFrom, convertTo)
import Lending.Types.Manager
  ( GlobalRiskParameters
      ( GlobalRiskParameters
      , grpLiquidatorIncentive
      , grpProtocolIncentive
      )
  , ManagerDatum (ManagerDatum, mdGlobalRiskParameters)
  )
import Lending.Types.Oracle (OracleDatum (OracleDatum, odAssetPrices))
import Lending.Types.Pool
  ( AssetInformation (AssetInformation, aiCumulatedInterestRateSupplying)
  , PoolDatum (PoolDatum, pdAssets)
  )
import LiquidationBot.Env
  ( AppEnv
      ( AppEnv
      , aeDbConnectionPool
      , aeLiquidationBotConfig
      , aeLiquidatorAddress
      , aeLiquidatorSigningKey
      , aeNodeConnection
      )
  , AppM
  , LiquidatingCollateralConfig (Custom, PriorityOrder)
  , LiquidationBotConfig
    ( LiquidationBotConfig
    , lbAccountId
    , lbLiquidatingCollateral
    , lbLiquidatingDebt
    , lbMaxRetry
    , lbNetworkId
    , lbRepayExtra
    , lbRetryDelay
    )
  , runSqlM
  )
import LiquidationBot.Exception
  ( LiquidationBotException
      ( AccountIdNotFound
      , InsufficientAccountCollateral
      , LiquidationMaxRetryReached
      , LiquidationNewAccountNotFound
      , LiquidationNotFound
      , LiquidationQueryApiError
      )
  )
import Plutarch.Extra.FixedDecimal (convertExp, ediv, emul, fromFixedZero, toFixedZero)
import TxBuilder.Api (UtxoInput (UtxoInput, uiTxIn, uiTxOut), UtxoInputWithDatum (UtxoInputWithDatum, uiwdDatum))

runWithEnv :: AppEnv -> AppM a -> IO a
runWithEnv env runner = runStdoutLoggingT $ Reader.runReaderT runner env

getEnvAndRun :: AppM () -> IO ()
getEnvAndRun runner = do
  liquidatorSigningKeyFile <- Environment.getEnv "LIQUIDATOR_SIGNING_KEY_FILE"
  liquidatorSigningKey <- Utils.loadTextEnvelope (CA.AsSigningKey CA.AsPaymentExtendedKey) liquidatorSigningKeyFile
  liquidationBotConfigFile <- Environment.getEnv "LIQUIDATION_BOT_CONFIG_FILE"
  lbConfig@LiquidationBotConfig {lbNetworkId = NetworkIdText networkId} <- Yaml.decodeFileThrow liquidationBotConfigFile
  cardanoNodeConnection <-
    CAE.mkNodeConnectInfo networkId <$> Environment.getEnv "CARDANO_NODE_SOCKET_PATH"
  dbConnection <- C8.pack <$> Environment.getEnv "DB_CONNECTION_STRING"
  let liquidatorAddress = Utils.getChangeAddress networkId liquidatorSigningKey
  runStdoutLoggingT $
    Utils.withDbPoolLending dbConnection $ \pool ->
      Reader.runReaderT runner $
        AppEnv liquidatorSigningKey liquidatorAddress lbConfig cardanoNodeConnection pool

liquidateAccount :: ClientEnv -> AppM ()
liquidateAccount clientEnv = do
  AppEnv
    { aeLiquidatorSigningKey
    , aeLiquidatorAddress
    , aeLiquidationBotConfig =
      LiquidationBotConfig
        { lbNetworkId
        , lbAccountId
        , lbRetryDelay
        , lbRepayExtra
        , lbLiquidatingDebt
        , lbLiquidatingCollateral
        , lbMaxRetry
        }
    , aeNodeConnection
    } <-
    Reader.ask
  liquidatedAccRef <- getAccountRefByAccountId lbAccountId
  (txIns, collaterals) <- Reader.runReaderT (Utils.getUserInputs aeLiquidatorAddress) aeNodeConnection
  let process :: CA.AddressInEra CA.BabbageEra -> AccountDatum -> AppM AccountId
      process accountScriptAddress AccountDatum {adCollateralAssets, adSupplies} = do
        liftIO $ putStrLn "Liquidating"
        liquidatingCollateral <- case lbLiquidatingCollateral of
          Custom mCollaterals -> pure mCollaterals
          PriorityOrder lCollaterals -> do
            UtxoInputWithDatum {uiwdDatum = PoolDatum {pdAssets}} <-
              runSqlM getPoolUtxo aeDbConnectionPool
            UtxoInputWithDatum {uiwdDatum = OracleDatum {odAssetPrices}} <-
              runSqlM getOracleUtxo aeDbConnectionPool
            UtxoInputWithDatum {uiwdDatum = ManagerDatum {mdGlobalRiskParameters}} <-
              runSqlM getManagerUtxo aeDbConnectionPool
            let accountCollaterals =
                  Map.filterWithKey (const . Maybe.fromMaybe False . flip Map.lookup adCollateralAssets) adSupplies
            maybe
              (Catch.throwM InsufficientAccountCollateral)
              pure
              $ chooseCollateral
                mdGlobalRiskParameters
                odAssetPrices
                pdAssets
                accountCollaterals
                lbLiquidatingDebt
                lCollaterals
        let calculate :: Actual -> Actual
            calculate amt =
              Tagged $
                fromFixedZero $
                  convertExp $
                    toFixedZero (unTagged amt) `emul` (1 + lbRepayExtra)
            clearBorrowingRequests =
              Map.map
                (\amt -> ClearBorrowing (calculate amt) (fromCardanoAddressInEra aeLiquidatorAddress))
                lbLiquidatingDebt
        clearRequests <- toApiClearRequests lbNetworkId clearBorrowingRequests

        -- Call the liquidation API
        LiquidateAccountResponse {larTx} <-
          liftIO $
            queryApi clientEnv $
              liquidateAccountClient $
                LiquidateAccountRequest
                  (AddressText aeLiquidatorAddress)
                  (Just $ Utxos txIns collaterals)
                  liquidatedAccRef
                  clearRequests
                  lbLiquidatingDebt
                  liquidatingCollateral
        txId <- Reader.runReaderT (signSubmitAndWaitTx Nothing aeLiquidatorSigningKey larTx) aeNodeConnection

        let CA.TxBody txBodyContent = larTx
            buildUtxoInput idx = UtxoInput (CA.TxIn txId (CA.TxIx idx)) . CA.toCtxUTxOTxOut
            newOutputs = List.zipWithFrom buildUtxoInput 0 (CA.txOuts txBodyContent)

            isAccountUtxo (CA.TxOut address _ _ _) = address == accountScriptAddress

        case tail (filter (isAccountUtxo . uiTxOut) newOutputs) of
          [newAccountUtxo] -> pure $ AccountId $ uiTxIn newAccountUtxo
          _ -> Catch.throwM $ LiquidationNewAccountNotFound lbAccountId

  _ <- loop "Liquidate" lbMaxRetry lbRetryDelay isRight $ do
    -- Query Account from DB
    (accountScriptAddress, accountDatum) <-
      runSqlM (getExtractedAccountByAccountRef liquidatedAccRef extractor) aeDbConnectionPool
    Catch.try @_ @SomeException $ process accountScriptAddress accountDatum
  liftIO $ putStrLn "Liquidate success."

getAccountRefByAccountId :: AccountId -> AppM CA.TxIn
getAccountRefByAccountId accId =
  IA.accountRef . Persist.entityVal
    <$> ( runSqlM (queryAccountByAccountId accId) aeDbConnectionPool
            >>= maybe (Catch.throwM $ AccountIdNotFound accId) pure
        )
toApiClearRequests :: NetworkIdText -> Map.Map Asset ClearRequest -> AppM (Map.Map Asset ApiClearRequest)
toApiClearRequests networkIdText =
  either Catch.throwM pure . traverse (fromClearRequest $ unNetworkIdText networkIdText)

clearSupply :: ClientEnv -> AccountId -> AppM ()
clearSupply clientEnv liquidatorAccountId = do
  AppEnv
    { aeLiquidatorSigningKey
    , aeLiquidatorAddress
    , aeLiquidationBotConfig = LiquidationBotConfig {lbNetworkId, lbRetryDelay, lbMaxRetry}
    , aeNodeConnection
    } <-
    Reader.ask
  void $
    loop "Clear Supplied" lbMaxRetry lbRetryDelay isRight $
      Catch.try @_ @SomeException $ do
        liftIO $ putStrLn "Clearing account's supply."
        (liquidatorAccountRef, liquidatorAccountDatum) <-
          runSqlM (getAccountRefAndDatum liquidatorAccountId) aeDbConnectionPool
        let clearSupplyingRequests =
              Map.foldMapWithKey
                ( \asset a ->
                    if a > 0
                      then Map.singleton asset $ ClearSupplying $ fromCardanoAddressInEra aeLiquidatorAddress
                      else mempty
                )
                (adSupplies liquidatorAccountDatum)
        clearRequests <- toApiClearRequests lbNetworkId clearSupplyingRequests
        (txIns', collaterals') <- Reader.runReaderT (Utils.getUserInputs aeLiquidatorAddress) aeNodeConnection
        UpdateAccountResponse {uarTx} <-
          liftIO $
            queryApi clientEnv $
              updateAccountClient $
                UpdateAccountRequest
                  liquidatorAccountRef
                  mempty
                  Nothing
                  clearRequests
                  (AddressText aeLiquidatorAddress)
                  (Just $ Utxos txIns' collaterals')
        _ <- Reader.runReaderT (signSubmitAndWaitTx Nothing aeLiquidatorSigningKey uarTx) aeNodeConnection
        liftIO $ putStrLn "Cleared account's supply successfully"

closeAccount :: ClientEnv -> AppM ()
closeAccount clientEnv = do
  AppEnv
    { aeLiquidatorSigningKey
    , aeLiquidatorAddress
    , aeLiquidationBotConfig =
      LiquidationBotConfig
        { lbRetryDelay
        , lbMaxRetry
        , lbAccountId
        }
    , aeNodeConnection
    } <-
    Reader.ask
  liquidatorAccountId <- getLiquidatorAccountId lbAccountId
  clearSupply clientEnv liquidatorAccountId
  void $
    loop "Close Account" lbMaxRetry lbRetryDelay isRight $
      Catch.try @_ @SomeException $ do
        liftIO $ putStrLn "Closing account."
        -- Query DB, wait for ClearBorrowing request to be applied.
        (liquidatorAccountRef, _) <-
          loop "Query Account" lbMaxRetry lbRetryDelay (null . adClearRequests . snd) $ do
            liftIO $ putStrLn "Querying new account."
            runSqlM (getAccountRefAndDatum liquidatorAccountId) aeDbConnectionPool
        (txIns', collaterals') <- Reader.runReaderT (Utils.getUserInputs aeLiquidatorAddress) aeNodeConnection
        CloseAccountResponse {clarTx} <-
          liftIO $
            queryApi clientEnv $
              closeAccountClient $
                CloseAccountRequest
                  liquidatorAccountRef
                  (AddressText aeLiquidatorAddress)
                  (Just $ Utxos txIns' collaterals')
        _ <- Reader.runReaderT (signSubmitAndWaitTx Nothing aeLiquidatorSigningKey clarTx) aeNodeConnection
        liftIO $ putStrLn "Closed account successfully."

extractor :: Persist.Entity IA.Account -> Maybe (CA.AddressInEra CA.BabbageEra, AccountDatum)
extractor accEntity =
  Just
    ( CA.anyAddressInShelleyBasedEra $ unAddressText $ view IA.AccountScriptAddress accEntity
    , unJSONB $ view IA.AccountDatum accEntity
    )

-- | Iterate an action, break after i times or stopCondition return True with delay after repeat.
loop :: Show a => String -> Integer -> Integer -> (a -> Bool) -> AppM a -> AppM a
loop name i delay stopCondition runner = do
  liftIO $ putStrLn $ "[" <> name <> "]: Loop  (" <> show i <> ")"
  if i > 0
    then do
      rs <- runner
      liftIO $ print rs
      if stopCondition rs
        then liftIO (putStrLn "Loop completed.") >> return rs
        else liftIO (threadDelay $ fromInteger delay) >> loop name (i - 1) delay stopCondition runner
    else Catch.throwM $ LiquidationMaxRetryReached name

liquidationBotRunner :: (ClientEnv -> AppM ()) -> AppM ()
liquidationBotRunner runner = do
  apiUrl <- liftIO $ Environment.getEnv "LENDING_API_URL"
  clientEnv <-
    liftIO $
      Client.mkClientEnv
        <$> HttpClient.newManager HttpClient.tlsManagerSettings
        <*> Client.parseBaseUrl apiUrl
  runner clientEnv

queryApi :: ClientEnv -> ClientM response -> IO response
queryApi = Utils.queryApi (Catch.throwM . LiquidationQueryApiError)

fiatValue :: Map.Map Asset Price -> Map.Map Asset Actual -> Sum Fiat
fiatValue priceMap =
  Map.foldMapWithKey ((Sum .) . convertFrom . sum . flip Map.lookup priceMap)

data ChoosingCollateralState = ChoosingCollateralState
  { remainingTakable :: Fiat
  , fullyTaken :: Bool
  }

chooseCollateral
  :: GlobalRiskParameters
  -> Map.Map Asset Price
  -> Map.Map Asset AssetInformation
  -> Map.Map Asset Receipt -- Liquidating Account's supply assets
  -> Map.Map Asset Actual -- Liquidating debt
  -> [Asset] -- Liquidating collateral asset (in priority order)
  -> Maybe (Map.Map Asset Actual)
chooseCollateral
  GlobalRiskParameters {grpLiquidatorIncentive, grpProtocolIncentive}
  priceMap
  assetInfoMap
  accountCollateral
  liquidatingDebt
  assets = do
    (chosenCollaterals, state) <- State.runStateT (chooseCollateral' assets) $ ChoosingCollateralState takeable False
    if fullyTaken state
      then Just chosenCollaterals
      else Nothing
    where
      -- \| The total value (USD) of liquidating debt
      Sum liquidatingFiatDebt = fiatValue priceMap liquidatingDebt

      -- \| The maximum value (USD) that can be taken
      -- \| liquidatingFiatDebt * (1 + liquidator incentive percent)
      takeable :: Fiat
      takeable = fromDecimal $ toDecimal liquidatingFiatDebt * (1 + grpLiquidatorIncentive)

      chooseCollateral' :: [Asset] -> StateT ChoosingCollateralState Maybe (Map.Map Asset Actual)
      chooseCollateral' =
        (Map.fromList <$>)
          . traverse
            ( \asset -> do
                ChoosingCollateralState {remainingTakable, fullyTaken} <- State.get
                price <- State.lift $ Map.lookup asset priceMap
                AssetInformation {aiCumulatedInterestRateSupplying} <- State.lift $ Map.lookup asset assetInfoMap
                let accountCollateralReceiptAmt = sum $ Map.lookup asset accountCollateral
                    accountCollateralActualAmt =
                      convertFrom aiCumulatedInterestRateSupplying accountCollateralReceiptAmt
                    takableAccountCollateralActualAmt =
                      fromDecimal $
                        toDecimal accountCollateralActualAmt
                          `emul` (1 + grpLiquidatorIncentive)
                          `ediv` (1 + grpProtocolIncentive + grpLiquidatorIncentive)
                    accountCollateralFiatVal = convertFrom price takableAccountCollateralActualAmt
                if accountCollateralFiatVal < remainingTakable
                  then do
                    -- Note: Take all the collateral
                    State.put $ ChoosingCollateralState (remainingTakable - accountCollateralFiatVal) fullyTaken
                    pure (asset, takableAccountCollateralActualAmt)
                  else do
                    let actualTaking = convertTo price remainingTakable
                        fiatTaking = convertFrom price actualTaking
                    -- Note: remainingTakable <= accountCollateralFiatVal -> Fully taken
                    State.put $ ChoosingCollateralState (remainingTakable - fiatTaking) True
                    pure (asset, actualTaking)
            )

-- Conversions
toDecimal :: Tagged (a :: Symbol) Integer -> Decimal
toDecimal = convertExp . toFixedZero . toInteger
fromDecimal :: Decimal -> Tagged (a :: Symbol) Integer
fromDecimal = pure . fromFixedZero . convertExp

getLiquidatorAccountId :: AccountId -> AppM AccountId
getLiquidatorAccountId (AccountId orgRef) = do
  liquidatorAddress <- AddressText . toAddressAny <$> Reader.asks aeLiquidatorAddress
  runSqlM
    ( queryLatestLiquidations liquidatorAddress orgRef
        >>= maybe
          (Catch.throwM $ LiquidationNotFound liquidatorAddress orgRef)
          (pure . AccountId . view IL.LiquidationLiquidatorAccountRef)
    )
    aeDbConnectionPool

-- | Query Liquidator Account Ref and Datum by Account Id
getAccountRefAndDatum :: AccountId -> SqlM (CA.TxIn, AccountDatum)
getAccountRefAndDatum accountId =
  queryAccountByAccountId accountId
    >>= maybe
      (Catch.throwM $ AccountIdNotFound accountId)
      (\account -> pure (view IA.AccountRef account, unJSONB $ view IA.AccountDatum account))
