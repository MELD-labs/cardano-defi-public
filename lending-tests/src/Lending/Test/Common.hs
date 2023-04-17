{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lending.Test.Common
  ( signAdditionalAndSubmitTx
  , mintTokenClient
  , faucetApi
  , clientQuote
  , clientFeedPrice
  , getUserInputs
  , queryApi
  , retry
  , retryOption
  , getLatestState
  , getLatestStateExtract
  , faucetStatusClient
  , prepareFaucetClient
  )
where

import Cardano.Api qualified as CA
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Logger qualified as Logger
import Control.Monad.Reader (MonadReader)
import Control.Retry (RetryPolicyM, RetryStatus (RetryStatus, rsIterNumber))
import Control.Retry qualified as Retry
import Data.Proxy (Proxy (Proxy))
import Data.Proxy qualified as Proxy
import Data.Text qualified as Text
import Database.Persist.Sql (ConnectionPool, Filter, PersistRecordBackend, SqlBackend, SymbolToField, (==.))
import Database.Persist.Sql qualified as Persist
import Servant.API qualified as Servant
import Servant.Client (ClientEnv, ClientM)
import Servant.Client qualified as Client
import Test.Tasty.HUnit qualified as Tasty

import Cardano.Api.Extra.Node (NodeConnectInfo)
import Cardano.Api.Extra.Query (MonadNodeQuery)
import Cardano.Api.Extra.Tx (submitTx)
import Cardano.Api.Extra.Wait (RetryOption (RetryOption, roDelayInSeconds, roMaxRetries), waitForTxConfirmation)
import Lending.Core.Utils qualified as Utils
import Lending.Faucet.Types
  ( FaucetApi
  , FaucetStatusResponse
  , MintTokenRequest
  , MintTokenResponse
  , PrepareFaucetRequest
  , PrepareFaucetResponse
  )
import Lending.Oracle.Api.Types (lendingOracleApi)
import Lending.Oracle.Api.Types.FeedPrice (FeedPriceRequest)
import Lending.Oracle.Api.Types.Quote (PriceResponse)
import Lending.Test.Env (runSqlM)

getUserInputs
  :: forall m
   . (MonadReader NodeConnectInfo m, MonadThrow m, MonadIO m)
  => CA.AddressInEra CA.BabbageEra
  -> m ([CA.TxIn], [CA.TxIn])
getUserInputs address = do
  (userTxIn, collateralTxIn) <- Utils.getUserInputs address
  MonadIO.liftIO $ do
    Tasty.assertBool "Should have a list of fund UTXOs" (not (null userTxIn))
    Tasty.assertBool "Should have a list of collateral UTXOs" (not (null collateralTxIn))
  pure (userTxIn, collateralTxIn)

mintTokenClient :: MintTokenRequest -> Client.ClientM MintTokenResponse
faucetStatusClient :: Client.ClientM FaucetStatusResponse
prepareFaucetClient :: PrepareFaucetRequest -> Client.ClientM PrepareFaucetResponse
mintTokenClient
  Servant.:<|> faucetStatusClient
  Servant.:<|> prepareFaucetClient = Client.client faucetApi

faucetApi :: Proxy.Proxy FaucetApi
faucetApi = Proxy.Proxy

clientQuote :: Maybe String -> ClientM PriceResponse
clientFeedPrice :: FeedPriceRequest -> ClientM FeedPriceRequest
clientQuote Servant.:<|> clientFeedPrice = Client.client lendingOracleApi

queryApi :: MonadIO m => ClientEnv -> ClientM response -> m response
queryApi = Utils.queryApi (Tasty.assertFailure . ("Unable to query API with error: " <>) . show)

retryOption :: RetryOption
retryOption =
  RetryOption
    { roMaxRetries = 30
    , roDelayInSeconds = 1
    }

signAdditionalAndSubmitTx
  :: (MonadMask m, MonadLogger m, MonadNodeQuery m)
  => CA.Tx CA.BabbageEra
  -> CA.SigningKey CA.PaymentExtendedKey
  -> m ()
signAdditionalAndSubmitTx (CA.Tx txBody witnesses) key = do
  let newWitness = CA.makeShelleyKeyWitness txBody (CA.WitnessPaymentExtendedKey key)
      signedTx = CA.makeSignedTransaction (newWitness : witnesses) txBody
  submitTx signedTx >>= waitForTxConfirmation retryOption

retry :: forall m a. (MonadMask m, MonadIO m, MonadLogger m) => String -> m a -> m a
retry msg task =
  Retry.recoverAll retryPolicy $ \RetryStatus {rsIterNumber} -> do
    Logger.logInfoN $
      Text.pack $
        if rsIterNumber == 0 then msg else "Retry #" <> show rsIterNumber <> "..."
    task
  where
    retryPolicy :: RetryPolicyM m
    retryPolicy = case retryOption of
      RetryOption {roMaxRetries, roDelayInSeconds} ->
        Retry.constantDelay (roDelayInSeconds * 1_000_000) <> Retry.limitRetries roMaxRetries

getLatestStateExtract
  :: forall a b m
   . ( SymbolToField "closingSlotNo" a (Maybe CA.SlotNo)
     , PersistRecordBackend a SqlBackend
     , MonadLogger m
     , MonadMask m
     , MonadUnliftIO m
     )
  => ConnectionPool
  -> [Filter a]
  -> (a -> m b)
  -> m b
getLatestStateExtract dbConnection filters extract = do
  let stateName = Text.unpack (Persist.unEntityNameHS (Persist.getEntityHaskellName (Persist.entityDef (Proxy @a))))
  retry ("Getting " <> stateName <> " Utxo") $ do
    runSqlM
      ( (Persist.entityVal <$>)
          <$> Persist.selectFirst ([(Persist.symbolToField @"closingSlotNo") ==. Nothing] <> filters) []
      )
      dbConnection
      >>= maybe (MonadIO.liftIO (Tasty.assertFailure ("Unable to get " <> stateName <> " Utxo"))) extract

getLatestState
  :: forall a m
   . ( SymbolToField "closingSlotNo" a (Maybe CA.SlotNo)
     , PersistRecordBackend a SqlBackend
     , MonadLogger m
     , MonadMask m
     , MonadUnliftIO m
     )
  => ConnectionPool
  -> m a
getLatestState dbConnection = getLatestStateExtract dbConnection [] pure
