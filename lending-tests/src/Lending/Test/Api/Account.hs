{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Test.Api.Account
  ( supplyRequest
  , withdrawRequest
  , borrowRequest
  , repayRequest
  , testCreateAccount
  , testSupply
  , testWithdraw
  , testSetCollateral
  , testBorrow
  , testRepay
  , testCloseAccount
  , testClearBorrowing
  , testClearBorrowingRequest
  , testClearSupplyingRequest
  , testClearSupplying
  , testBorrowMeldAndAda
  , testLiquidateAccountSpec
  , testClearBorrowingMeldRequest
  , queryLiquidatedAccounts
  , testQueryLiquidatedAccount
  , testSetCollateralWithRequest
  , testMigrateAccountSpec
  , testHistoryApiAccount
  )
where

import Cardano.Api qualified as CA
import Control.Monad ((>=>))
import Control.Monad qualified as Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB))
import Database.Persist.Sql (ConnectionPool, Entity (entityVal), selectList, (==.))
import Test.Tasty.HUnit qualified as Tasty

import Cardano.Api.Extra.AssetId (AssetIdText (AssetIdText))
import Cardano.Api.Extra.Tx (signSubmitAndWaitTx)
import Cardano.Index.Data.AddressText (AddressText (AddressText))
import Lending.Api.Client
  ( closeAccountClient
  , historyAccountClient
  , liquidateAccountClient
  , migrateAccountInputClient
  , queryExceedLtvAccountsClient
  , updateAccountClient
  )
import Lending.Api.Types.Account
  ( CloseAccountRequest (CloseAccountRequest)
  , CloseAccountResponse (CloseAccountResponse, clarTx)
  , ExceedLtvAccount (elaUserNft)
  , HistoryAccountRequest (HistoryAccountRequest)
  , HistoryAccountResponse (harHistoryTx)
  , LiquidateAccountRequest (LiquidateAccountRequest)
  , LiquidateAccountResponse (LiquidateAccountResponse, larTx)
  , QueryExceedLtvAccountsResponse (QueryExceedLtvAccountsResponse, qelarAccountList)
  , UpdateAccountRequest (UpdateAccountRequest)
  , UpdateAccountResponse (UpdateAccountResponse, uarTx)
  )
import Lending.Api.Types.Request (fromClearRequest, fromRequest)
import Lending.Core.AccountValue (AccountId (AccountId))
import Lending.Core.Utils (fromCardanoAddressInEra)
import Lending.Index.Account
  ( Account (Account, accountOriginalRef, accountRef)
  , EntityField (AccountClosingSlotNo, AccountHasRequests, AccountUserNft)
  )
import Lending.Services.Transactions.Utils (extractAccountInput)
import Lending.Test.Common
  ( getLatestStateExtract
  , queryApi
  , retry
  )
import Lending.Test.Env
  ( IntegTest
  , SingleUserIntegTest
  , TestEnv
    ( TestEnv
    , apiClientEnv
    , dbConnection
    , nodeConnection
    )
  , TestUserCredential (TestUserCredential, tucAddress, tucSigningKey)
  , adaAsset
  , meldAsset
  , runOnNodeConnectionPool
  , runSqlM
  , testNetwork
  )
import Lending.Test.InitAccount (createAccount)
import Lending.Types (ClearRequest (ClearBorrowing, ClearSupplying))
import Lending.Types.Account
  ( AccountDatum (AccountDatum, adBorrowings, adSupplies)
  , Request
    ( BorrowRequest
    , RepayRequest
    , SupplyRequest
    , WithdrawRequest
    , brAmount
    , brAsset
    , brReceiver
    , rrAmount
    , rrAsset
    , srAmount
    , srAsset
    , wrAmount
    , wrAsset
    , wrReceiver
    )
  )
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Receipt)
import Plutarch.Extra.AssetClass (toPlutusAssetClass)
import TxBuilder.Api.Types (UtxoInputWithDatum (UtxoInputWithDatum, uiwdDatum))

testCreateAccount :: SingleUserIntegTest
testCreateAccount = (Monad.void .) . createAccount [supplyRequest]

testUpdateAccount :: [Request] -> Maybe (Map Asset Bool) -> Maybe (Map Asset ClearRequest) -> SingleUserIntegTest
testUpdateAccount
  requests
  collateralAssetUpdate
  clearRequest
  TestUserCredential {tucAddress, tucSigningKey}
  TestEnv
    { nodeConnection
    , dbConnection
    , apiClientEnv
    } = do
    accountRef <- getAccountRef dbConnection
    requestApi <-
      MonadIO.liftIO $
        either
          (\_ -> Tasty.assertFailure "Unable to extract user request")
          pure
          (traverse (fromRequest testNetwork) requests)
    clearRequestsApi <-
      MonadIO.liftIO $
        either
          (\_ -> Tasty.assertFailure "Unable to extract user request")
          pure
          (traverse (fromClearRequest testNetwork) $ fromMaybe Map.empty clearRequest)
    UpdateAccountResponse {uarTx} <-
      queryApi apiClientEnv $
        updateAccountClient $
          UpdateAccountRequest
            accountRef
            requestApi
            collateralAssetUpdate
            clearRequestsApi
            (AddressText tucAddress)
            Nothing
    -- Submit tx and confirm tx in onchain
    Monad.void $ runOnNodeConnectionPool nodeConnection $ signSubmitAndWaitTx Nothing tucSigningKey uarTx

testLiquidateAccount :: Maybe (Map Asset ClearRequest) -> SingleUserIntegTest
testLiquidateAccount
  clearRequest
  TestUserCredential {tucAddress, tucSigningKey}
  TestEnv
    { nodeConnection
    , dbConnection
    , apiClientEnv
    } = do
    accountRef <- getAccountRef dbConnection
    clearRequestsApi <-
      MonadIO.liftIO $
        either
          (\_ -> Tasty.assertFailure "Unable to extract account user NFT")
          pure
          (traverse (fromClearRequest testNetwork) $ fromMaybe Map.empty clearRequest)
    let debt = Map.fromList [(0, 250_000)]
        collateral = Map.fromList [(0, 275_000)]
    LiquidateAccountResponse {larTx} <-
      queryApi apiClientEnv $
        liquidateAccountClient $
          LiquidateAccountRequest
            (AddressText tucAddress)
            Nothing
            accountRef
            clearRequestsApi
            debt
            collateral
    -- Submit tx and confirm tx in onchain
    Monad.void $ runOnNodeConnectionPool nodeConnection $ signSubmitAndWaitTx Nothing tucSigningKey larTx

supplyRequest :: Request
supplyRequest =
  SupplyRequest
    { srAsset = adaAsset
    , srAmount = 10_000_000
    }

withdrawRequest :: CA.AddressInEra CA.BabbageEra -> Request
withdrawRequest address =
  WithdrawRequest
    { wrAsset = adaAsset
    , wrAmount = 5_000_000
    , wrReceiver = fromCardanoAddressInEra address
    }

borrowRequest :: CA.AddressInEra CA.BabbageEra -> Request
borrowRequest address =
  BorrowRequest
    { brAsset = adaAsset
    , brAmount = 1_000_000
    , brReceiver = fromCardanoAddressInEra address
    }

repayRequest :: Request
repayRequest =
  RepayRequest
    { rrAsset = adaAsset
    , rrAmount = 500_000
    }

supplyAdaRequest :: Request
supplyAdaRequest =
  SupplyRequest
    { srAsset = adaAsset
    , srAmount = 2_000_000
    }

supplyMeldRequest :: Request
supplyMeldRequest =
  SupplyRequest
    { srAsset = meldAsset
    , srAmount = 2_000_000
    }

borrowMeldRequest :: CA.AddressInEra CA.BabbageEra -> Request
borrowMeldRequest address =
  BorrowRequest
    { brAsset = meldAsset
    , brAmount = 1_000_000
    , brReceiver = fromCardanoAddressInEra address
    }

setCollateralRequest :: Maybe (Map Asset Bool)
setCollateralRequest = Just (Map.singleton adaAsset True)

testClearBorrowingRequest :: CA.AddressInEra CA.BabbageEra -> (Asset, ClearRequest)
testClearBorrowingRequest address =
  (adaAsset, ClearBorrowing 1_000_000 (fromCardanoAddressInEra address))

testClearBorrowingMeldRequest :: CA.AddressInEra CA.BabbageEra -> (Asset, ClearRequest)
testClearBorrowingMeldRequest address =
  (meldAsset, ClearBorrowing 2_000_000 (fromCardanoAddressInEra address))

testClearSupplyingRequest :: CA.AddressInEra CA.BabbageEra -> (Asset, ClearRequest)
testClearSupplyingRequest address =
  (adaAsset, ClearSupplying (fromCardanoAddressInEra address))

testSupply :: SingleUserIntegTest
testSupply = testUpdateAccount [supplyRequest] Nothing Nothing

-- Creating updating account request doesn't check if the request can be applied
-- so we don't have to mind the requests order.
testWithdraw :: SingleUserIntegTest
testWithdraw user@TestUserCredential {tucAddress} = testUpdateAccount [withdrawRequest tucAddress] Nothing Nothing user

testSetCollateral :: SingleUserIntegTest
testSetCollateral = testUpdateAccount [] setCollateralRequest Nothing

testSetCollateralWithRequest :: SingleUserIntegTest
testSetCollateralWithRequest = testUpdateAccount [supplyRequest] setCollateralRequest Nothing

testBorrow :: SingleUserIntegTest
testBorrow user@TestUserCredential {tucAddress} = testUpdateAccount [borrowRequest tucAddress] Nothing Nothing user

testRepay :: SingleUserIntegTest
testRepay = testUpdateAccount [repayRequest] Nothing Nothing

testClearSupplying :: SingleUserIntegTest
testClearSupplying user@TestUserCredential {tucAddress} =
  testUpdateAccount [] Nothing (Just $ Map.fromAscList [testClearSupplyingRequest tucAddress]) user

testClearBorrowing :: SingleUserIntegTest
testClearBorrowing user@TestUserCredential {tucAddress} =
  testUpdateAccount [] Nothing (Just $ Map.fromAscList [testClearBorrowingRequest tucAddress]) user

testBorrowMeldAndAda :: SingleUserIntegTest
testBorrowMeldAndAda user@TestUserCredential {tucAddress} =
  let borrowAdaRequest = BorrowRequest adaAsset 200_000 $ fromCardanoAddressInEra tucAddress
   in testUpdateAccount
        [supplyAdaRequest, supplyMeldRequest, borrowMeldRequest tucAddress, borrowAdaRequest]
        Nothing
        Nothing
        user

testLiquidateAccountSpec :: SingleUserIntegTest
testLiquidateAccountSpec user@TestUserCredential {tucAddress} =
  testLiquidateAccount
    (Just $ Map.fromList [testClearBorrowingMeldRequest tucAddress])
    user

testCloseAccount :: SingleUserIntegTest
testCloseAccount
  TestUserCredential {tucAddress, tucSigningKey}
  TestEnv {nodeConnection, apiClientEnv, dbConnection} = do
    accountOwnerNft <- getClosableAccountId dbConnection
    CloseAccountResponse {clarTx} <-
      queryApi apiClientEnv $
        closeAccountClient $
          CloseAccountRequest accountOwnerNft (AddressText tucAddress) Nothing
    -- Submit tx and confirm tx in onchain
    Monad.void $ runOnNodeConnectionPool nodeConnection $ signSubmitAndWaitTx Nothing tucSigningKey clarTx

testQueryLiquidatedAccount :: IntegTest
testQueryLiquidatedAccount
  TestEnv {apiClientEnv, dbConnection} = do
    QueryExceedLtvAccountsResponse {qelarAccountList} <-
      queryApi apiClientEnv queryExceedLtvAccountsClient
    let AssetIdText actualUserNft = elaUserNft $ head qelarAccountList
    Monad.void $
      getLatestStateExtract @Account dbConnection [AccountUserNft ==. JSONB (toPlutusAssetClass actualUserNft)] pure

testMigrateAccountSpec :: IntegTest
testMigrateAccountSpec
  TestEnv
    { apiClientEnv
    } = Monad.void $ queryApi apiClientEnv migrateAccountInputClient

getAccountRef :: (MonadLogger m, MonadMask m, MonadUnliftIO m) => ConnectionPool -> m CA.TxIn
getAccountRef dbConnection =
  getLatestStateExtract dbConnection [] (pure . accountRef)

getAccountOriginalRef :: (MonadLogger m, MonadMask m, MonadUnliftIO m) => ConnectionPool -> m CA.TxIn
getAccountOriginalRef dbConnection =
  getLatestStateExtract dbConnection [] (pure . accountOriginalRef)

queryLiquidatedAccounts :: ConnectionPool -> LoggingT IO [Entity Account]
queryLiquidatedAccounts = do
  runSqlM
    (selectList [AccountClosingSlotNo ==. Nothing, AccountHasRequests ==. False] [])
    >=> \newAccs ->
      if length newAccs == 2 then pure newAccs else MonadIO.liftIO (Tasty.assertFailure "Cannot find 2 Account Utxos")

getClosableAccountId :: ConnectionPool -> LoggingT IO CA.TxIn
getClosableAccountId dbConnection = do
  newAccs <-
    retry "Getting closable account" $
      queryLiquidatedAccounts dbConnection
  let account = find isClosableAccount newAccs
      Account {accountRef} = entityVal $ fromJust account

  pure accountRef

checkValueInMapIsZero :: Map Asset Receipt -> Bool
checkValueInMapIsZero = all (== 0)

isClosableAccount :: Entity Account -> Bool
isClosableAccount eAccount =
  let UtxoInputWithDatum
        { uiwdDatum = AccountDatum {adSupplies, adBorrowings}
        } = fromJust $ extractAccountInput eAccount
   in checkValueInMapIsZero adSupplies && checkValueInMapIsZero adBorrowings

testHistoryApiAccount :: IntegTest
testHistoryApiAccount
  TestEnv
    { dbConnection
    , apiClientEnv
    } = do
    accountId <- getAccountOriginalRef dbConnection
    response <-
      queryApi apiClientEnv $
        historyAccountClient $
          HistoryAccountRequest (AccountId accountId) Nothing Nothing
    MonadIO.liftIO $
      Tasty.assertBool "History account wrong" (length (harHistoryTx response) == 11)
