{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lending.Test.Functional.Update
  ( updateAccounts
  )
where

import Cardano.Api qualified as CA
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger (LoggingT)
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans qualified as MonadTrans
import Control.Monad.Trans.Accum qualified as Accum
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Tagged (Tagged (Tagged))
import Data.Text qualified as Text
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB))
import Database.Persist.Sql ((==.))
import Test.Tasty.HUnit qualified as Tasty
import UnliftIO.Async qualified as Async

import Cardano.Api.Extra.Tx (signSubmitAndWaitTx)
import Cardano.Index.Data.AddressText (AddressText (AddressText))
import Lending.Api.Client (updateAccountClient)
import Lending.Api.Types.Account
  ( UpdateAccountRequest
      ( UpdateAccountRequest
      , uarChangeAddress
      , uarClearRequests
      , uarCollateralUpdate
      , uarNormalRequests
      , uarRef
      , uarUtxos
      )
  , UpdateAccountResponse (UpdateAccountResponse, uarTx)
  , Utxos (Utxos)
  )
import Lending.Api.Types.Request (fromRequest)
import Lending.Core.Utils (fromCardanoAddressInEra)
import Lending.Index.Account (Account (Account, accountHasRequests, accountRef), EntityField (AccountUserNft))
import Lending.Test.Common
  ( getLatestStateExtract
  , getUserInputs
  , queryApi
  )
import Lending.Test.Env
  ( TestEnv
      ( TestEnv
      , apiClientEnv
      , dbConnection
      , nodeConnection
      )
  , TestUserAccount (TestUserAccount, tuaCredential, tuaUserNft)
  , TestUserCredential (TestUserCredential, tucAddress, tucSigningKey)
  , runOnNodeConnectionPool
  , testNetwork
  )
import Lending.Test.Functional.Asset (TestAsset, getAssetMap)
import Lending.Test.Functional.Types
  ( FunctionalTestM
  , TestAssetAmount (taaAmount, taaAssetName)
  , TestRequestInstruction (TestBorrow, TestCollateral, TestRepay, TestSupply, TestWithdraw)
  , TestUserMap
  , TestUserName
  )
import Lending.Test.InitAccount (generateAccount)
import Lending.Types.Account
  ( Request
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
import Lending.Types.Exchange (Actual)
import Plutarch.Extra.AssetClass (AssetClass)
import Plutarch.Extra.FixedDecimal (fixedNumerator)

ensureAccountInitialized :: TestEnv -> Set TestUserName -> FunctionalTestM ()
ensureAccountInitialized testEnv users = do
  initializedAccounts <- Accum.looks Map.keysSet
  let uninitializedAccounts = users \\ initializedAccounts
  Monad.unless (null uninitializedAccounts) $
    MonadTrans.lift (generateAccount uninitializedAccounts testEnv) >>= Accum.add

loadAccount :: TestUserName -> ReaderT TestUserMap (LoggingT IO) TestUserAccount
loadAccount testUserName =
  Reader.ask
    >>= MonadIO.liftIO
      . maybe (Tasty.assertFailure ("Unable to get account of test user " <> testUserName)) pure
      . Map.lookup testUserName

assertAccountHavingRequests :: TestEnv -> AssetClass -> Bool -> ReaderT TestUserMap (LoggingT IO) ()
assertAccountHavingRequests TestEnv {dbConnection} userNft isHavingRequests =
  getLatestStateExtract dbConnection [AccountUserNft ==. JSONB userNft] $ \Account {accountHasRequests} ->
    MonadIO.liftIO $
      Tasty.assertBool "Whether account is having requests doesn't match expected" $
        accountHasRequests == isHavingRequests

getAsset :: Map TestAsset Asset -> TestAsset -> ReaderT TestUserMap (LoggingT IO) Asset
getAsset assetMap testAsset =
  maybe
    (MonadIO.liftIO $ Tasty.assertFailure "Unable to get test asset")
    pure
    (Map.lookup testAsset assetMap)

getAmount :: TestAssetAmount -> Actual
getAmount = Tagged . fixedNumerator . taaAmount

setRequest :: UpdateAccountRequest -> Request -> ReaderT TestUserMap (LoggingT IO) UpdateAccountRequest
setRequest params@UpdateAccountRequest {uarNormalRequests = oldList} request = do
  requestApi <-
    MonadIO.liftIO $
      either
        (\_ -> Tasty.assertFailure "Unable to extract user request")
        pure
        (fromRequest testNetwork request)
  pure $ params {uarNormalRequests = requestApi : oldList}

setCollateral
  :: Map TestAsset Asset
  -> [TestAsset]
  -> UpdateAccountRequest
  -> ReaderT TestUserMap (LoggingT IO) UpdateAccountRequest
setCollateral assetMap assets params = do
  collaterals <- traverse (getAsset assetMap) assets
  pure $
    params
      { uarCollateralUpdate = Just (Map.fromList ((,True) <$> collaterals))
      }

toRequest
  :: Map TestAsset Asset
  -> CA.AddressInEra CA.BabbageEra
  -> UpdateAccountRequest
  -> TestRequestInstruction
  -> ReaderT TestUserMap (LoggingT IO) UpdateAccountRequest
toRequest assetMap _ initial (TestSupply testAssetAmount) = do
  asset <- getAsset assetMap (taaAssetName testAssetAmount)
  setRequest initial $
    SupplyRequest
      { srAsset = asset
      , srAmount = getAmount testAssetAmount
      }
toRequest assetMap user initial (TestWithdraw testAssetAmount) = do
  asset <- getAsset assetMap (taaAssetName testAssetAmount)
  setRequest initial $
    WithdrawRequest
      { wrAsset = asset
      , wrAmount = getAmount testAssetAmount
      , wrReceiver = fromCardanoAddressInEra user
      }
toRequest assetMap user initial (TestBorrow testAssetAmount) = do
  asset <- getAsset assetMap (taaAssetName testAssetAmount)
  setRequest initial $
    BorrowRequest
      { brAsset = asset
      , brAmount = getAmount testAssetAmount
      , brReceiver = fromCardanoAddressInEra user
      }
toRequest assetMap _ initial (TestRepay testAssetAmount) = do
  asset <- getAsset assetMap (taaAssetName testAssetAmount)
  setRequest initial $
    RepayRequest
      { rrAsset = asset
      , rrAmount = getAmount testAssetAmount
      }
toRequest assetMap _ initial (TestCollateral assets) = setCollateral assetMap assets initial

updateAccount :: TestEnv -> TestUserName -> [TestRequestInstruction] -> ReaderT TestUserMap (LoggingT IO) ()
updateAccount testEnv@TestEnv {dbConnection, nodeConnection, apiClientEnv} testUserName requests = do
  TestUserAccount
    { tuaCredential = TestUserCredential {tucSigningKey, tucAddress}
    , tuaUserNft
    } <-
    loadAccount testUserName
  Logger.logInfoN ("Submitting requests for " <> Text.pack testUserName)
  assertAccountHavingRequests testEnv tuaUserNft False
  (userTxIn, collateralUtxo) <- runOnNodeConnectionPool nodeConnection (getUserInputs tucAddress)
  assetMap <- getAssetMap dbConnection
  ref <- getLatestStateExtract dbConnection [AccountUserNft ==. JSONB tuaUserNft] (pure . accountRef)
  let initialRequest =
        UpdateAccountRequest
          { uarRef = ref
          , uarNormalRequests = []
          , uarCollateralUpdate = Nothing
          , uarChangeAddress = AddressText tucAddress
          , uarUtxos = Just $ Utxos userTxIn collateralUtxo
          , uarClearRequests = Map.empty
          }
  request <- Monad.foldM (toRequest assetMap tucAddress) initialRequest requests
  UpdateAccountResponse {uarTx} <-
    queryApi apiClientEnv $
      updateAccountClient request
  _ <- runOnNodeConnectionPool nodeConnection $ signSubmitAndWaitTx Nothing tucSigningKey uarTx
  Logger.logInfoN ("Submitted requests for " <> Text.pack testUserName)
  assertAccountHavingRequests testEnv tuaUserNft True
  Logger.logInfoN ("Waiting for pool processing requests for " <> Text.pack testUserName)
  assertAccountHavingRequests testEnv tuaUserNft False
  Logger.logInfoN ("Processed all requests for " <> Text.pack testUserName)

updateAccounts :: Map TestUserName [TestRequestInstruction] -> TestEnv -> FunctionalTestM ()
updateAccounts requests testEnv = do
  ensureAccountInitialized testEnv (Map.keysSet requests)
  Accum.readerToAccumT $
    Async.mapConcurrently_ (uncurry (updateAccount testEnv)) (Map.toList (Map.filter (not . null) requests))
