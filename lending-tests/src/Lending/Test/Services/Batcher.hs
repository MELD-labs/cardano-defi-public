{-# LANGUAGE NamedFieldPuns #-}

module Lending.Test.Services.Batcher
  ( indexPoolAfterApplyingSpec
  , indexPoolAfterSupplySpec
  , indexPoolAfterWithdrawSpec
  , indexPoolAfterBorrowSpec
  , indexPoolAfterRepaySpec
  , indexPoolAfterApplyingSetCollateralSpec
  , indexPoolAfterClearBorrowingSpec
  , indexPoolAfterClearSupplyingSpec
  , indexAccountAfterBorrowingMeldSpec
  , indexPoolAfterClearBorrowingMeldSpec
  , indexAccountAfterLiquidatingAccountSpec
  )
where

import Cardano.Api qualified as CA
import Cardano.Index.PersistLens (view)
import Control.Monad (void, (>=>))
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger (LoggingT)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Maybe qualified as Maybe
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB))
import Database.Persist ((==.))
import Database.Persist qualified as Persist
import Database.Persist.Sql qualified as Persist
import Test.Tasty.HUnit qualified as Tasty

import Data.Tagged (Tagged (unTagged))
import Lending.Index.Account (Account, EntityField (AccountClosingSlotNo, AccountHasRequests))
import Lending.Index.Manager (Manager (Manager, managerDatum))
import Lending.Index.Pool (EntityField (PoolClosingSlotNo, PoolSlotNo), Pool)
import Lending.Services.Transactions.Utils (extractAccountInput, extractPoolInput)
import Lending.Test.Api.Account
  ( borrowRequest
  , queryLiquidatedAccounts
  , repayRequest
  , supplyRequest
  , testClearBorrowingMeldRequest
  , testClearBorrowingRequest
  , testClearSupplyingRequest
  , withdrawRequest
  )
import Lending.Test.Common (getLatestState, retry)
import Lending.Test.Env
  ( IntegTest
  , SingleUserIntegTest
  , TestEnv (TestEnv, dbConnection)
  , TestUserCredential (TestUserCredential, tucAddress)
  , meldAsset
  , runSqlM
  )
import Lending.Types
  ( CumulativeRate
  , ManagerDatum (ManagerDatum, mdRiskParameters)
  , PoolDatum (PoolDatum, pdAssets)
  , RiskParameters (rpAssetClassData)
  , aiBorrowAmount
  , aiCumulatedInterestRateBorrowing
  , aiCumulatedInterestRateSupplying
  , aiSupplyAmount
  )
import Lending.Types.Account
  ( AccountDatum (AccountDatum, adBorrowings, adSupplies)
  , ClearRequest (ClearBorrowing, ClearSupplying)
  , Request
    ( BorrowRequest
    , RepayRequest
    , SupplyRequest
    , WithdrawRequest
    , brAmount
    , brAsset
    , rrAmount
    , rrAsset
    , srAmount
    , srAsset
    , wrAmount
    , wrAsset
    )
  )
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (convertTo)
import Plutarch.Extra.AssetClass (fromPlutusAssetClass)
import TxBuilder.Api (valueInTxOut)
import TxBuilder.Api.Types
  ( UtxoInput (uiTxOut)
  , UtxoInputWithDatum (UtxoInputWithDatum, uiwdDatum, uiwdUtxo)
  )

defaultPercent :: CumulativeRate
defaultPercent = 1

queryLatestAppliedAccount :: Persist.ConnectionPool -> LoggingT IO (Persist.Entity Account)
queryLatestAppliedAccount =
  runSqlM
    (Persist.selectFirst [AccountClosingSlotNo ==. Nothing, AccountHasRequests Persist.==. False] [])
    >=> maybe (MonadIO.liftIO (Tasty.assertFailure "Unable to find Account Utxo")) pure

queryLatestPool :: Persist.ConnectionPool -> LoggingT IO (Persist.Entity Pool)
queryLatestPool =
  runSqlM
    (Persist.selectFirst [PoolClosingSlotNo ==. Nothing] [])
    >=> maybe (MonadIO.liftIO (Tasty.assertFailure "Unable to find Pool Utxo")) pure

queryPreviousPool :: CA.SlotNo -> Persist.ConnectionPool -> LoggingT IO (Persist.Entity Pool)
queryPreviousPool slotNo =
  runSqlM
    (Persist.selectFirst [PoolClosingSlotNo ==. Just slotNo] [])
    >=> maybe (MonadIO.liftIO (Tasty.assertFailure "Unable to find Pool Utxo")) pure

indexPoolAfterApplyingClearRequest :: (Asset, ClearRequest) -> IntegTest
indexPoolAfterApplyingClearRequest (asset, clearRequest) TestEnv {dbConnection} = do
  newAccount <-
    retry "Getting latest account" $
      queryLatestAppliedAccount dbConnection
  let accountInput = extractAccountInput newAccount
  MonadIO.liftIO $ do
    Tasty.assertBool "Should be able to extract account" (Maybe.isJust accountInput)
    Tasty.assertBool "Should be zero amount" $ validAccountDatum (Maybe.fromJust accountInput)
  where
    validAccountDatum :: UtxoInputWithDatum AccountDatum -> Bool
    validAccountDatum
      UtxoInputWithDatum
        { uiwdDatum = AccountDatum {adSupplies = supplyAssets, adBorrowings = borrowAssets}
        } = do
        case clearRequest of
          ClearSupplying _ -> 0 `elem` Map.lookup asset supplyAssets
          ClearBorrowing _ _ -> 0 `elem` Map.lookup asset borrowAssets

indexPoolAfterApplyingSpec :: Request -> IntegTest
indexPoolAfterApplyingSpec request TestEnv {dbConnection} = do
  _ <-
    retry "Getting latest account" $
      queryLatestAppliedAccount dbConnection

  poolDB <-
    retry "Getting latest Pool Utxo" $
      queryLatestPool dbConnection

  oldPoolDB <-
    retry "Getting previous Pool Utxo" $
      queryPreviousPool (view PoolSlotNo poolDB) dbConnection

  Manager {managerDatum = JSONB ManagerDatum {mdRiskParameters}} <- getLatestState dbConnection

  let poolInput = extractPoolInput poolDB
      oldPool = extractPoolInput oldPoolDB
      (requestAsset, amount) =
        case request of
          SupplyRequest {srAsset, srAmount} -> (srAsset, CA.Quantity (unTagged srAmount))
          WithdrawRequest {wrAsset, wrAmount} -> (wrAsset, CA.Quantity (unTagged wrAmount))
          BorrowRequest {brAsset, brAmount} -> (brAsset, CA.Quantity (unTagged brAmount))
          RepayRequest {rrAsset, rrAmount} -> (rrAsset, CA.Quantity (unTagged rrAmount))

      requestAssetId =
        Maybe.fromMaybe
          (error "FIXME")
          (Map.lookup requestAsset mdRiskParameters >>= fromPlutusAssetClass . rpAssetClassData)

      lendingInfo = (aiSupplyAmount <$>) . Map.lookup requestAsset

      lendingRate = (aiCumulatedInterestRateSupplying <$>) . Map.lookup requestAsset

      borrowingInfo = (aiBorrowAmount <$>) . Map.lookup requestAsset

      borrowingRate = (aiCumulatedInterestRateBorrowing <$>) . Map.lookup requestAsset

      validPoolValue :: UtxoInputWithDatum PoolDatum -> UtxoInputWithDatum PoolDatum -> Bool
      validPoolValue
        UtxoInputWithDatum {uiwdUtxo = poolUtxo}
        UtxoInputWithDatum {uiwdUtxo = oldPoolUtxo} =
          valueInTxOut (uiTxOut poolUtxo)
            == case request of
              SupplyRequest {} ->
                CA.valueFromList [(requestAssetId, amount)] <> valueInTxOut (uiTxOut oldPoolUtxo)
              WithdrawRequest {} ->
                CA.negateValue (CA.valueFromList [(requestAssetId, amount)]) <> valueInTxOut (uiTxOut oldPoolUtxo)
              BorrowRequest {} ->
                CA.negateValue (CA.valueFromList [(requestAssetId, amount)]) <> valueInTxOut (uiTxOut oldPoolUtxo)
              RepayRequest {} ->
                CA.valueFromList [(requestAssetId, amount)] <> valueInTxOut (uiTxOut oldPoolUtxo)

      validPoolDatum :: UtxoInputWithDatum PoolDatum -> UtxoInputWithDatum PoolDatum -> Bool
      validPoolDatum
        UtxoInputWithDatum {uiwdDatum = PoolDatum {pdAssets = poolSupplies}}
        UtxoInputWithDatum {uiwdDatum = PoolDatum {pdAssets = oldPoolSupplies}} =
          -- TODO: estimate new cumulative rate
          case request of
            SupplyRequest {srAmount} ->
              (lendingRate poolSupplies == Just defaultPercent)
                && ( Just
                      ( sum (lendingInfo oldPoolSupplies)
                          + convertTo defaultPercent srAmount
                      )
                      == lendingInfo poolSupplies
                   )
            WithdrawRequest {wrAmount} ->
              Just
                ( sum (lendingInfo oldPoolSupplies)
                    - convertTo (Maybe.fromJust (lendingRate poolSupplies)) wrAmount
                )
                == lendingInfo poolSupplies
            BorrowRequest {brAmount} ->
              Just
                ( sum (borrowingInfo oldPoolSupplies)
                    + convertTo (Maybe.fromJust (borrowingRate poolSupplies)) brAmount
                )
                == borrowingInfo poolSupplies
            RepayRequest {rrAmount} ->
              Just
                ( sum (borrowingInfo oldPoolSupplies)
                    - convertTo (Maybe.fromJust (borrowingRate poolSupplies)) rrAmount
                )
                == borrowingInfo poolSupplies
  MonadIO.liftIO $ do
    Tasty.assertBool "Should be able to extract pools" (Maybe.isJust poolInput && Maybe.isJust oldPool)
    Tasty.assertBool "Pool value must match" $ validPoolValue (Maybe.fromJust poolInput) (Maybe.fromJust oldPool)
    Tasty.assertBool "Pool datum must match" $ validPoolDatum (Maybe.fromJust poolInput) (Maybe.fromJust oldPool)

indexPoolAfterSupplySpec :: IntegTest
indexPoolAfterSupplySpec = indexPoolAfterApplyingSpec supplyRequest

indexPoolAfterWithdrawSpec :: SingleUserIntegTest
indexPoolAfterWithdrawSpec TestUserCredential {tucAddress} = indexPoolAfterApplyingSpec (withdrawRequest tucAddress)

indexPoolAfterBorrowSpec :: SingleUserIntegTest
indexPoolAfterBorrowSpec TestUserCredential {tucAddress} = indexPoolAfterApplyingSpec (borrowRequest tucAddress)

indexPoolAfterRepaySpec :: IntegTest
indexPoolAfterRepaySpec = indexPoolAfterApplyingSpec repayRequest

indexPoolAfterClearBorrowingSpec :: SingleUserIntegTest
indexPoolAfterClearBorrowingSpec
  TestUserCredential {tucAddress} = indexPoolAfterApplyingClearRequest (testClearBorrowingRequest tucAddress)

indexPoolAfterClearSupplyingSpec :: SingleUserIntegTest
indexPoolAfterClearSupplyingSpec
  TestUserCredential {tucAddress} = indexPoolAfterApplyingClearRequest (testClearSupplyingRequest tucAddress)

indexPoolAfterClearBorrowingMeldSpec :: SingleUserIntegTest
indexPoolAfterClearBorrowingMeldSpec
  TestUserCredential {tucAddress} = indexPoolAfterApplyingClearRequest (testClearBorrowingMeldRequest tucAddress)

indexPoolAfterApplyingSetCollateralSpec :: IntegTest
indexPoolAfterApplyingSetCollateralSpec TestEnv {dbConnection} = do
  _ <-
    retry "Getting latest account" $
      queryLatestAppliedAccount dbConnection

  poolDB <-
    retry "Getting latest Pool Utxo" $
      queryLatestPool dbConnection

  oldPoolDB <-
    retry "Getting previous Pool Utxo" $
      queryPreviousPool (view PoolSlotNo poolDB) dbConnection

  let poolInput = extractPoolInput poolDB
      oldPool = extractPoolInput oldPoolDB
  MonadIO.liftIO $ do
    Tasty.assertBool "Should be able to extract pools" (Maybe.isJust poolInput && Maybe.isJust oldPool)

indexAccountAfterBorrowingMeldSpec :: IntegTest
indexAccountAfterBorrowingMeldSpec TestEnv {dbConnection} = do
  newAccount <-
    retry "Getting latest account" $
      queryLatestAppliedAccount dbConnection
  let accountInput = extractAccountInput newAccount
  MonadIO.liftIO $ do
    Tasty.assertBool "Should be able to extract account" (Maybe.isJust accountInput)
    Tasty.assertBool "Should be zero amount" $ validAccountDatum (Maybe.fromJust accountInput)
  where
    validAccountDatum :: UtxoInputWithDatum AccountDatum -> Bool
    validAccountDatum
      UtxoInputWithDatum
        { uiwdDatum = AccountDatum {adBorrowings = borrowAssets}
        } = fromJust (Map.lookup meldAsset borrowAssets) > 0

indexAccountAfterLiquidatingAccountSpec :: IntegTest
indexAccountAfterLiquidatingAccountSpec TestEnv {dbConnection} =
  void $
    retry "Getting liquidated account" $
      queryLiquidatedAccounts dbConnection
