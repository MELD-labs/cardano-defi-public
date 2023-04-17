{-# LANGUAGE OverloadedStrings #-}

module Lending.Test.Integ (integTests) where

import Test.Tasty (TestTree)

import Lending.Test.Api.Account
  ( testBorrow
  , testBorrowMeldAndAda
  , testClearBorrowing
  , testClearSupplying
  , testCloseAccount
  , testCreateAccount
  , testHistoryApiAccount
  , testMigrateAccountSpec
  , testQueryLiquidatedAccount
  , testRepay
  , testSetCollateral
  , testSetCollateralWithRequest
  , testSupply
  , testWithdraw
  )
import Lending.Test.Api.Manager (getManagerApiSpec, testMigrateManagerSpec, testUpdateManagerSpec)
import Lending.Test.Api.Pool (getPoolApiSpec, testMigratePoolSpec, testTreasuryPoolSpec)
import Lending.Test.Api.SyncStatus (testSyncStatusApi)
import Lending.Test.Env
  ( TestUserMode (SingleUserMode)
  , after
  , testCaseWithEnv
  , testCaseWithEnvSingleUser
  , testGroupWithEnv
  , withTestEnv
  )
import Lending.Test.Faucet.MintToken (testFaucetStatus, testMintToken, testPrepareFaucet)
import Lending.Test.Index.Account (indexAccountUpdated)
import Lending.Test.Index.Manager (indexManagerSpec)
import Lending.Test.Index.Oracle (indexOracleSpec)
import Lending.Test.Index.Pool (indexPoolSpec)
import Lending.Test.LiquidationBot (liquidateAccountSpec)
import Lending.Test.Services.Batcher
  ( indexAccountAfterBorrowingMeldSpec
  , indexAccountAfterLiquidatingAccountSpec
  , indexPoolAfterApplyingSetCollateralSpec
  , indexPoolAfterBorrowSpec
  , indexPoolAfterClearBorrowingSpec
  , indexPoolAfterClearSupplyingSpec
  , indexPoolAfterRepaySpec
  , indexPoolAfterSupplySpec
  , indexPoolAfterWithdrawSpec
  )
import Lending.Test.Services.Oracle
  ( feedPriceToMockApi
  , testInitOracleSpec
  , testUpdateMeldPriceSpec
  )

-- Prefix [Integ] to exclude this test group from unit tests
integTests :: TestTree
integTests =
  withTestEnv SingleUserMode $
    testGroupWithEnv
      "[Integ] Lending Integration tests"
      [ testGroupWithEnv
          "Faucet"
          [ testCaseWithEnv "Prepare faucet" testPrepareFaucet
          , testCaseWithEnv "Get Faucet Status" testFaucetStatus
              `after` "Prepare faucet"
          , testCaseWithEnvSingleUser "Init test user" testMintToken
              `after` "Get Faucet Status"
          ]
      , testGroupWithEnv
          "Lending API, indexers and services"
          [ testCaseWithEnv "Index Manager" indexManagerSpec
          , testCaseWithEnv "Index Oracle" indexOracleSpec
          , testCaseWithEnv "Index Pool" indexPoolSpec
          , testCaseWithEnv "Get Manager" getManagerApiSpec
              `after` "Index Manager"
          , testCaseWithEnv "Get Pool" getPoolApiSpec
              `after` "Index Pool"
          , testCaseWithEnv "Init Oracle" testInitOracleSpec
              `after` "Index Oracle"
          , testCaseWithEnv "Admin Update Manager" testUpdateManagerSpec
              `after` "Index Manager"
              `after` "Init Oracle"
          , testCaseWithEnvSingleUser "Create Account" testCreateAccount
              `after` "Init test user"
              `after` "Admin Update Manager"
              `after` "Init Oracle"
          , testCaseWithEnv "Index Account" indexAccountUpdated
              `after` "Create Account"
          , testCaseWithEnvSingleUser "Supply Account" testSupply
              `after` "Index Account"
              `after` "Index Pool"
          , testCaseWithEnv "Index Updating Account" indexAccountUpdated
              `after` "Supply Account"
          , testCaseWithEnv "Apply Supply to Pool" indexPoolAfterSupplySpec
              `after` "Supply Account"
          , testCaseWithEnvSingleUser "Withdraw Account" testWithdraw
              `after` "Apply Supply to Pool"
          , testCaseWithEnvSingleUser "Apply Withdraw to Pool" indexPoolAfterWithdrawSpec
              `after` "Withdraw Account"
          , testCaseWithEnvSingleUser "Set Collateral Account" testSetCollateral
              `after` "Apply Withdraw to Pool"
          , testCaseWithEnv "Apply Set Collateral to Pool" indexPoolAfterApplyingSetCollateralSpec
              `after` "Set Collateral Account"
          , testCaseWithEnvSingleUser "Have Request And Collateral Account" testSetCollateralWithRequest
              `after` "Apply Set Collateral to Pool"
          , testCaseWithEnv "Update Request and Set Collateral to Pool" indexPoolAfterApplyingSetCollateralSpec
              `after` "Have Request And Collateral Account"
          , testCaseWithEnvSingleUser "Borrow Account" testBorrow
              `after` "Update Request and Set Collateral to Pool"
          , testCaseWithEnvSingleUser "Apply Borrow to Pool" indexPoolAfterBorrowSpec
              `after` "Borrow Account"
          , testCaseWithEnvSingleUser "Repay Account" testRepay
              `after` "Apply Borrow to Pool"
          , testCaseWithEnv "Apply Repay to Pool" indexPoolAfterRepaySpec
              `after` "Repay Account"
          , testCaseWithEnvSingleUser "User Clear Borrowing" testClearBorrowing
              `after` "Apply Repay to Pool"
          , testCaseWithEnvSingleUser "Batcher Apply Clear Borrowing to Pool" indexPoolAfterClearBorrowingSpec
              `after` "User Clear Borrowing"
          , testCaseWithEnvSingleUser "User Clear Supplying" testClearSupplying
              `after` "Batcher Apply Clear Borrowing to Pool"
          , testCaseWithEnvSingleUser "Batcher Apply Clear Supplying to Pool" indexPoolAfterClearSupplyingSpec
              `after` "User Clear Supplying"
          , testCaseWithEnvSingleUser "User supply Ada Meld and borrow Meld" testBorrowMeldAndAda
              `after` "Batcher Apply Clear Supplying to Pool"
          , testCaseWithEnv "Batcher apply borrowing meld" indexAccountAfterBorrowingMeldSpec
              `after` "User supply Ada Meld and borrow Meld"
          , testCaseWithEnv "Update Meld price" testUpdateMeldPriceSpec
              `after` "Batcher apply borrowing meld"
          , testCaseWithEnv "Query liquidated account" testQueryLiquidatedAccount
              `after` "Update Meld price"
          , testCaseWithEnvSingleUser "Liquidate Account" liquidateAccountSpec
              `after` "Query liquidated account"
          , testCaseWithEnv "Batcher apply Liquidate request" indexAccountAfterLiquidatingAccountSpec
              `after` "Liquidate Account"
          , testCaseWithEnvSingleUser "Close Account" testCloseAccount
              `after` "Batcher apply Liquidate request"
          , testCaseWithEnv "Feed price to mock API" feedPriceToMockApi
              `after` "Close Account"
          , testCaseWithEnv "Supply Treasury Pool API" (testTreasuryPoolSpec (-2000000))
              `after` "Feed price to mock API"
          , -- TODO: Move this test before Supply treasury when we increase aiCumulatedInterestRate
            testCaseWithEnv "Withdraw Treasury Pool API" (testTreasuryPoolSpec 1000000)
              `after` "Supply Treasury Pool API"
          , testCaseWithEnv "History Account" testHistoryApiAccount
              `after` "Withdraw Treasury Pool API"
          , testCaseWithEnv "Migrate Pool api" testMigratePoolSpec
              `after` "History Account"
          , testCaseWithEnv "Migrate Account" testMigrateAccountSpec
              `after` "Migrate Pool api"
          , testCaseWithEnv "Migrate Manager api" testMigrateManagerSpec
              `after` "Migrate Account"
          , testCaseWithEnv "Sync Status api" testSyncStatusApi
              `after` "Migrate Account"
          ]
      ]
