{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Test.Functional (functionalTests) where

import Control.Concurrent qualified as Concurrent
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Accum qualified as Accum
import Data.Foldable qualified as Foldable
import Data.Yaml qualified as Yaml
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit qualified as Tasty

import Lending.Index.Manager (Manager)
import Lending.Index.Oracle (Oracle)
import Lending.Index.Pool (Pool)
import Lending.Test.Common (getLatestState)
import Lending.Test.Env
  ( TestEnv (dbConnection, liquidatorOperator)
  , TestUserMode (SingleUserMode)
  , after
  , testGroupWithEnv
  , withTestEnv
  )
import Lending.Test.Faucet.MintToken (testPrepareFaucet)
import Lending.Test.Functional.Oracle (updateOracle)
import Lending.Test.Functional.Types
  ( FunctionalTestM
  , TestScenario (TestScenario, tsDisable, tsName, tsSteps)
  , TestStep (TestStepOracle, TestStepUpdate, TestStepWait)
  )
import Lending.Test.Functional.Update (updateAccounts)
import Lending.Test.InitAccount (mintTestTokens)

isTestExtension :: FilePath -> Bool
isTestExtension file = FilePath.isExtensionOf "yml" file || FilePath.isExtensionOf "yaml" file

runStep :: TestStep -> TestEnv -> FunctionalTestM ()
runStep (TestStepUpdate requestMap) testEnv = updateAccounts requestMap testEnv
runStep (TestStepOracle priceMap) testEnv = updateOracle priceMap testEnv
runStep (TestStepWait waitInSeconds) _ = MonadIO.liftIO (Concurrent.threadDelay (waitInSeconds * 1_000_000))

waitTestName :: TestName
waitTestName = "Wait for global state initialization"

fundTokenForLiquidatorTestName :: TestName
fundTokenForLiquidatorTestName = "Fund test token for liquidator"

waitForGlobalStateInitialization :: IO TestEnv -> TestTree
waitForGlobalStateInitialization getTestEnv =
  Tasty.testCase waitTestName $ do
    getTestEnv
      >>= Foldable.sequence_
        . ([run . getLatestState @Pool, run . getLatestState @Oracle, run . getLatestState @Manager] <*>)
        . pure
        . dbConnection
    getTestEnv >>= run . testPrepareFaucet
  where
    run = Logger.runStdoutLoggingT . Monad.void

fundTokenForLiquidator :: IO TestEnv -> TestTree
fundTokenForLiquidator getTestEnv =
  Tasty.testCase fundTokenForLiquidatorTestName $
    getTestEnv
      >>= (Logger.runStdoutLoggingT . (mintTestTokens =<< liquidatorOperator))

runTest :: TestScenario -> IO TestEnv -> TestTree
runTest TestScenario {tsName, tsSteps} getTestEnv =
  Tasty.testCase tsName $
    Logger.runStdoutLoggingT $ do
      flip Accum.evalAccumT mempty $
        Foldable.forM_ tsSteps $ \step ->
          MonadIO.liftIO getTestEnv >>= runStep step

-- Prefix [Integ] to exclude this test group from unit tests
functionalTests :: FilePath -> IO TestTree
functionalTests testDir = do
  allTestFiles <- ((testDir </>) <$>) . filter isTestExtension <$> Directory.listDirectory testDir
  allTests <- traverse Yaml.decodeFileThrow allTestFiles
  pure $
    withTestEnv SingleUserMode $
      testGroupWithEnv "[Integ] Lending Functional tests" $
        waitForGlobalStateInitialization
          : fundTokenForLiquidator
          : ((`after` waitTestName) . runTest <$> filter (not . or . tsDisable) allTests)
