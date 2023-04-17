{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Test.Env
  ( IntegTest
  , IntegTestM
  , SingleUserIntegTest
  , TestEnv (..)
  , TestError (..)
  , TestUserAccount (..)
  , TestUserCredential (..)
  , TestUserMode (..)
  , TestStakingCredential (..)
  , adaAsset
  , meldAsset
  , after
  , testCaseWithEnv
  , testCaseWithEnvSingleUser
  , testGroupWithEnv
  , withTestEnv
  , runOnNodeConnectionPool
  , runSqlM
  , testNetwork
  )
where

import Cardano.Api qualified as CA
import Control.Exception (Exception)
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.IO.Unlift qualified as Unlift
import Control.Monad.Logger (LoggingT)
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Resource qualified as Resource
import Data.ByteString.Char8 qualified as C8
import Data.Pool (Pool, PoolConfig (PoolConfig, createResource, freeResource, poolCacheTTL, poolMaxResources))
import Data.Pool qualified as Pool
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.Persist.Postgresql (ConnectionPool)
import Database.Persist.Postgresql qualified as Persist
import Database.Persist.Sql (SqlBackend)
import Database.PostgreSQL.Simple qualified as PostgreSQL
import Network.HTTP.Client qualified as HttpClient
import Servant.Client (ClientEnv)
import Servant.Client qualified as Servant
import System.Environment qualified as Environment
import Test.Tasty (DependencyType (AllSucceed), TestName, TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (Assertion)
import Test.Tasty.HUnit qualified as Tasty

import Cardano.Api.Extra.Key (readPaymentSigningKey, readStakeSigningKey)
import Cardano.Api.Extra.Node qualified as CAE
import Cardano.Index.Context (runVoidLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Lending.Core.Utils (getChangeAddress, getStakingAddress)
import Lending.Types.Asset (Asset)
import Plutarch.Extra.AssetClass (AssetClass)

data TestUserCredential = TestUserCredential
  { tucSigningKey :: CA.SigningKey CA.PaymentExtendedKey
  , tucAddress :: CA.AddressInEra CA.BabbageEra
  }

data TestStakingCredential = TestStakingCredential
  { tscStakeSigningKey :: CA.SigningKey CA.StakeKey
  , tscStakeKeyHash :: CA.StakeAddress
  }

data TestUserAccount = TestUserAccount
  { tuaCredential :: TestUserCredential
  , tuaUserNft :: AssetClass
  }

data TestUserMode
  = -- | Test mode for simple test cases that do not require or require interacting 1 Account.
    SingleUserMode
  | -- | Test mode for complex test cases that require interacting with multiple Accounts.
    MultipleUserMode Int

data TestEnv = TestEnv
  { testUsers :: [TestUserCredential]
  , managerOperator :: TestUserCredential
  , poolOperator :: TestUserCredential
  , migrationOperator :: TestUserCredential
  , stakeKey :: TestStakingCredential
  , liquidatorOperator :: TestUserCredential
  , dbConnection :: ConnectionPool
  , nodeConnection :: Pool CAE.NodeConnectInfo
  , apiClientEnv :: ClientEnv
  , faucetClientEnv :: ClientEnv
  , mockApiClientEnv :: ClientEnv
  }

type IntegTestM a = TestEnv -> LoggingT IO a
type IntegTest = IntegTestM ()
type SingleUserIntegTest = TestUserCredential -> IntegTest

adaAsset :: Asset
adaAsset = 0

meldAsset :: Asset
meldAsset = 1

runSqlM :: MonadUnliftIO m => ReaderT SqlBackend (ResourceT m) a -> ConnectionPool -> m a
runSqlM sql = Resource.runResourceT . Persist.runSqlPool sql

runTestLoggingT :: (String -> IO ()) -> LoggingT m a -> m a
runTestLoggingT step task = Logger.runLoggingT task $ \_ _ _ logStr ->
  step (C8.unpack (Logger.fromLogStr logStr))

runOnNodeConnectionPool :: MonadUnliftIO m => Pool CAE.NodeConnectInfo -> ReaderT CAE.NodeConnectInfo m a -> m a
runOnNodeConnectionPool nodeConnectionPool task =
  Unlift.withRunInIO $ \runInIO ->
    Pool.withResource nodeConnectionPool (runInIO . Reader.runReaderT task)

data TestError = AccountUtxoNotFound | InvalidUserNft
  deriving stock (Show)
  deriving anyclass (Exception)

testNetwork :: CA.NetworkId
testNetwork = CA.Testnet (CA.NetworkMagic 42)

withCredential :: String -> (IO TestUserCredential -> TestTree) -> TestTree
withCredential env = Tasty.withResource (getUserCredential <$> readPaymentSigningKey env) (const (pure ()))

withStakingCredential :: String -> (IO TestStakingCredential -> TestTree) -> TestTree
withStakingCredential env = Tasty.withResource (getTestStakingCredential <$> readStakeSigningKey env) (const (pure ()))

withGeneratedCredentials :: TestUserMode -> (IO [TestUserCredential] -> TestTree) -> TestTree
withGeneratedCredentials mode =
  let userAmount = case mode of
        SingleUserMode -> 1
        MultipleUserMode n -> n
   in Tasty.withResource
        ((getUserCredential <$>) <$> Monad.replicateM userAmount (CA.generateSigningKey CA.AsPaymentExtendedKey))
        (const (pure ()))

getUserCredential :: CA.SigningKey CA.PaymentExtendedKey -> TestUserCredential
getUserCredential signingKey = TestUserCredential signingKey (getChangeAddress testNetwork signingKey)

getTestStakingCredential :: CA.SigningKey CA.StakeKey -> TestStakingCredential
getTestStakingCredential signingKey = TestStakingCredential signingKey (getStakingAddress testNetwork signingKey)

withDbConnection :: (IO ConnectionPool -> TestTree) -> TestTree
withDbConnection = Tasty.withResource createPool Pool.destroyAllResources
  where
    poolSize = 5
    setSchema conn = Monad.void (PostgreSQL.execute_ conn "SET search_path TO public, cardano")
    createPool = do
      connectionString <- Text.encodeUtf8 . Text.pack <$> Environment.getEnv "DB_CONNECTION_STRING"
      runVoidLoggingT (Persist.createPostgresqlPoolModified setSchema connectionString poolSize)

getHttpClientEnv :: String -> IO ClientEnv
getHttpClientEnv env =
  Servant.mkClientEnv
    <$> HttpClient.newManager HttpClient.defaultManagerSettings
    <*> (Environment.getEnv env >>= Servant.parseBaseUrl)

getNodeConnection :: IO (Pool CAE.NodeConnectInfo)
getNodeConnection = do
  nodeConnection <- CAE.mkNodeConnectInfo testNetwork <$> Environment.getEnv "CARDANO_NODE_SOCKET_PATH"
  Pool.newPool
    PoolConfig
      { createResource = pure nodeConnection
      , freeResource = const (pure ())
      , poolCacheTTL = 1
      , poolMaxResources = 8
      }

withTestEnv :: TestUserMode -> (IO TestEnv -> TestTree) -> TestTree
withTestEnv mode tests =
  withCredential "MANAGER_OPERATOR_SIGNING_KEY" $ \getManagerOperatorCredential ->
    withCredential "POOL_OPERATOR_SIGNING_KEY" $ \getPoolOperatorCredential ->
      withCredential "MIGRATION_OPERATOR_SIGNING_KEY" $ \getMigrationCredential ->
        withCredential "LIQUIDATOR_SIGNING_KEY" $ \getLiquidatorCredential ->
          withGeneratedCredentials mode $ \getTestUserCredentials ->
            withStakingCredential "STAKE_SIGNING_KEY" $ \getStakeCredential ->
              withDbConnection $ \getDbConnection ->
                tests $
                  TestEnv
                    <$> getTestUserCredentials
                    <*> getManagerOperatorCredential
                    <*> getPoolOperatorCredential
                    <*> getMigrationCredential
                    <*> getStakeCredential
                    <*> getLiquidatorCredential
                    <*> getDbConnection
                    <*> getNodeConnection
                    <*> getHttpClientEnv "API_URL"
                    <*> getHttpClientEnv "FAUCET_URL"
                    <*> getHttpClientEnv "MOCK_API_URL"

testGroupWithEnv :: TestName -> [IO TestEnv -> TestTree] -> IO TestEnv -> TestTree
testGroupWithEnv testName tests env = Tasty.testGroup testName (($ env) <$> tests)

testCaseWithEnv :: TestName -> IntegTest -> IO TestEnv -> TestTree
testCaseWithEnv testName test getTestEnv = Tasty.testCaseSteps testName $ \step ->
  runTestLoggingT step (MonadIO.liftIO getTestEnv >>= test)

testCaseWithEnvSingleUser :: TestName -> SingleUserIntegTest -> IO TestEnv -> TestTree
testCaseWithEnvSingleUser testName test getTestEnv =
  Tasty.testCaseSteps
    testName
    handle
  where
    handle :: (String -> IO ()) -> Assertion
    handle logger = do
      testEnv@TestEnv {testUsers} <- getTestEnv
      case testUsers of
        [] -> Tasty.assertFailure "No test user"
        (u : _) -> runTestLoggingT logger (test u testEnv)

after :: (IO TestEnv -> TestTree) -> String -> (IO TestEnv -> TestTree)
after test deps = Tasty.after AllSucceed deps . test
