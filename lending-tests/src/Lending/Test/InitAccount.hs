{-# LANGUAGE NamedFieldPuns #-}

module Lending.Test.InitAccount
  ( createAccount
  , mintTestTokens
  , generateAccount
  )
where

import Cardano.Api qualified as CA
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Data.Map qualified as Map
import Data.Set qualified as Set
import Database.Esqueleto.PostgreSQL.JSON (JSONB (unJSONB))
import Database.Persist ((==.))
import Test.Tasty.HUnit qualified as Tasty
import UnliftIO.Async qualified as Async

import Cardano.Api.Extra.Tx (signSubmitAndWaitTx)
import Cardano.Api.Extra.Wait (waitForTxIdConfirmation)
import Cardano.Index.Data.AddressText (AddressText (AddressText))
import Lending.Api.Client (createAccountClient)
import Lending.Api.Types.Account
  ( CreateAccountRequest (CreateAccountRequest)
  , CreateAccountResponse (CreateAccountResponse, crarTx)
  )
import Lending.Api.Types.Request (fromRequest)
import Lending.Core.Utils (getChangeAddress)
import Lending.Faucet.Types (MintTokenRequest (MintTokenRequest), MintTokenResponse (MintTokenResponse, mtTxId))
import Lending.Index.Account (Account (Account), EntityField (AccountRef), accountUserNft)
import Lending.Test.Common
  ( getLatestStateExtract
  , mintTokenClient
  , queryApi
  , retryOption
  )
import Lending.Test.Env
  ( IntegTest
  , IntegTestM
  , TestEnv
    ( TestEnv
    , apiClientEnv
    , dbConnection
    , faucetClientEnv
    , nodeConnection
    )
  , TestUserAccount (TestUserAccount)
  , TestUserCredential (TestUserCredential, tucAddress, tucSigningKey)
  , runOnNodeConnectionPool
  , testNetwork
  )
import Lending.Test.Functional.Types (TestUserName)
import Lending.Types.Account (Request)

createAccount :: [Request] -> TestUserCredential -> IntegTestM TestUserAccount
createAccount
  initialRequests
  testUserCredential@TestUserCredential {tucSigningKey, tucAddress}
  TestEnv {dbConnection, nodeConnection, apiClientEnv} = do
    requestApi <-
      MonadIO.liftIO $
        either
          (\_ -> Tasty.assertFailure "Unable to extract user request")
          pure
          (traverse (fromRequest testNetwork) initialRequests)
    CreateAccountResponse {crarTx} <-
      queryApi apiClientEnv $
        createAccountClient $
          CreateAccountRequest (AddressText tucAddress) Nothing (Just requestApi) Nothing
    txId <- runOnNodeConnectionPool nodeConnection $ signSubmitAndWaitTx Nothing tucSigningKey crarTx
    let accountTxIn = CA.TxIn txId (CA.TxIx 0)
    Account {accountUserNft} <-
      getLatestStateExtract dbConnection [AccountRef ==. accountTxIn] pure
    pure (TestUserAccount testUserCredential $ unJSONB accountUserNft)

mintTestTokens :: TestUserCredential -> IntegTest
mintTestTokens TestUserCredential {tucAddress} TestEnv {nodeConnection, faucetClientEnv} = do
  MintTokenResponse {mtTxId} <-
    queryApi faucetClientEnv $
      mintTokenClient $
        MintTokenRequest (AddressText tucAddress)
  Monad.void $ runOnNodeConnectionPool nodeConnection $ waitForTxIdConfirmation retryOption mtTxId 1

generateAccount :: Set.Set TestUserName -> IntegTestM (Map.Map TestUserName TestUserAccount)
generateAccount usersName testEnv = do
  credentials <- Monad.replicateM (length usersName) $ do
    signingKey <- MonadIO.liftIO $ CA.generateSigningKey CA.AsPaymentExtendedKey
    pure (TestUserCredential signingKey (getChangeAddress testNetwork signingKey))
  Map.fromList . zip (Set.toList usersName)
    <$> Async.forConcurrently
      credentials
      ( \credential ->
          mintTestTokens credential testEnv -- mint token for user
            >> createAccount [] credential testEnv
      )
