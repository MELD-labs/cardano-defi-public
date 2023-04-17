{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Test.Services.Oracle
  ( feedPriceToMockApi
  , testInitOracleSpec
  , testUpdateMeldPriceSpec
  )
where

import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Data.Map qualified as Map
import Data.Tagged (Tagged (Tagged))
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB))
import Database.Persist ((==.))
import Test.Tasty.HUnit qualified as Tasty

import Lending.Index.Oracle qualified as IO
import Lending.Oracle.Api.Types.Quote
  ( CMCTokenInfo (CMCTokenInfo)
  , CurrencyType (CurrencyType)
  , PriceResponse (priceResponse)
  , USDPrice (USDPrice)
  )
import Lending.Test.Common (clientFeedPrice, clientQuote, getLatestStateExtract, queryApi)
import Lending.Test.Env (IntegTest, TestEnv (TestEnv, dbConnection, mockApiClientEnv), adaAsset, meldAsset)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)
import Lending.Types.Oracle (OracleDatum (OracleDatum))
import Lending.Types.Percent (percent)

feedPriceToMockApi :: IntegTest
feedPriceToMockApi TestEnv {mockApiClientEnv} = do
  Monad.void $ queryApi mockApiClientEnv (clientFeedPrice $ Map.fromList [(adaAsset, 0.3), (meldAsset, 0.002)])
  result <- queryApi mockApiClientEnv (clientQuote $ Just "cardano,meld")
  MonadIO.liftIO $
    Tasty.assertBool
      ("Prices should be updated" <> show result)
      (all (`elem` Map.elems (priceResponse result)) expected)
  where
    expected :: [CMCTokenInfo]
    expected =
      [ CMCTokenInfo "cardano" (CurrencyType . USDPrice $ 0.3)
      , CMCTokenInfo "meld" (CurrencyType . USDPrice $ 0.002)
      ]

-- Oracle datum will update prices with decimal of tokens from config
testOracleService :: Map.Map Asset Price -> IntegTest
testOracleService prices TestEnv {dbConnection, mockApiClientEnv} = do
  Monad.void $ queryApi mockApiClientEnv (clientFeedPrice prices)
  let oracleDatum = OracleDatum prices
  Monad.void $ getLatestStateExtract @IO.Oracle dbConnection [IO.OracleDatum ==. JSONB oracleDatum] pure

testInitOracleSpec :: IntegTest
testInitOracleSpec =
  testOracleService
    ( Map.fromList
        [ (adaAsset, 1)
        , (meldAsset, 1)
        , (2, 13)
        , (3, 300)
        , (4, 17000)
        , (5, 1200)
        , (6, 0.01)
        , (7, 0.015)
        , (8, 1)
        , (9, 1)
        ]
    )

testUpdateMeldPriceSpec :: IntegTest
testUpdateMeldPriceSpec =
  testOracleService
    ( Map.fromList
        [ (adaAsset, 1)
        , (meldAsset, Tagged $ percent 175)
        , (2, 12)
        , (3, 400)
        , (4, 18000)
        , (5, 1100)
        , (6, 0.005)
        , (7, 0.01)
        , (8, 1)
        , (9, 1)
        ]
    )
