{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Test.Functional.Oracle (updateOracle) where

import Control.Monad qualified as Monad
import Control.Monad.Trans.Accum qualified as Accum
import Data.Foldable qualified as Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB))
import Database.Persist ((==.))

import Lending.Index.Oracle qualified as IO
import Lending.Test.Common (clientFeedPrice, getLatestStateExtract, queryApi)
import Lending.Test.Env
  ( TestEnv (TestEnv, dbConnection, mockApiClientEnv)
  )
import Lending.Test.Functional.Asset (TestAsset, getAssetMap)
import Lending.Test.Functional.Types (FunctionalTestM)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)
import Lending.Types.Oracle (OracleDatum (OracleDatum))

toPriceMap :: Map TestAsset Price -> Map TestAsset Asset -> Map Asset Price
toPriceMap testPriceMap =
  Foldable.fold . Map.mapMaybeWithKey (\testAsset asset -> Map.singleton asset <$> Map.lookup testAsset testPriceMap)

updateOracle :: Map TestAsset Price -> TestEnv -> FunctionalTestM ()
updateOracle
  testPriceMap
  TestEnv
    { mockApiClientEnv
    , dbConnection
    } = do
    Accum.readerToAccumT $ do
      priceMap <- toPriceMap testPriceMap <$> getAssetMap dbConnection
      Monad.void $ queryApi mockApiClientEnv (clientFeedPrice priceMap)
      let oracleDatum = OracleDatum priceMap
      Monad.void $ getLatestStateExtract @IO.Oracle dbConnection [IO.OracleDatum ==. JSONB oracleDatum] pure
