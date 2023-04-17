{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Lending.Mock.Api.Handler.FeedPrice (feedPriceApi) where

import Control.Concurrent (swapMVar)
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Trans.Reader qualified as Reader
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tagged (Tagged (Tagged, unTagged))
import Servant (ServerT)

import Lending.Mock.Api.AppEnv (AppEnv (AppEnv, aeAssetsAndSlugs, aeAssetsPrices), AppM, AssetConfig (adDecimal))
import Lending.Oracle.Api.Types.FeedPrice (FeedPriceApi)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)
import Plutarch.Extra.FixedDecimal (FixedDecimal, ediv, emul)

transformDecimals :: Map Asset AssetConfig -> Asset -> Price -> Price
transformDecimals slugMap asset price =
  Tagged (unTagged price `emul` expectedBatchSize `ediv` defaultUnit)
  where
    expectedBatchSize :: FixedDecimal 0
    expectedBatchSize = 10 ^ maybe 0 adDecimal (Map.lookup asset slugMap)

    defaultUnit :: FixedDecimal 0
    defaultUnit = 1_000_000

feedPriceApi :: ServerT FeedPriceApi AppM
feedPriceApi prices = do
  AppEnv {aeAssetsPrices, aeAssetsAndSlugs} <- Reader.ask
  MonadIO.liftIO $ swapMVar aeAssetsPrices (Map.mapWithKey (transformDecimals aeAssetsAndSlugs) prices)
