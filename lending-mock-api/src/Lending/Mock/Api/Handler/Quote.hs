{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Mock.Api.Handler.Quote (quoteApi) where

import Control.Concurrent (readMVar)
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Trans.Reader qualified as Reader
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Servant (ServerT)

import Lending.Mock.Api.AppEnv
  ( AppEnv (AppEnv, aeAssetsAndSlugs, aeAssetsPrices)
  , AppM
  , AssetConfig (adSlug)
  , AssetsPrices
  )
import Lending.Oracle.Api.Types.Quote
  ( CMCTokenInfo (CMCTokenInfo)
  , CurrencyType (CurrencyType)
  , PriceResponse (PriceResponse)
  , QuoteApi
  , USDPrice (USDPrice)
  )
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)

getPriceResponse :: Map Asset AssetConfig -> Maybe String -> AssetsPrices -> PriceResponse
getPriceResponse _ Nothing _ = PriceResponse Map.empty
getPriceResponse assetsAndSlugs (Just tokens) prices =
  let slugs = Text.splitOn "," $ Text.pack tokens
      assets = Map.filter ((`elem` slugs) . adSlug) assetsAndSlugs
      f :: Text.Text -> Price -> CMCTokenInfo
      f token price = CMCTokenInfo token (CurrencyType . USDPrice $ price)
      result = Map.intersectionWith (f . adSlug) assets prices
   in PriceResponse $ Map.mapKeysMonotonic (Text.pack . show) result

quoteApi :: ServerT QuoteApi AppM
quoteApi assets = do
  AppEnv {aeAssetsPrices, aeAssetsAndSlugs} <- Reader.ask
  prices <- MonadIO.liftIO $ readMVar aeAssetsPrices
  pure $ getPriceResponse aeAssetsAndSlugs assets prices
