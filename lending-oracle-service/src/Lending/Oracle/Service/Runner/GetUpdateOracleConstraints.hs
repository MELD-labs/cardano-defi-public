{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Oracle.Service.Runner.GetUpdateOracleConstraints (runGetUpdateOracleTxBody) where

import Cardano.Api.Shelley qualified as CA
import Control.Monad.Catch qualified as Catch
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Data.Map qualified as Map
import Data.Tagged (Tagged (Tagged, unTagged))
import Data.Text (Text, pack)
import Data.Tuple.Extra ((&&&))

import Lending.Oracle.Api.Exceptions (OracleServiceException (UnableToDecodeAssetOracleService))
import Lending.Oracle.Api.Types.Quote
  ( CMCTokenInfo (quote, slug)
  , CurrencyType (currencyType)
  , PriceResponse (priceResponse)
  , USDPrice (price)
  )
import Lending.Oracle.Service.AppEnv
  ( AppEnv
      ( AppEnv
      , aeAPIClientEnv
      , aeCMCConfig
      )
  , AppM
  )
import Lending.Oracle.Service.Client (clientQuote, queryApi)
import Lending.Oracle.Service.Config
  ( AssetConfig (adDecimal, adSlug)
  , CoinMarketCapConfig (cmAssetsAndSlugs, cmTokensString)
  )
import Lending.Oracle.Service.Runner.Utils (buildUpdateOracleTxBody)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)
import Plutarch.Extra.FixedDecimal (FixedDecimal, ediv, emul)

defaultUnit :: FixedDecimal 0
defaultUnit = 1_000_000

mulPrice :: Integer -> Price -> Price
mulPrice dec pri = Tagged $ (unTagged pri `emul` defaultUnit) `ediv` (10 ^ dec)

getAssetPrices :: Map.Map Text CurrencyType -> Map.Map Asset AssetConfig -> AppM (Map.Map Asset Price)
getAssetPrices cmcPrices =
  Map.traverseWithKey $ \asset assetConfig ->
    maybe
      (Catch.throwM (UnableToDecodeAssetOracleService asset))
      (pure . mulPrice (adDecimal assetConfig) . price . currencyType)
      (Map.lookup (adSlug assetConfig) cmcPrices)

runGetUpdateOracleTxBody :: AppM (CA.TxBody CA.BabbageEra)
runGetUpdateOracleTxBody = do
  AppEnv
    { aeCMCConfig
    , aeAPIClientEnv
    } <-
    Reader.ask
  Logger.logInfoN "Get price asset from CMC"
  prices <- queryApi aeAPIClientEnv (clientQuote . Just $ cmTokensString aeCMCConfig)
  Logger.logInfoN $ "Build update oracle transaction with price: " <> pack (show prices)
  let slugsAndPrices = Map.fromList $ (slug &&& quote) <$> Map.elems (priceResponse prices)
  getAssetPrices slugsAndPrices (cmAssetsAndSlugs aeCMCConfig) >>= buildUpdateOracleTxBody
