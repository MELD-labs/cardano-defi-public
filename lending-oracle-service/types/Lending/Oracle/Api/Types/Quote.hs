{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lending.Oracle.Api.Types.Quote
  ( QuoteApi
  , CurrencyType (..)
  , PriceResponse (..)
  , CMCTokenInfo (..)
  , USDPrice (..)
  )
where

import Data.Aeson (FromJSON, object, withObject, (.:), (.=))
import Data.Aeson.Types (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (Get, JSON, QueryParam, type (:>))

import Lending.Types.Exchange (Price)
import Lending.Types.Orphans ()

type QuoteApi =
  "quotes"
    :> "latest"
    :> QueryParam "slug" String
    :> Get '[JSON] PriceResponse

newtype PriceResponse = PriceResponse {priceResponse :: Map Text CMCTokenInfo}
  deriving stock (Generic, Show)

instance FromJSON PriceResponse where
  parseJSON = withObject "PriceResponse" $ \v -> PriceResponse <$> v .: "data"

instance ToJSON PriceResponse where
  toJSON (PriceResponse pr) = object ["data" .= pr]

data CMCTokenInfo = CMCTokenInfo
  { slug :: Text
  , quote :: CurrencyType
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Price in usd
newtype CurrencyType = CurrencyType {currencyType :: USDPrice}
  deriving stock (Generic, Show, Eq)

instance FromJSON CurrencyType where
  parseJSON = withObject "CurrencyType" $ \v -> CurrencyType <$> v .: "USD"

instance ToJSON CurrencyType where
  toJSON (CurrencyType c) = object ["USD" .= c]

newtype USDPrice = USDPrice {price :: Price}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
