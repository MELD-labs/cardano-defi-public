{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Lending.Oracle.Api.Types.FeedPrice (FeedPriceApi, FeedPriceRequest) where

import Data.Map (Map)
import Servant.API (JSON, Post, ReqBody, type (:>))

import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)

type FeedPriceApi =
  "feed-price"
    :> ReqBody '[JSON] FeedPriceRequest
    :> Post '[JSON] FeedPriceRequest

type FeedPriceRequest = Map Asset Price
