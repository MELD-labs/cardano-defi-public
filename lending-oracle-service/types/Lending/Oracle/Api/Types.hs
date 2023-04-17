{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lending.Oracle.Api.Types (LendingOracleApi, lendingOracleApi) where

import Data.Proxy (Proxy (Proxy))

import Lending.Oracle.Api.Types.FeedPrice (FeedPriceApi)
import Lending.Oracle.Api.Types.Quote (QuoteApi)

import Servant.API ((:<|>), (:>))

type LendingOracleApi = "cryptocurrency" :> LendingOracleApiInner

type LendingOracleApiInner = QuoteApi :<|> FeedPriceApi

lendingOracleApi :: Proxy LendingOracleApi
lendingOracleApi = Proxy
