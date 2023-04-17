module Lending.Oracle.Service.Client
  ( clientQuote
  , clientFeedPrice
  , queryApi
  )
where

import Servant.API qualified as Servant
import Servant.Client (ClientEnv, ClientM)
import Servant.Client qualified as Client

import Lending.Core.Errors (throwE)
import Lending.Core.Utils qualified as Utils
import Lending.Oracle.Api.Exceptions (OracleServiceException (ServiceOracleCmcApiError))
import Lending.Oracle.Api.Types (lendingOracleApi)
import Lending.Oracle.Api.Types.FeedPrice (FeedPriceRequest)
import Lending.Oracle.Api.Types.Quote (PriceResponse)
import Lending.Oracle.Service.AppEnv (AppM)

clientQuote :: Maybe String -> ClientM PriceResponse
clientFeedPrice :: FeedPriceRequest -> ClientM FeedPriceRequest
clientQuote Servant.:<|> clientFeedPrice = Client.client lendingOracleApi

queryApi :: ClientEnv -> ClientM response -> AppM response
queryApi = Utils.queryApi (throwE id . ServiceOracleCmcApiError)
