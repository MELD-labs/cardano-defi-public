{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lending.Api.Docs (apiServer) where

import Control.Lens ((&), (.~), (?~))
import Data.OpenApi
  ( HasServers (servers)
  , OpenApi
  , description
  , info
  , title
  )
import Data.String qualified as String
import Data.Text (Text)
import Servant
  ( Proxy (Proxy)
  , (:<|>)
  )
import Servant qualified as S
import Servant.OpenApi (toOpenApi)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import System.Environment qualified as Environment

import Lending.Api.Types (LendingApiInner)
import Lending.Api.Types.Account (AccountApi)
import Lending.Api.Types.Liquidation (LiquidationApi)
import Lending.Api.Types.Manager (ManagerApi)
import Lending.Api.Types.Oracle (OracleApi)
import Lending.Api.Types.Pool (PoolApi)
import Lending.Faucet.Types (FaucetApiInner)
import Servant.Api.Docs (PostRedirect, getServerInfo, redirectsToOpenApi, tagSubApi)
import Servant.Api.Handler (getTlsSettings, runAppServer)

-- | Synonym Lending API documentation
type LendingApiDocs =
  PostRedirect
    :<|> SwaggerSchemaUI "api" "api.openapi.json"
    :<|> SwaggerSchemaUI "faucet" "faucet.openapi.json"

-- | OpenApi details
apiOpenApi :: Text -> OpenApi
apiOpenApi host =
  toOpenApi (Proxy :: Proxy LendingApiInner)
    & info . title .~ "Lending API"
    & info . description ?~ "Lending API by MELD"
    & servers .~ [getServerInfo "API_HOST" "lending/v1" host]
    & tagSubApi (Proxy @AccountApi) "Account" "Account interactions: create, update, close, query account state"
    & tagSubApi (Proxy @ManagerApi) "Manager" "Manager interactions: update, query manager state"
    & tagSubApi (Proxy @OracleApi) "Oracle" "Query or update manager"
    & tagSubApi (Proxy @PoolApi) "Pool" "Query latest status of the pool"
    & tagSubApi (Proxy @LiquidationApi) "Liquidation" "Query liquidations of protocol"

faucetOpenApi :: Text -> OpenApi
faucetOpenApi host =
  toOpenApi (Proxy :: Proxy FaucetApiInner)
    & info . title .~ "Faucet API"
    & info . description ?~ "Faucet API by MELD"
    & servers .~ [getServerInfo "FAUCET_API_HOST" "faucet/v1" host]

-- | OpenApi Api documentation
lendingApiDocsServer :: Text -> Text -> S.Server LendingApiDocs
lendingApiDocsServer apiHost faucetApiHost =
  redirectsToOpenApi
    S.:<|> swaggerSchemaUIServer (apiOpenApi apiHost)
    S.:<|> swaggerSchemaUIServer (faucetOpenApi faucetApiHost)

-- | Read all env vars and run the 'AppM' action using constructed environment
apiServer :: IO ()
apiServer = do
  tls <- getTlsSettings
  apiHost <- String.fromString <$> Environment.getEnv "API_HOST"
  faucetApiHost <- String.fromString <$> Environment.getEnv "FAUCET_API_HOST"
  runAppServer @LendingApiDocs 3002 tls (lendingApiDocsServer apiHost faucetApiHost) id
