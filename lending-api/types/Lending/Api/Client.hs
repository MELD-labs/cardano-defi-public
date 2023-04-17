module Lending.Api.Client
  ( queryManagerClient
  , updateAccountClient
  , createAccountClient
  , updateManagerClient
  , closeAccountClient
  , queryPoolClient
  , updateTreasuryPoolClient
  , queryAccountsClient
  , queryAccountsByAddressClient
  , queryExceedLtvAccountsClient
  , liquidateAccountClient
  , clientUpdateOracle
  , clientQueryGlobalState
  , migratePoolInputClient
  , clientQueryRiskDao
  , migrateAccountInputClient
  , migrateManagerClient
  , clientSyncStatus
  , historyAccountClient
  , clientQueryLiquidation
  )
where

import Cardano.Api qualified as CA
import Data.Proxy qualified as Proxy
import Servant.API qualified as Servant
import Servant.Client qualified as Client

import Cardano.Api.Extra.AssetId (AssetIdText)
import Cardano.Index.Data.AddressText (AddressText)
import Lending.Api.Types (LendingApi)
import Lending.Api.Types.Account
  ( CloseAccountRequest
  , CloseAccountResponse
  , CreateAccountRequest
  , CreateAccountResponse
  , HistoryAccountRequest
  , HistoryAccountResponse
  , LiquidateAccountRequest
  , LiquidateAccountResponse
  , MigrateAccountInputResponse
  , QueryAccountsResponse
  , QueryExceedLtvAccountsResponse
  , UpdateAccountRequest
  , UpdateAccountResponse
  )
import Lending.Api.Types.GlobalState (QueryGlobalStateResponse)
import Lending.Api.Types.Liquidation (QueryLiquidationResponse)
import Lending.Api.Types.Manager
  ( QueryManagerResponse
  , UpdateManagerResponse
  )
import Lending.Api.Types.Oracle (UpdateOracleRequest, UpdateOracleResponse)
import Lending.Api.Types.Pool
  ( MigratePoolInputResponse
  , QueryPoolResponse
  , UpdateTreasuryPoolRequest
  , UpdateTreasuryPoolResponse
  )
import Lending.Api.Types.RiskDao (QueryRiskDaoResponse)
import Lending.Api.Types.SyncStatus (SyncStatusResponse)
import Lending.Types (ManagerDatum)

lendingApi :: Proxy.Proxy LendingApi
lendingApi = Proxy.Proxy

queryManagerClient :: Client.ClientM QueryManagerResponse
queryPoolClient :: Client.ClientM QueryPoolResponse
updateTreasuryPoolClient :: UpdateTreasuryPoolRequest -> Client.ClientM UpdateTreasuryPoolResponse
migratePoolInputClient :: Client.ClientM MigratePoolInputResponse
queryAccountsClient :: [AssetIdText] -> Client.ClientM QueryAccountsResponse
queryAccountsByAddressClient :: AddressText (CA.AddressInEra CA.BabbageEra) -> Client.ClientM QueryAccountsResponse
queryExceedLtvAccountsClient :: Client.ClientM QueryExceedLtvAccountsResponse
createAccountClient :: CreateAccountRequest -> Client.ClientM CreateAccountResponse
updateAccountClient :: UpdateAccountRequest -> Client.ClientM UpdateAccountResponse
migrateAccountInputClient :: Client.ClientM MigrateAccountInputResponse
updateManagerClient :: ManagerDatum -> Client.ClientM UpdateManagerResponse
clientUpdateOracle :: UpdateOracleRequest -> Client.ClientM UpdateOracleResponse
closeAccountClient :: CloseAccountRequest -> Client.ClientM CloseAccountResponse
liquidateAccountClient :: LiquidateAccountRequest -> Client.ClientM LiquidateAccountResponse
clientQueryRiskDao :: Client.ClientM QueryRiskDaoResponse
clientQueryGlobalState :: Client.ClientM QueryGlobalStateResponse
migrateManagerClient :: ManagerDatum -> Client.ClientM UpdateManagerResponse
historyAccountClient :: HistoryAccountRequest -> Client.ClientM HistoryAccountResponse
clientSyncStatus :: Client.ClientM SyncStatusResponse
clientQueryLiquidation :: [CA.TxIn] -> Client.ClientM QueryLiquidationResponse
( updateManagerClient
    Servant.:<|> queryManagerClient
    Servant.:<|> migrateManagerClient
  )
  Servant.:<|> ( createAccountClient
                  Servant.:<|> closeAccountClient
                  Servant.:<|> updateAccountClient
                  Servant.:<|> liquidateAccountClient
                  Servant.:<|> queryAccountsClient
                  Servant.:<|> queryAccountsByAddressClient
                  Servant.:<|> queryExceedLtvAccountsClient
                  Servant.:<|> migrateAccountInputClient
                  Servant.:<|> historyAccountClient
                )
  Servant.:<|> clientUpdateOracle
  Servant.:<|> ( queryPoolClient
                  Servant.:<|> updateTreasuryPoolClient
                  Servant.:<|> migratePoolInputClient
                )
  Servant.:<|> clientQueryRiskDao
  Servant.:<|> clientQueryGlobalState
  Servant.:<|> clientSyncStatus
  Servant.:<|> clientQueryLiquidation = Client.client lendingApi
