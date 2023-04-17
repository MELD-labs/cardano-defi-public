{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The module defining types for /account/ API endpoints.
module Lending.Api.Types.Account
  ( AccountApi
  , CreateAccountApi
  , CreateAccountRequest (..)
  , CreateAccountResponse (..)
  , UpdateAccountApi
  , UpdateAccountRequest (..)
  , UpdateAccountResponse (..)
  , CloseAccountRequest (..)
  , CloseAccountApi
  , CloseAccountResponse (..)
  , QueryAccountsApi
  , QueryAccountsByAddressApi
  , QueryAccountsResponse (..)
  , AccountInformation (..)
  , AccountState (..)
  , AssetValue (..)
  , LiquidateAccountApi
  , LiquidateAccountRequest (..)
  , LiquidateAccountResponse (..)
  , QueryExceedLtvAccountsApi
  , QueryExceedLtvAccountsResponse (..)
  , ExceedLtvAccount (..)
  , MigrateAccountApi
  , MigrateAccountInputApi
  , MigrateAccountInputResponse (..)
  , Utxos (..)
  , HistoryAccountApi
  , HistoryAccountRequest (..)
  , HistoryAccountResponse (..)
  , HistoryAccountTx (..)
  , AppliedAccount (..)
  )
where

import Cardano.Api qualified as CA
import Data.Aeson (FromJSON, Options (sumEncoding, unwrapUnaryRecords), SumEncoding (UntaggedValue), ToJSON)
import Data.Aeson.Casing qualified as Aeson
import Data.Map (Map)
import Data.OpenApi (ToSchema)
import Data.Set (Set)
import GHC.Generics (Generic)
import PlutusLedgerApi.V2 (POSIXTime)
import Servant.API (Capture, Get, JSON, Post, QueryParams, ReqBody, Summary, type (:<|>), type (:>))

import Cardano.Api.Extra.AssetId (AssetIdText)
import Cardano.Index.Data.AddressText (AddressText)
import Lending.Api.Types.Aeson (DefaultApiJson (DefaultApiJson), HasAesonOptions (aesonOptions))
import Lending.Api.Types.Orphans ()
import Lending.Api.Types.Request (ApiClearRequest, ApiRequest, HistoryAccountAction, StatusTx, TxIdAndTimeTx)
import Lending.Core.AccountValue (AccountId)
import Lending.Core.JsonViaTextEnvelope (WrapAsTx (WrapAsTx))
import Lending.Types.Account (AccountDatum)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, Fiat, LtvRatio)
import TxBuilder.Api (UtxoInput, UtxoInputWithDatum)

data HistoryAccountRequest = HistoryAccountRequest
  { harAccountId :: AccountId
  -- ^ The account id.
  , harCount :: Maybe Int
  -- ^ The number of results displayed on one page.
  , harPage :: Maybe Int
  -- ^ The page number for listing the results.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson HistoryAccountRequest

data HistoryAccountTx = HistoryAccountTx
  { hatHistoryRequestTx :: [HistoryAccountAction]
  , hatStatusTx :: StatusTx
  , hatTxInformation :: TxIdAndTimeTx
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson HistoryAccountTx

newtype HistoryAccountResponse = HistoryAccountResponse
  { harHistoryTx :: [HistoryAccountTx]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson HistoryAccountResponse

data Utxos = Utxos
  { udInputs :: [CA.TxIn]
  -- ^ Spent Utxo list.
  , udCollateralInputs :: [CA.TxIn]
  -- ^ The collateral Utxos of transaction.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson Utxos

-- | Request body for creating account API.
data CreateAccountRequest = CreateAccountRequest
  { crarChangeAddress :: AddressText (CA.AddressInEra CA.BabbageEra)
  -- ^ The change address of transaction.
  , crarUtxos :: Maybe Utxos
  -- ^ The Spent Utxos and Collateral Utxos of transaction.
  , crarNormalRequests :: Maybe [ApiRequest]
  -- ^ The request list that owner want to create.
  , crarCollateralUpdate :: Maybe (Map Asset Bool)
  -- ^ The new collateral asset list that owner want to apply.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson CreateAccountRequest

-- | Request body for closing account API.
data CloseAccountRequest = CloseAccountRequest
  { clarRef :: CA.TxIn
  -- ^ The account ref.
  , clarChangeAddress :: AddressText (CA.AddressInEra CA.BabbageEra)
  -- ^ The change address of transaction.
  , clarUtxos :: Maybe Utxos
  -- ^ The Spent Utxos and Collateral Utxos of transaction.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson CloseAccountRequest

-- | Request body for liquidating account API.
data LiquidateAccountRequest = LiquidateAccountRequest
  { larChangeAddress :: AddressText (CA.AddressInEra CA.BabbageEra)
  -- ^ The change address of transaction.
  , larUtxos :: Maybe Utxos
  -- ^ The Spent Utxos and Collateral Utxos of transaction.
  , larRef :: CA.TxIn
  -- ^ The account ref.
  , larClearRequests :: Map Asset ApiClearRequest
  , larLiquidatingDebt :: Map Asset Actual
  , larLiquidatingCollateral :: Map Asset Actual
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson LiquidateAccountRequest

-- | Response body for creating account API.
newtype CreateAccountResponse
  = -- | The balanced transaction body.
    CreateAccountResponse {crarTx :: CA.TxBody CA.BabbageEra}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via WrapAsTx

-- | Response body for closing account API.
newtype CloseAccountResponse
  = -- | The balanced transaction body.
    CloseAccountResponse {clarTx :: CA.TxBody CA.BabbageEra}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via WrapAsTx

-- | Request body for updating account API.
data UpdateAccountRequest = UpdateAccountRequest
  { uarRef :: CA.TxIn
  -- ^ The account ref.
  , uarNormalRequests :: [ApiRequest]
  -- ^ The request list that owner want to apply.
  , uarCollateralUpdate :: Maybe (Map Asset Bool)
  -- ^ The new collateral asset list that owner want to apply.
  , uarClearRequests :: Map Asset ApiClearRequest
  -- ^ The map of asset and clearRequest
  , uarChangeAddress :: AddressText (CA.AddressInEra CA.BabbageEra)
  -- ^ The change address of transaction.
  , uarUtxos :: Maybe Utxos
  -- ^ The Spent Utxos and Collateral Utxos of transaction.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson UpdateAccountRequest

-- | Response body for updating account API.
newtype UpdateAccountResponse
  = -- | The balanced transaction body.
    UpdateAccountResponse {uarTx :: CA.TxBody CA.BabbageEra}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via WrapAsTx

-- | Response body for liquidating account API.
newtype LiquidateAccountResponse
  = -- | The balanced transaction body.
    LiquidateAccountResponse {larTx :: CA.TxBody CA.BabbageEra}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via WrapAsTx

data AssetValue = AssetValue
  { avAmount :: Actual
  , avValue :: Fiat
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson AssetValue

data AccountInformation = AccountInformation
  { aiSupplies :: Map Asset AssetValue
  -- ^ The supply asset map.
  , aiBorrowings :: Map Asset AssetValue
  -- ^ The borrow asset map.
  , aiCollaterals :: Set Asset
  -- ^ The collateral asset map.
  , aiLoanToValue :: Maybe LtvRatio
  -- ^ The loan to value.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson AccountInformation

data AppliedAccount
  = AppliedAccount AccountInformation
  | AppliedAccountError String
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson AppliedAccount

instance HasAesonOptions AppliedAccount where
  aesonOptions = (Aeson.aesonPrefix Aeson.camelCase) {unwrapUnaryRecords = True, sumEncoding = UntaggedValue}

data AccountState = AccountState
  { asId :: AccountId
  -- ^ The account id
  , asRef :: CA.TxIn
  -- ^ The account ref
  , asTxId :: CA.TxId
  -- ^ The transaction id that the account was created.
  , asUserNft :: AssetIdText
  -- ^ The asset id used to identify account owner.
  , asCurrent :: AccountInformation
  -- ^ The current account (without pending requests).
  , asUpdated :: AccountInformation
  -- ^ The account with updated Pool.
  , asApplied :: AppliedAccount
  -- ^ The account after applying all pending requests.
  , asNormalRequests :: [ApiRequest]
  -- ^ The pending requests waiting to be applied.
  , asClearRequests :: Map Asset ApiClearRequest
  -- ^ The clear requests waiting to be applied.
  , asCollateralUpdate :: Maybe (Set Asset)
  -- ^ The pending request to update collateral.
  , asBeingLiquidated :: Bool
  -- ^ The flag of if the account is being liquidated or not.
  , asExtraLovelace :: Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson AccountState

-- | Response body for querying accounts' state API
data QueryAccountsResponse = QueryAccountsResponse
  { qarAccounts :: [AccountState]
  -- ^ The current accounts' state
  , qarTimestamp :: POSIXTime
  -- ^ The current time stamp
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson QueryAccountsResponse

data ExceedLtvAccount = ExceedLtvAccount
  { elaId :: AccountId
  -- ^ The account id
  , elaRef :: CA.TxIn
  -- ^ The account ref
  , elaUserNft :: AssetIdText
  -- ^ The owner nft of account
  , elaSupplies :: Map Asset Actual
  -- ^ The current supply asset map.
  , elaBorrowings :: Map Asset Actual
  -- ^ The current borrow asset map.
  , elaCollaterals :: Map Asset Bool
  -- ^ The current collateral asset map.
  , elaCurrentLoanToValue :: Maybe LtvRatio
  -- ^ The current loan to value (without pending requests).
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson ExceedLtvAccount

newtype QueryExceedLtvAccountsResponse = QueryExceedLtvAccountsResponse
  { qelarAccountList :: [ExceedLtvAccount]
  -- ^ The account list including account that can be liquidated.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson QueryExceedLtvAccountsResponse

data MigrateAccountInputResponse = MigrateAccountInputResponse
  { mairAccountInputList :: [UtxoInputWithDatum AccountDatum]
  , mairAccountScriptRefUtxo :: UtxoInput
  , mairOldAccountAuthToken :: AssetIdText
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson MigrateAccountInputResponse

-- | The API to build transaction to interact with account.
type AccountApi =
  "account"
    :> ( CreateAccountApi
          :<|> CloseAccountApi
          :<|> UpdateAccountApi
          :<|> LiquidateAccountApi
          :<|> QueryAccountsApi
          :<|> QueryAccountsByAddressApi
          :<|> QueryExceedLtvAccountsApi
          :<|> MigrateAccountApi
          :<|> HistoryAccountApi
       )

-- | The API to build transaction creating a new account.
type CreateAccountApi =
  "create"
    :> Summary "Build transaction creating a new account"
    :> ReqBody '[JSON] CreateAccountRequest
    :> Post '[JSON] CreateAccountResponse

-- | The API to build transaction closing an account.
type CloseAccountApi =
  "close"
    :> Summary "Build transaction closing an account"
    :> ReqBody '[JSON] CloseAccountRequest
    :> Post '[JSON] CloseAccountResponse

-- | The API to build transaction updating an account.
type UpdateAccountApi =
  "update"
    :> Summary "Build transaction updating an account"
    :> ReqBody '[JSON] UpdateAccountRequest
    :> Post '[JSON] UpdateAccountResponse

-- | The API to build transaction liquidating an account.
type LiquidateAccountApi =
  "liquidate"
    :> Summary "Build transaction liquidating an account"
    :> ReqBody '[JSON] LiquidateAccountRequest
    :> Post '[JSON] LiquidateAccountResponse

type QueryAccountsApi =
  Summary "Query up to date accounts' state"
    :> QueryParams "ownerNft" AssetIdText
    :> Get '[JSON] QueryAccountsResponse

type QueryAccountsByAddressApi =
  Summary "Query up to date accounts' state by address"
    :> Capture "address" (AddressText (CA.AddressInEra CA.BabbageEra))
    :> Get '[JSON] QueryAccountsResponse

type QueryExceedLtvAccountsApi =
  "exceed-ltv"
    :> Summary "Query up to date accounts that can be liquidated"
    :> Get '[JSON] QueryExceedLtvAccountsResponse

type MigrateAccountApi =
  "migrate"
    :> MigrateAccountInputApi

type MigrateAccountInputApi =
  "input"
    :> Summary "Query current accounts to spend as inputs of migration tx"
    :> Get '[JSON] MigrateAccountInputResponse

type HistoryAccountApi =
  "history"
    :> Summary "Query account's history"
    :> ReqBody '[JSON] HistoryAccountRequest
    :> Get '[JSON] HistoryAccountResponse
