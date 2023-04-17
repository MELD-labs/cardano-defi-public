{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lending.Api.Types.Request
  ( ApiClearRequest (..)
  , ApiRequest (..)
  , toClearRequest
  , fromClearRequest
  , toRequest
  , fromRequest
  , HistoryAccountAction (..)
  , StatusTx (..)
  , TxIdAndTimeTx (..)
  , fromAccountLiquidateRedeemerData
  )
where

import Cardano.Api qualified as CA
import Data.Aeson
  ( FromJSON
  , Options (constructorTagModifier, sumEncoding)
  , SumEncoding (TaggedObject, contentsFieldName, tagFieldName)
  , ToJSON
  )
import Data.Aeson.Casing qualified as Aeson
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Data.OpenApi (ToSchema)
import Data.Time qualified as Time
import GHC.Generics (Generic)

import Cardano.Api.Extra.Adapters
  ( ToCardanoError (DeserialisationError)
  , fromCardanoAddressInEra
  , toCardanoAddressInEra
  )
import Cardano.Api.Extra.AssetId (AssetIdText (AssetIdText))
import Cardano.Index.Data.AddressText (AddressText (AddressText, unAddressText))
import Lending.Api.Types.Aeson (DefaultApiJson (DefaultApiJson), HasAesonOptions (aesonOptions))
import Lending.Api.Types.Orphans ()
import Lending.Types.Account
  ( AccountLiquidateRedeemerData
      ( AccountLiquidateRedeemerData
      , alrBorrowings
      , alrClearRequests
      , alrCollaterals
      , alrExtraLovelace
      , alrUserNft
      )
  , ClearRequest
    ( ClearBorrowing
    , ClearSupplying
    , cbLimited
    , cbReceiver
    , cdReceiver
    )
  , Request
    ( BorrowRequest
    , RepayRequest
    , SupplyRequest
    , WithdrawRequest
    , brAmount
    , brAsset
    , brReceiver
    , rrAmount
    , rrAsset
    , srAmount
    , srAsset
    , wrAmount
    , wrAsset
    , wrReceiver
    )
  )
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual)
import Plutarch.Extra.AssetClass (fromPlutusAssetClass)

data ApiAccountLiquidateRedeemerData = ApiAccountLiquidateRedeemerData
  { aalrBorrowings :: Map.Map Asset Actual
  , aalrCollaterals :: Map.Map Asset Actual
  , aalrClearRequests :: Map.Map Asset ApiClearRequest
  , aalrExtraLovelace :: Integer
  , aalrUserNft :: AssetIdText
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson ApiAccountLiquidateRedeemerData

data ApiClearRequest
  = ApiClearSupplying
      { acdReceiver :: AddressText (CA.AddressInEra CA.BabbageEra)
      }
  | ApiClearBorrowing
      { acbLimited :: Actual
      , acbReceiver :: AddressText (CA.AddressInEra CA.BabbageEra)
      }
  deriving stock (Generic, Eq, Show)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson ApiClearRequest

instance HasAesonOptions ApiClearRequest where
  aesonOptions = apiRequestAndClearRequestOptions

data ApiRequest
  = ApiSupplyRequest
      { asrAsset :: Asset
      , asrAmount :: Actual
      }
  | ApiWithdrawRequest
      { awrAsset :: Asset
      , awrAmount :: Actual
      , awrReceiver :: AddressText (CA.AddressInEra CA.BabbageEra)
      }
  | ApiBorrowRequest
      { abrAsset :: Asset
      , abrAmount :: Actual
      , abrReceiver :: AddressText (CA.AddressInEra CA.BabbageEra)
      }
  | ApiRepayRequest
      { arrAsset :: Asset
      , arrAmount :: Actual
      }
  deriving stock (Generic, Eq, Show)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson ApiRequest

instance HasAesonOptions ApiRequest where
  aesonOptions = apiRequestAndClearRequestOptions

data HistoryAccountAction
  = CreateAccountAction
  | NormalRequestAction [ApiRequest]
  | ClearRequestAction [(Asset, ApiClearRequest)]
  | SetCollateralAction [(Asset, Bool)]
  | LiquidateAccountAction ApiAccountLiquidateRedeemerData
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson HistoryAccountAction

instance HasAesonOptions HistoryAccountAction where
  aesonOptions = historyAccountActionptions

data TxIdAndTimeTx = TxIdAndTimeTx
  { tatTxId :: CA.TxId
  , tatTime :: Time.UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON, ToJSON, ToSchema) via DefaultApiJson TxIdAndTimeTx

data StatusTx = Processing | Cancel TxIdAndTimeTx | Done TxIdAndTimeTx
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

apiRequestAndClearRequestOptions :: Options
apiRequestAndClearRequestOptions =
  (Aeson.aesonPrefix Aeson.camelCase)
    { sumEncoding =
        TaggedObject
          { tagFieldName = "type"
          , contentsFieldName = ""
          }
    , constructorTagModifier = List.dropPrefix "Api" . List.dropSuffix "Request"
    }

historyAccountActionptions :: Options
historyAccountActionptions =
  (Aeson.aesonPrefix Aeson.camelCase)
    { sumEncoding =
        TaggedObject
          { tagFieldName = "type"
          , contentsFieldName = ""
          }
    , constructorTagModifier = List.dropSuffix "Action"
    }

toClearRequest :: ApiClearRequest -> ClearRequest
toClearRequest ApiClearSupplying {acdReceiver} = ClearSupplying (fromCardanoAddressInEra $ unAddressText acdReceiver)
toClearRequest ApiClearBorrowing {acbLimited, acbReceiver} =
  ClearBorrowing acbLimited (fromCardanoAddressInEra $ unAddressText acbReceiver)

fromClearRequest :: CA.NetworkId -> ClearRequest -> Either ToCardanoError ApiClearRequest
fromClearRequest networkId ClearSupplying {cdReceiver} =
  ApiClearSupplying . AddressText <$> toCardanoAddressInEra networkId cdReceiver
fromClearRequest networkId ClearBorrowing {cbLimited, cbReceiver} =
  ApiClearBorrowing cbLimited . AddressText <$> toCardanoAddressInEra networkId cbReceiver

toRequest :: ApiRequest -> Request
toRequest ApiSupplyRequest {asrAsset, asrAmount} = SupplyRequest {srAsset = asrAsset, srAmount = asrAmount}
toRequest ApiWithdrawRequest {awrAsset, awrAmount, awrReceiver} =
  WithdrawRequest
    { wrAsset = awrAsset
    , wrAmount = awrAmount
    , wrReceiver = fromCardanoAddressInEra $ unAddressText awrReceiver
    }
toRequest ApiBorrowRequest {abrAsset, abrAmount, abrReceiver} =
  BorrowRequest
    { brAsset = abrAsset
    , brAmount = abrAmount
    , brReceiver = fromCardanoAddressInEra $ unAddressText abrReceiver
    }
toRequest ApiRepayRequest {arrAsset, arrAmount} = RepayRequest {rrAsset = arrAsset, rrAmount = arrAmount}

fromRequest :: CA.NetworkId -> Request -> Either ToCardanoError ApiRequest
fromRequest _ SupplyRequest {srAsset, srAmount} = pure $ ApiSupplyRequest srAsset srAmount
fromRequest networkId WithdrawRequest {wrAsset, wrAmount, wrReceiver} =
  ApiWithdrawRequest wrAsset wrAmount . AddressText <$> toCardanoAddressInEra networkId wrReceiver
fromRequest networkId BorrowRequest {brAsset, brAmount, brReceiver} =
  ApiBorrowRequest brAsset brAmount . AddressText <$> toCardanoAddressInEra networkId brReceiver
fromRequest _ RepayRequest {rrAsset, rrAmount} = pure $ ApiRepayRequest rrAsset rrAmount

fromAccountLiquidateRedeemerData
  :: CA.NetworkId -> AccountLiquidateRedeemerData -> Either ToCardanoError ApiAccountLiquidateRedeemerData
fromAccountLiquidateRedeemerData
  networkId
  AccountLiquidateRedeemerData {alrBorrowings, alrCollaterals, alrClearRequests, alrExtraLovelace, alrUserNft} = do
    userNft <-
      maybe
        (Left DeserialisationError)
        (pure . AssetIdText)
        (fromPlutusAssetClass alrUserNft)
    mapClearRequests <- traverse (fromClearRequest networkId) alrClearRequests
    pure (ApiAccountLiquidateRedeemerData alrBorrowings alrCollaterals mapClearRequests alrExtraLovelace userNft)
