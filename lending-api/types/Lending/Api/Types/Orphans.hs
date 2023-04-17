{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan ToSchema typeclass instances for Plutus types.
module Lending.Api.Types.Orphans () where

import Cardano.Api qualified as CA
import Control.Lens ((&), (.~), (?~))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.OpenApi
  ( HasDescription (description)
  , HasProperties (properties)
  , HasType (type_)
  , NamedSchema (NamedSchema)
  , OpenApiType (OpenApiObject)
  , ToParamSchema (toParamSchema)
  , ToSchema (declareNamedSchema)
  , declareSchemaRef
  , sketchSchema
  )
import Data.OpenApi.Internal.Schema (unnamed)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Typeable (Typeable)
import PlutusLedgerApi.V2
  ( Address (Address)
  , Credential (PubKeyCredential)
  , POSIXTime (POSIXTime)
  )
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

import Cardano.Api.Extra.AssetId (AssetIdText (AssetIdText, unAssetIdText), parseAssetId, renderAssetId)
import Cardano.Api.Shelley qualified as CA
import Cardano.Index.ChainPoint.Model (ChainPoint (ChainPoint))
import Cardano.Index.Data.AddressText (AddressText (AddressText, unAddressText))
import Cardano.Index.Data.RawBytesHex (RawBytesHex)
import Cardano.Index.Orphans (BlockHash)
import Lending.Core.AccountValue (AccountId (AccountId))
import Lending.Types.Account (AccountDatum, AccountLiquidateRedeemerData, ClearRequest, Request)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, Decimal, Fiat, LtvRatio, Price, Receipt)
import Lending.Types.Manager (GlobalRiskParameters, ManagerDatum, RiskParameters)
import Lending.Types.Pool (AssetInformation, CumulativeRate, PoolDatum)
import Plutarch.Extra.AssetClass (AssetClass (AssetClass))
import Plutarch.Extra.FixedDecimal (FixedDecimal)
import TxBuilder.Api (UtxoInput (UtxoInput), UtxoInputWithDatum)

mockTxIn :: CA.TxIn
mockTxIn = CA.TxIn (CA.TxId "bba41b340501b9c43f5a293b0936611f04a79b6ebbc9751bd8abf18d12c10e05") (CA.TxIx 1)

mockTxId :: CA.TxId
mockTxId = CA.TxId "bba41b340501b9c43f5a293b0936611f04a79b6ebbc9751bd8abf18d12c10e05"

mockAssetClass :: AssetClass
mockAssetClass = AssetClass "4ebcb13f01e735976486b3c39072eed6d9f362e916fb04fe2c37f951" "MELD"

mockAssetId :: AssetIdText
mockAssetId = AssetIdText (CA.AssetId "4ebcb13f01e735976486b3c39072eed6d9f362e916fb04fe2c37f951" "MELD")

mockAddress :: Address
mockAddress = Address (PubKeyCredential "0185545935760c5e370d01e6f4fedbb89b7fd79e115f2837cfab9ea8") Nothing

mockStakeAddress :: CA.StakeAddress
mockStakeAddress =
  fromJust $ CA.deserialiseAddress CA.AsStakeAddress "stake_test1uqgeh3yx7z9zy8zr0920zc7q69ccmm5xz32rg4u0vgrr44s04gfuv"

mockApiAddress :: CA.AddressInEra CA.BabbageEra
mockApiAddress =
  CA.makeShelleyAddressInEra
    (CA.Testnet (CA.NetworkMagic 42))
    (CA.PaymentCredentialByKey "0185545935760c5e370d01e6f4fedbb89b7fd79e115f2837cfab9ea8")
    CA.NoStakeAddress

mockValue :: CA.Value
mockValue = CA.lovelaceToValue 10

mockUtxoInput :: UtxoInput
mockUtxoInput =
  UtxoInput
    mockTxIn
    ( CA.TxOut
        mockApiAddress
        (CA.TxOutValue CA.MultiAssetInBabbageEra mockValue)
        CA.TxOutDatumNone
        CA.ReferenceScriptNone
    )

mockBlockHash :: RawBytesHex BlockHash
mockBlockHash = "95799619033b7d4ef00816ce9d11a82171a663d075181ef6d326b9425f93adf8"

mockBlockNo :: CA.BlockNo
mockBlockNo = 27

mockSlotNo :: CA.SlotNo
mockSlotNo = 246

mockUTCTime :: UTCTime
mockUTCTime = posixSecondsToUTCTime 1000

mockChainPoint :: ChainPoint
mockChainPoint =
  ChainPoint mockBlockHash mockBlockNo mockSlotNo mockUTCTime

instance {-# OVERLAPPING #-} ToSchema v => ToSchema (Map Asset v) where
  declareNamedSchema _ = do
    schema <- declareSchemaRef (Proxy :: Proxy v)
    return $
      unnamed $
        mempty
          & type_ ?~ OpenApiObject
          & description ?~ "Mapping from an Asset (serialized as string) to an object."
          & properties .~ InsOrdHashMap.fromList ((,schema) . Text.pack . show @Integer <$> [0, 1, 4, 6])

instance ToSchema CA.TxIn where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "UTXO") $
        sketchSchema mockTxIn
          & description ?~ "Cardano UTXO in format of {transaction_id}#{transaction_output_Index}."

instance ToSchema CA.TxId where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "TxId") $
        sketchSchema mockTxId
          & description ?~ "Cardano TxId in format of {transaction_id}"

instance ToSchema AssetClass where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "AssetClass") $
        sketchSchema mockAssetClass
          & description ?~ "Cardano asset type in format of {currency_symbol}.{token_name}."

instance ToSchema AssetIdText where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "AssetId") $
        sketchSchema mockAssetId
          & description ?~ "Cardano asset type in format of {currency_symbol}.{token_name}."

instance ToParamSchema AssetIdText where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

instance ToParamSchema CA.TxIn where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

instance ToParamSchema (AddressText a) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

instance FromHttpApiData AssetIdText where
  parseUrlPiece = maybe (Left "Invalid AssetIdText") (Right . AssetIdText) . parseAssetId

instance (CA.SerialiseAddress a) => FromHttpApiData (AddressText a) where
  parseUrlPiece =
    maybe (Left "Invalid AddressText") (Right . AddressText) . CA.deserialiseAddress (CA.proxyToAsType Proxy)

instance ToHttpApiData AssetIdText where
  toUrlPiece = renderAssetId . unAssetIdText

instance (CA.SerialiseAddress a) => ToHttpApiData (AddressText a) where
  toUrlPiece = CA.serialiseAddress . unAddressText

instance Typeable a => ToSchema (AddressText a) where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "AddressText") $
        sketchSchema mockApiAddress
          & description ?~ "Cardano address in Bech32 format."

instance ToSchema Address where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "Address") $
        sketchSchema mockAddress
          & description ?~ "Cardano address in object."

instance ToSchema CA.StakeAddress where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "StakeAddress") $
        sketchSchema mockStakeAddress
          & description ?~ "Cardano staking address."

deriving newtype instance ToSchema POSIXTime

instance ToSchema Decimal where
  declareNamedSchema _ = pure $ NamedSchema (Just "Decimal") $ sketchSchema @Decimal 0.5

instance ToSchema Asset where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "Asset") $
        sketchSchema @Asset 0
          & description ?~ "Short id for asset to identify within MELD lending and borrowing protocol."

instance ToSchema CumulativeRate where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "CumulativeRate") $
        sketchSchema @CumulativeRate 1.1
          & description ?~ "Continuously increasing exchange rate between receipt and actual token to accrue yield."

instance ToSchema Receipt where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "Receipt") $
        sketchSchema @Receipt 100
          & description ?~ "Balance of receipt token recorded in contract datums."

instance ToSchema Actual where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "Actual") $
        sketchSchema @Actual 110
          & description ?~ "Balance of real asset."

instance ToSchema Fiat where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "Fiat") $
        sketchSchema @Fiat 120
          & description ?~ "Value of real asset."

instance ToSchema Price where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "Price") $
        sketchSchema @Price 0.02
          & description ?~ "Asset price recorded by oracle."

instance ToSchema LtvRatio where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "LtvRatio") $
        sketchSchema @LtvRatio 0.65
          & description ?~ "Weighted collateral value to calculate loan-to-value and liquidation threshold."

instance ToSchema CA.Value where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "Value") $
        sketchSchema mockValue
          & description ?~ "Cardano Value in format of list asset with amount."

instance ToSchema UtxoInput where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "UtxoInput") $
        sketchSchema mockUtxoInput
          & description ?~ "Cardano UtxoInput in format of list asset and amount."

instance ToSchema ChainPoint where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "ChainPoint") $
        sketchSchema mockChainPoint
          & description ?~ "Chain point information."

instance ToSchema (FixedDecimal 4) where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "FixedDecimal") $
        sketchSchema @(FixedDecimal 4) 0.5
          & description ?~ "Sync progress of indexer."

instance ToSchema CA.SlotNo where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "SlotNo") $
        sketchSchema mockSlotNo
          & description ?~ "Slot no in cardano chain"

instance ToSchema RiskParameters
instance ToSchema GlobalRiskParameters
instance ToSchema ManagerDatum
instance ToSchema PoolDatum
instance ToSchema Request
instance ToSchema ClearRequest
instance ToSchema AccountDatum
instance ToSchema (UtxoInputWithDatum AccountDatum)
instance ToSchema AssetInformation
instance ToSchema AccountLiquidateRedeemerData

deriving newtype instance ToSchema AccountId
