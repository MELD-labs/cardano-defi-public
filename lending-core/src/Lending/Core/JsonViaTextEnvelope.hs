{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Core.JsonViaTextEnvelope (JsonViaTextEnvelope (..), WrapAsTx (..)) where

import Cardano.Api qualified as CA
import Control.Lens ((&), (.~), (?~))
import Control.Monad ((>=>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Data (Proxy (Proxy))
import Data.OpenApi (NamedSchema (NamedSchema), OpenApiType (OpenApiObject), ToSchema (declareNamedSchema))
import Data.OpenApi qualified as OpenApi
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)

wrap :: CA.TxBody CA.BabbageEra -> JsonViaTextEnvelope (CA.Tx CA.BabbageEra)
wrap txBody = JsonViaTextEnvelope (CA.signShelleyTransaction txBody [])

unwrap :: JsonViaTextEnvelope (CA.Tx CA.BabbageEra) -> CA.TxBody CA.BabbageEra
unwrap (JsonViaTextEnvelope (CA.Tx txBody _)) = txBody

newtype WrapAsTx = WrapAsTx {wrappedTxBody :: CA.TxBody CA.BabbageEra}

instance FromJSON WrapAsTx where
  parseJSON = (WrapAsTx . unwrap <$>) . parseJSON

instance ToJSON WrapAsTx where
  toJSON = toJSON . wrap . wrappedTxBody

instance ToSchema WrapAsTx where
  declareNamedSchema _ = declareNamedSchema (Proxy @(JsonViaTextEnvelope (CA.Tx CA.BabbageEra)))

newtype JsonViaTextEnvelope a = JsonViaTextEnvelope {unJsonViaTextEnvelope :: a}
  deriving stock (Eq, Show)

instance (CA.HasTextEnvelope a, CA.HasTypeProxy a) => FromJSON (JsonViaTextEnvelope a) where
  parseJSON =
    parseJSON
      >=> either (fail . CA.displayError) (pure . JsonViaTextEnvelope)
        . CA.deserialiseFromTextEnvelope (CA.proxyToAsType Proxy)

instance (CA.HasTextEnvelope a) => ToJSON (JsonViaTextEnvelope a) where
  toJSON = toJSON . CA.serialiseToTextEnvelope Nothing . unJsonViaTextEnvelope

instance (CA.HasTextEnvelope a, Typeable a) => ToSchema (JsonViaTextEnvelope a) where
  declareNamedSchema _ = do
    textSchema <- OpenApi.declareSchemaRef (Proxy @Text)
    let CA.TextEnvelopeType typeName = CA.textEnvelopeType (CA.proxyToAsType (Proxy @a))
    return $
      NamedSchema (Just (Text.pack typeName)) $
        mempty
          & OpenApi.type_ ?~ OpenApiObject
          & OpenApi.properties
            .~ [ ("type", textSchema)
               , ("description", textSchema)
               , ("cborHex", textSchema)
               ]
          & OpenApi.required .~ ["type", "description", "cborHex"]
