{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Api.Types.Aeson
  ( DefaultApiJson (..)
  , HasAesonOptions (..)
  )
where

import Data.Aeson
  ( FromJSON (parseJSON)
  , GFromJSON
  , GToJSON'
  , Options (unwrapUnaryRecords)
  , ToJSON (toJSON)
  , Value
  , Zero
  )
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing qualified as Aeson
import Data.OpenApi (ToSchema)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Internal.Schema (GToSchema)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)
import Servant.API.Generic (Generic (Rep))

class HasAesonOptions a where
  aesonOptions :: Options
  default aesonOptions :: Options
  aesonOptions = (Aeson.aesonPrefix Aeson.camelCase) {unwrapUnaryRecords = True}

newtype DefaultApiJson a = DefaultApiJson {unDefaultApiJson :: a}

instance (HasAesonOptions a, Generic a, GFromJSON Zero (Rep a)) => FromJSON (DefaultApiJson a) where
  parseJSON = (DefaultApiJson <$>) . Aeson.genericParseJSON (aesonOptions @a)

instance (HasAesonOptions a, Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (DefaultApiJson a) where
  toJSON = Aeson.genericToJSON (aesonOptions @a) . unDefaultApiJson

instance (HasAesonOptions a, Typeable a, Generic a, GToSchema (Rep a)) => ToSchema (DefaultApiJson a) where
  declareNamedSchema _ = OpenApi.genericDeclareNamedSchema (OpenApi.fromAesonOptions (aesonOptions @a)) (Proxy @a)
