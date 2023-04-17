{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Types.Orphans () where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tagged (Tagged (unTagged))
import GHC.TypeLits (KnownNat)
import Plutarch.Extra.FixedDecimal (FixedDecimal, fixedNumerator)
import PlutusLedgerApi.V2
  ( Address
  , Credential
  , Data
  , OutputDatum
  , PubKeyHash (PubKeyHash)
  , StakingCredential
  , TxOut
  , Value
  , toData
  )
import PlutusTx (FromData (fromBuiltinData), ToData (toBuiltinData), UnsafeFromData (unsafeFromBuiltinData))
import PlutusTx.AssocMap qualified as AssocMap

import Plutarch.Orphans ()
import Ply.Core.Class (PlyArg (ToDataConstraint, UPLCRep, toBuiltinArg, toBuiltinArgData))

deriving anyclass instance (FromJSON k, FromJSON v) => FromJSON (AssocMap.Map k v)
deriving anyclass instance (ToJSON k, ToJSON v) => ToJSON (AssocMap.Map k v)

deriving newtype instance ToJSON PubKeyHash
deriving newtype instance FromJSON PubKeyHash

deriving anyclass instance ToJSON Credential
deriving anyclass instance FromJSON Credential

deriving anyclass instance ToJSON StakingCredential
deriving anyclass instance FromJSON StakingCredential

deriving anyclass instance ToJSON Address
deriving anyclass instance FromJSON Address

instance KnownNat exp => FromJSON (FixedDecimal exp) where
  parseJSON = Aeson.withScientific "FixedDecimal" $ pure . fromRational . toRational

instance KnownNat exp => ToJSON (FixedDecimal exp) where
  toJSON = Aeson.Number . fromRational . toRational

deriving anyclass instance ToJSON Value
deriving anyclass instance FromJSON Value

instance (ToData k, ToData v) => ToData (Map k v) where
  toBuiltinData = toBuiltinData . AssocMap.fromList . Map.toList

instance (Ord k, FromData k, FromData v) => FromData (Map k v) where
  fromBuiltinData = (Map.fromList . AssocMap.toList <$>) . fromBuiltinData

instance (Ord k, UnsafeFromData k, UnsafeFromData v) => UnsafeFromData (Map k v) where
  unsafeFromBuiltinData = Map.fromList . AssocMap.toList . unsafeFromBuiltinData

instance PlyArg (FixedDecimal exp) where
  type UPLCRep (FixedDecimal exp) = UPLCRep Integer
  type ToDataConstraint (FixedDecimal exp) = ToDataConstraint Integer
  toBuiltinArg = toBuiltinArg . fixedNumerator
  toBuiltinArgData = toBuiltinArgData . fixedNumerator

instance (PlyArg a) => PlyArg (Tagged sym a) where
  type UPLCRep (Tagged sym a) = UPLCRep a
  type ToDataConstraint (Tagged sym a) = ToDataConstraint a
  toBuiltinArg :: Tagged sym a -> UPLCRep (Tagged sym a)
  toBuiltinArg = toBuiltinArg . unTagged
  toBuiltinArgData = toBuiltinArgData . unTagged

instance PlyArg OutputDatum where
  type UPLCRep OutputDatum = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData

instance PlyArg TxOut where
  type UPLCRep TxOut = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData
