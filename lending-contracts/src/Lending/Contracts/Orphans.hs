{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Contracts.Orphans where

import Data.Tagged (Tagged)
import GHC.TypeLits (Symbol)
import Lending.Types.Exchange qualified as ExchangeOffChain
import Plutarch.Api.V2 (PTxOut)
import Plutarch.Extra.AssetClass (PAssetClassData)
import Plutarch.Extra.ExchangeRate qualified as ExchangeOnChain
import Plutarch.Extra.FixedDecimal (FixedDecimal, PFixedDecimal)
import Plutarch.Extra.Tagged (PTagged)
import PlutusLedgerApi.V2 (TxOut)
import Ply.Plutarch (PlyArgOf)

instance PTryFrom PData (PAsData PAssetClassData)
instance PTryFrom PData (PAsData (PBuiltinList PAssetClassData))
instance PTryFrom PData (PAsData (PFixedDecimal unit))
instance PPartialOrd PAssetClassData
instance POrd PAssetClassData
instance PTryFrom PData (PAsData PBool)

type instance PlyArgOf (PFixedDecimal unit) = FixedDecimal unit
type instance PlyArgOf (PTagged (sym :: Symbol) a) = Tagged sym (PlyArgOf a)
type instance PlyArgOf (PTagged (a ExchangeOnChain.:> b) rate) = Tagged (a ExchangeOffChain.:> b) (PlyArgOf rate)
type instance PlyArgOf PTxOut = TxOut
