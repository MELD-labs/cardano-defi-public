{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lending.Types.Exchange
  ( type (:>)
  , Actual
  , Receipt
  , Fiat
  , Decimal
  , CumulativeRate
  , Price
  , LtvRatio
  , convertFrom
  , convertTo
  )
where

import Data.Tagged (Tagged (Tagged, unTagged))
import GHC.TypeLits (KnownNat, Symbol)
import Plutarch.Extra.FixedDecimal (FixedDecimal, fixedDenominator, fixedNumerator)

data (:>) (a :: Symbol) (b :: Symbol)

type Actual = Tagged "Actual" Integer

type Receipt = Tagged "Receipt" Integer

type Fiat = Tagged "Fiat" Integer

type Decimal = FixedDecimal 18

type CumulativeRate = Tagged ("Receipt" :> "Actual") Decimal

type Price = Tagged ("Actual" :> "Fiat") (FixedDecimal 18)

type LtvRatio = Tagged ("Fiat" :> "Fiat") (FixedDecimal 18)

convertFrom :: KnownNat u => Tagged (a :> b) (FixedDecimal u) -> Tagged a Integer -> Tagged b Integer
convertFrom (Tagged rate) receipt =
  let num = fixedNumerator rate
      den = fixedDenominator rate
   in Tagged $ unTagged receipt * num `div` den

convertTo :: KnownNat u => Tagged (a :> b) (FixedDecimal u) -> Tagged b Integer -> Tagged a Integer
convertTo (Tagged rate) receipt =
  let num = fixedNumerator rate
      den = fixedDenominator rate
   in Tagged $ unTagged receipt * den `div` num
