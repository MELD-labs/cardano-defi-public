module Lending.Types.Percent (percent) where

import GHC.TypeLits (KnownNat)
import Plutarch.Extra.FixedDecimal (FixedDecimal, ediv, toFixedZero)

class ToPercent a where
  percent :: Integer -> a

instance KnownNat u => ToPercent (FixedDecimal u) where
  percent a = ediv (fromInteger a) (toFixedZero 100)
