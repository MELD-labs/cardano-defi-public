{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lending.Types.LoanToValue (InterestDataExported (..)) where

import Plutarch.Extra.Tagged ()
import PlutusLedgerApi.V2 (Data, toData)
import PlutusTx qualified
import Ply.Core.Class (PlyArg (UPLCRep, toBuiltinArg, toBuiltinArgData))

import Lending.Types.Exchange (Decimal)
import Lending.Types.Orphans ()

data InterestDataExported = InterestDataExported
  { ideUnderLiquidationThreshold :: Bool
  , ideSafeHealthFactorThreshold :: Bool
  , ideLoanToValue :: Decimal
  }

PlutusTx.makeIsDataIndexed ''InterestDataExported [('InterestDataExported, 0)]

instance PlyArg InterestDataExported where
  type UPLCRep InterestDataExported = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData
