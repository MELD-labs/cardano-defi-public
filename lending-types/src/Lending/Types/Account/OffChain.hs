{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lending.Types.Account.OffChain (LiquidationResultData (..)) where

import Plutarch.Extra.Tagged ()
import PlutusLedgerApi.V2 (Data, Value, toData)
import PlutusTx qualified
import Ply.Core.Class (PlyArg (UPLCRep, toBuiltinArg, toBuiltinArgData))

import Lending.Types.Account (AccountDatum)
import Lending.Types.Orphans ()

data LiquidationResultData = LiquidationResultData
  { lrContinuingAccountDatum :: AccountDatum
  , lrNewAccountDatum :: AccountDatum
  , lrNewAccountValue :: Value
  }

PlutusTx.makeIsDataIndexed ''LiquidationResultData [('LiquidationResultData, 0)]

instance PlyArg LiquidationResultData where
  type UPLCRep LiquidationResultData = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData
