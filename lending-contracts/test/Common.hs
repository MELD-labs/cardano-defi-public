{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Common where

import Plutarch.Context (MintingBuilder, SpendingBuilder)
import Plutarch.Context qualified as Plutarch
import Plutarch.Extra.AssetClass (AssetClass (AssetClass))
import Plutarch.Extra.FixedDecimal (convertExp, emul, fromFixedZero, toFixedZero)
import PlutusLedgerApi.V1.Value (Value, singleton)
import PlutusTx (Data)
import PlutusTx qualified

import Lending.Types.Exchange (Decimal)

withValidatorData :: (PlutusTx.ToData datum, PlutusTx.ToData redeemer) => datum -> redeemer -> SpendingBuilder -> [Data]
withValidatorData datum redeemer ctx =
  [ PlutusTx.toData datum
  , PlutusTx.toData redeemer
  , PlutusTx.toData (Plutarch.buildSpending' (Plutarch.mkNormalized ctx))
  ]

withPolicyData :: (PlutusTx.ToData redeemer) => redeemer -> MintingBuilder -> [Data]
withPolicyData redeemer ctx =
  [ PlutusTx.toData redeemer
  , PlutusTx.toData (Plutarch.buildMinting' (Plutarch.mkNormalized ctx))
  ]

valueFromAsset :: AssetClass -> Integer -> Value
valueFromAsset (AssetClass cs tn) = singleton cs tn

calculatePercent :: Integer -> Decimal -> Integer
calculatePercent value per = fromFixedZero . convertExp $ toFixedZero value `emul` per

data TypeActionWithTreasury = SupplyTreasury | WithdrawTreasury
  deriving stock (Eq)
