{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lending.Types.Pool.OffChain (MatcherContextData (..), StateChangeData (..), RequestResultData (..)) where

import Data.Map (Map)
import Data.Map qualified as Map
import PlutusLedgerApi.V2 (Data, TxOut, Value, toData)
import PlutusTx qualified

import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price, Receipt)
import Lending.Types.Manager (RiskParameters)
import Lending.Types.Pool (AssetInformation)
import Plutarch.Extra.AssetClass (AssetClass)
import Ply.Core.Class (PlyArg (UPLCRep, toBuiltinArg, toBuiltinArgData))

data MatcherContextData = MatcherContextData
  { mcdAssetPrices :: Map Asset Price
  , mcdAssets :: Map Asset AssetInformation
  , mcdRiskParameters :: Map Asset RiskParameters
  , mcdMinAdaUtxo :: Integer
  , mcdAccountAuthToken :: AssetClass
  }
  deriving stock (Show)

data StateChangeData = StateChangeData
  { scdSupply :: Map Asset Receipt
  , scdBorrow :: Map Asset Receipt
  , scdPoolReceivable :: Value
  , scdPoolPayable :: Value
  }
  deriving stock (Show)

instance Semigroup StateChangeData where
  x <> y =
    StateChangeData
      { scdSupply = Map.unionWith (+) (scdSupply x) (scdSupply y)
      , scdBorrow = Map.unionWith (+) (scdBorrow x) (scdBorrow y)
      , scdPoolReceivable = scdPoolReceivable x <> scdPoolReceivable y
      , scdPoolPayable = scdPoolPayable x <> scdPoolPayable y
      }

instance Monoid StateChangeData where
  mempty = StateChangeData mempty mempty mempty mempty

data RequestResultData = RequestResultData
  { rrdOutputs :: [TxOut]
  , rrdAccountStateChange :: StateChangeData
  }

instance Semigroup RequestResultData where
  x <> y =
    RequestResultData
      { rrdOutputs = rrdOutputs x <> rrdOutputs y
      , rrdAccountStateChange = rrdAccountStateChange x <> rrdAccountStateChange y
      }

instance Monoid RequestResultData where
  mempty = RequestResultData mempty mempty

PlutusTx.makeIsDataIndexed ''MatcherContextData [('MatcherContextData, 0)]
PlutusTx.makeIsDataIndexed ''StateChangeData [('StateChangeData, 0)]
PlutusTx.makeIsDataIndexed ''RequestResultData [('RequestResultData, 0)]

instance PlyArg MatcherContextData where
  type UPLCRep MatcherContextData = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData

instance PlyArg StateChangeData where
  type UPLCRep StateChangeData = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData

instance PlyArg RequestResultData where
  type UPLCRep RequestResultData = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData
