{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Core.AccountValue where

import Cardano.Api qualified as CA
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as MonadIO
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Sum (Sum))
import Data.Tagged (Tagged (Tagged))
import PlutusLedgerApi.V2 (POSIXTime (POSIXTime), Value)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

import Cardano.Api.Extra.Adapters (fromCardanoTxOut)
import Data.Ratio ((%))
import Lending.Core.Utils (fromTxOutUtxoToTx)
import Lending.Functions
  ( LendingFunction
      ( LendingFunctionBorrowApy
      , LendingFunctionCalculateAccountValue
      , LendingFunctionCalculateLiquidationResult
      , LendingFunctionInterestRate
      , LendingFunctionProcessAccount
      , LendingFunctionSupplyApy
      , LendingFunctionUpdatePool
      , LendingFunctionUtilization
      )
  )
import Lending.Types.Account
  ( AccountDatum (adNormalRequests)
  , AccountLiquidateRedeemerData
  , Request (SupplyRequest)
  )
import Lending.Types.Account.OffChain (LiquidationResultData)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, CumulativeRate, Decimal, Fiat, LtvRatio, Price, convertFrom)
import Lending.Types.Manager (ManagerDatum, RiskParameters (rpLiquidationThreshold, rpMaxLoanToValue))
import Lending.Types.Orphans ()
import Lending.Types.Pool
  ( AssetInformation
      ( AssetInformation
      , aiBorrowAmount
      , aiCumulatedInterestRateBorrowing
      , aiCumulatedInterestRateSupplying
      , aiSupplyAmount
      )
  )
import Lending.Types.Pool.OffChain (MatcherContextData, RequestResultData, StateChangeData)
import Plutarch.Import.Functions (FunctionTypedScript, apply)
import Plutarch.Import.HList (HList (HNil, (:~:)))
import TxBuilder.Api (UtxoInputWithDatum)

newtype AccountId = AccountId {aiOriginalRef :: CA.TxIn}
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data ExtractedAccount = ExtractedAccount
  { eaAccountId :: AccountId
  , eaAccountRef :: CA.TxIn
  , eaAccountUtxo :: UtxoInputWithDatum AccountDatum
  }

data AccountValue = AccountValue
  { avSupplies :: Map Asset Actual
  , avBorrowings :: Map Asset Actual
  }
  deriving stock (Show)

tryUpdateInterestRate
  :: MonadIO m
  => FunctionTypedScript 'LendingFunctionInterestRate
  -> Map Asset RiskParameters
  -> Map Asset AssetInformation
  -> Integer
  -> m (Map Asset AssetInformation)
tryUpdateInterestRate typedScript riskParameters mapAssetSupply deltaTime =
  MonadIO.liftIO $
    apply @LendingFunctionInterestRate PlutusTx.Map typedScript $
      toAssocMap riskParameters :~: toAssocMap mapAssetSupply :~: deltaTime :~: HNil

checkMaxLoanToValue
  :: Map Asset RiskParameters
  -> Map Asset Price
  -> Map Asset Actual
  -> Map Asset Actual
  -> Bool
checkMaxLoanToValue = validate rpMaxLoanToValue

checkLiquidationThreshold
  :: Map Asset RiskParameters
  -> Map Asset Price
  -> Map Asset Actual
  -> Map Asset Actual
  -> Bool
checkLiquidationThreshold = validate rpLiquidationThreshold

validate
  :: (RiskParameters -> LtvRatio)
  -> Map Asset RiskParameters
  -> Map Asset Price
  -> Map Asset Actual
  -> Map Asset Actual
  -> Bool
validate getLtvRatio riskParams priceMap collateral borrow = totalBorrow <= totalCollateral
  where
    totalCollateral = Map.foldMapWithKey (weightedCollateralValue (getLtvRatio <$> riskParams) priceMap) collateral
    totalBorrow = Map.foldMapWithKey (borrowValue priceMap) borrow

getLiquidationResult
  :: MonadIO m
  => FunctionTypedScript 'LendingFunctionCalculateLiquidationResult
  -> ManagerDatum
  -> AssocMap.Map Asset Price
  -> AssocMap.Map Asset AssetInformation
  -> AccountDatum
  -> AccountLiquidateRedeemerData
  -> m LiquidationResultData
getLiquidationResult
  typedScript
  managerDatum
  assetPrices
  poolDatum
  accountDatum
  liquidateRedeemerData =
    MonadIO.liftIO $
      apply @LendingFunctionCalculateLiquidationResult id typedScript $
        managerDatum
          :~: assetPrices
          :~: poolDatum
          :~: accountDatum
          :~: liquidateRedeemerData
          :~: HNil

getValueFromAccount
  :: MonadIO m
  => FunctionTypedScript 'LendingFunctionCalculateAccountValue
  -> ManagerDatum
  -> AccountDatum
  -> m Value
getValueFromAccount typedScript manager account =
  MonadIO.liftIO $
    apply @LendingFunctionCalculateAccountValue PlutusTx.Map typedScript $
      manager
        :~: account
        :~: HNil

getLoanToValue
  :: Map Asset Price
  -> Map Asset Actual
  -> Map Asset Actual
  -> Maybe LtvRatio
getLoanToValue priceMap collateral borrow =
  if totalCollateral == 0 then Nothing else Just (fromRational (fromSum totalBorrow % fromSum totalCollateral))
  where
    totalCollateral = Map.foldMapWithKey (weightedCollateralValue (1 <$ priceMap) priceMap) collateral
    totalBorrow = Map.foldMapWithKey (borrowValue priceMap) borrow
    fromSum :: Sum Fiat -> Integer
    fromSum (Sum (Tagged a)) = a

weightedCollateralValue
  :: Map Asset LtvRatio
  -> Map Asset Price
  -> Asset
  -> Actual
  -> Sum Fiat
weightedCollateralValue ltvMap priceMap asset =
  Sum . convertFrom ltvRatio . convertFrom price
  where
    ltvRatio = Map.findWithDefault 0 asset ltvMap
    price = Map.findWithDefault 0 asset priceMap

borrowValue :: Map Asset Price -> Asset -> Actual -> Sum Fiat
borrowValue priceMap asset = Sum . convertFrom price
  where
    price = Map.findWithDefault 0 asset priceMap

getBorrowApy
  :: MonadIO m
  => FunctionTypedScript 'LendingFunctionBorrowApy
  -> RiskParameters
  -> Decimal
  -> m CumulativeRate
getBorrowApy typedScript riskParameter utilization =
  MonadIO.liftIO $ do
    apply @LendingFunctionBorrowApy PlutusTx.I typedScript $ riskParameter :~: utilization :~: HNil

getSupplyApy
  :: MonadIO m
  => FunctionTypedScript 'LendingFunctionSupplyApy
  -> RiskParameters
  -> CumulativeRate
  -> Decimal
  -> m CumulativeRate
getSupplyApy typedScript riskParameter bRate utilization =
  MonadIO.liftIO $
    apply @LendingFunctionSupplyApy PlutusTx.I typedScript $
      riskParameter :~: bRate :~: utilization :~: HNil

getUtilizationRatio
  :: MonadIO m
  => FunctionTypedScript 'LendingFunctionUtilization
  -> Actual
  -> Actual
  -> m Decimal
getUtilizationRatio typedScript debtAmount supplyAmount =
  MonadIO.liftIO $ do
    apply @LendingFunctionUtilization PlutusTx.I typedScript $ debtAmount :~: supplyAmount :~: HNil

tryProcessAccount
  :: MonadIO m
  => FunctionTypedScript 'LendingFunctionProcessAccount
  -> MatcherContextData
  -> CA.TxOut CA.CtxUTxO CA.BabbageEra
  -> m RequestResultData
tryProcessAccount typedScript ctx txOut =
  MonadIO.liftIO $
    apply @LendingFunctionProcessAccount id typedScript $
      ctx :~: fromCardanoTxOut (fromTxOutUtxoToTx txOut) :~: HNil

tryUpdatePool
  :: MonadIO m
  => FunctionTypedScript 'LendingFunctionUpdatePool
  -> Map Asset RiskParameters
  -> StateChangeData
  -> Integer
  -> Map Asset AssetInformation
  -> m (Map Asset AssetInformation)
tryUpdatePool typedScript riskParams stateChange deltaTime assetMap =
  MonadIO.liftIO $
    apply @LendingFunctionUpdatePool PlutusTx.Map typedScript $
      toAssocMap riskParams :~: stateChange :~: deltaTime :~: toAssocMap assetMap :~: HNil

toAssocMap :: Map k v -> AssocMap.Map k v
toAssocMap = AssocMap.fromList . Map.toList

-- TODO: port from on-chain
getDeltaTime :: POSIXTime -> POSIXTime -> Integer
getDeltaTime (POSIXTime oldTime) (POSIXTime newTime) = (newTime - oldTime) `div` 1000

defaultAssetInformation :: AssetInformation
defaultAssetInformation =
  AssetInformation
    { aiSupplyAmount = 0
    , aiBorrowAmount = 0
    , aiCumulatedInterestRateSupplying = 1
    , aiCumulatedInterestRateBorrowing = 1
    }

extractNewAsset :: Request -> Map Asset AssetInformation
extractNewAsset (SupplyRequest asset _) = Map.singleton asset defaultAssetInformation
extractNewAsset _ = mempty

fillNewAssets :: [AccountDatum] -> Map Asset AssetInformation -> Map Asset AssetInformation
fillNewAssets accounts = (<> foldMap extractNewAsset (accounts >>= adNormalRequests))
