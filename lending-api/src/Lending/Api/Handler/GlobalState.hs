{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Lending.Api.Handler.GlobalState (globalStateApi, getGlobalState, calculateAssetState) where

import Cardano.Api qualified as CA
import Control.Concurrent qualified as Concurrent
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Trans.Reader qualified as Reader
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tagged (Tagged (Tagged, unTagged))
import PlutusLedgerApi.V1.Value (adaToken, toString)
import Servant (ServerT)

import Cardano.Api.Extra.AssetId (AssetIdText (AssetIdText))
import Lending.Api.Config
  ( ContractsConfigApi
      ( ContractsConfigApi
      , ccaLendingFunctionBorrowApy
      , ccaLendingFunctionSupplyApy
      , ccaLendingFunctionUtilization
      )
  )
import Lending.Api.Env
  ( AppEnv (AppEnv, aeContractsConfig, aeNetworkParams, aeProtocalStateConnectionPool, aeTokenDecimalMap)
  , AppM
  , runSqlM
  )
import Lending.Api.Types.Exception (ServerError (InvalidScriptNft))
import Lending.Api.Types.GlobalState
  ( AssetState
      ( AssetState
      , asBorrowApy
      , asDisplayName
      , asPrice
      , asRiskParameters
      , asSupplyApy
      , asTotalBorrow
      , asTotalBorrowedValue
      , asTotalSuppliedValue
      , asTotalSupply
      , asUtilization
      )
  , GlobalState
    ( GlobalState
    , gsAccountAuthToken
    , gsAssets
    , gsAverageBorrowApy
    , gsAverageSupplyApy
    , gsGlobalRiskParameters
    , gsOracleCheckerToken
    , gsPoolNft
    , gsTreasuryValue
    , gsTreasuryWithdrawableValue
    )
  , GlobalStateApi
  , QueryGlobalStateResponse (QueryGlobalStateResponse)
  )
import Lending.Core.AccountValue (getBorrowApy, getSupplyApy, getUtilizationRatio)
import Lending.Core.Utils qualified as Utils
import Lending.Index.Query.Manager (getManagerUtxo)
import Lending.Index.Query.Oracle (getOracleUtxo)
import Lending.Index.Query.Pool (getPoolUtxo)
import Lending.Scripts (LendingScript (Account, OracleCheckerToken, Pool))
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, CumulativeRate, Decimal, Fiat, Price, convertFrom)
import Lending.Types.Manager
  ( ManagerDatum
      ( ManagerDatum
      , mdAccountAuthToken
      , mdGlobalRiskParameters
      , mdOracleCheckerToken
      , mdPoolNft
      , mdRiskParameters
      )
  , RiskParameters (rpAssetClassData)
  )
import Lending.Types.Oracle (OracleDatum (OracleDatum, odAssetPrices))
import Lending.Types.Pool
  ( AssetInformation
      ( AssetInformation
      , aiBorrowAmount
      , aiCumulatedInterestRateBorrowing
      , aiCumulatedInterestRateSupplying
      , aiSupplyAmount
      )
  , PoolDatum (PoolDatum, pdAssets)
  )
import Plutarch.Extra.AssetClass (AssetClass (AssetClass, name), fromPlutusAssetClass, toPlutusAssetClass)
import Plutarch.Extra.FixedDecimal (FixedDecimal, convertExp, ediv, emul, toFixedZero)
import TxBuilder.Api
  ( NetworkParams (pparams)
  , UtxoInputWithDatum (UtxoInputWithDatum, uiwdDatum, uiwdUtxo)
  , toTxOutInlineDatum
  )

globalStateApi :: ServerT GlobalStateApi AppM
globalStateApi = queryGlobalStateH

data GlobalUtxos = GlobalUtxos
  { puManager :: UtxoInputWithDatum ManagerDatum
  , puPool :: UtxoInputWithDatum PoolDatum
  , puOracle :: UtxoInputWithDatum OracleDatum
  }

data AllAssetInUsd = AllAssetInUsd
  { aauAllValue :: FixedDecimal 0
  , aauAllValueWithApy :: Decimal
  }

instance Semigroup AllAssetInUsd where
  x <> y =
    AllAssetInUsd
      { aauAllValue = aauAllValue x + aauAllValue y
      , aauAllValueWithApy = aauAllValueWithApy x + aauAllValueWithApy y
      }

instance Monoid AllAssetInUsd where
  mempty = AllAssetInUsd 0 0

queryGlobalStateH :: AppM QueryGlobalStateResponse
queryGlobalStateH = do
  GlobalUtxos
    { puManager = UtxoInputWithDatum {uiwdDatum = managerDatum}
    , puPool
    , puOracle = UtxoInputWithDatum {uiwdDatum = oracleDatum}
    } <-
    runSqlM
      (GlobalUtxos <$> getManagerUtxo <*> getPoolUtxo <*> getOracleUtxo)
      aeProtocalStateConnectionPool
  QueryGlobalStateResponse <$> getGlobalState managerDatum puPool oracleDatum

getGlobalState :: ManagerDatum -> UtxoInputWithDatum PoolDatum -> OracleDatum -> AppM GlobalState
getGlobalState
  ManagerDatum
    { mdRiskParameters
    , mdAccountAuthToken
    , mdPoolNft
    , mdOracleCheckerToken
    , mdGlobalRiskParameters
    }
  UtxoInputWithDatum {uiwdDatum = poolDatum@PoolDatum {pdAssets}, uiwdUtxo}
  OracleDatum {odAssetPrices} =
    do
      assetStateMap <- Map.traverseWithKey (calculateAssetState odAssetPrices pdAssets) mdRiskParameters

      -- Average APY formula: Sum(APY_i * AssetPrice_i * AssetAmount_i) / Sum (AssetPrice_i * AssetAmount_i)
      let averageSupplyApy = getAverageApy $ foldMap (calculateAllAsset asTotalSuppliedValue asSupplyApy) assetStateMap
          averageBorrowApy = getAverageApy $ foldMap (calculateAllAsset asTotalBorrowedValue asBorrowApy) assetStateMap

      poolNftAssetId <-
        maybe
          (Catch.throwM $ InvalidScriptNft Pool mdPoolNft)
          (pure . AssetIdText)
          (fromPlutusAssetClass mdPoolNft)
      oracleCheckerAssetId <-
        maybe
          (Catch.throwM $ InvalidScriptNft OracleCheckerToken mdOracleCheckerToken)
          (pure . AssetIdText)
          (fromPlutusAssetClass mdOracleCheckerToken)
      accountAuthToken <-
        maybe
          (Catch.throwM $ InvalidScriptNft Account mdAccountAuthToken)
          (pure . AssetIdText)
          (fromPlutusAssetClass mdAccountAuthToken)
      networkParamsMvar <- Reader.asks aeNetworkParams
      networkParams <- MonadIO.liftIO $ Concurrent.readMVar networkParamsMvar
      let poolValue = Utils.sumUtxoValue [uiwdUtxo]
          lendingValue = calculateLendingValue mdRiskParameters poolDatum
          minPoolValue =
            Utils.calculateUtxoValueSatisfyMinAda
              (pparams networkParams)
              (Utils.utxoAddress uiwdUtxo)
              lendingValue
              $ toTxOutInlineDatum poolDatum
          withdrawableTreasuryValue = poolValue <> CA.negateValue minPoolValue
          totalPoolValue = poolValue <> CA.negateValue lendingValue
      pure $
        GlobalState
          { gsAssets = assetStateMap
          , gsAccountAuthToken = accountAuthToken
          , gsPoolNft = poolNftAssetId
          , gsOracleCheckerToken = oracleCheckerAssetId
          , gsGlobalRiskParameters = mdGlobalRiskParameters
          , gsTreasuryValue = toAssetMap mdRiskParameters totalPoolValue
          , gsTreasuryWithdrawableValue = toAssetMap mdRiskParameters withdrawableTreasuryValue
          , gsAverageSupplyApy = averageSupplyApy
          , gsAverageBorrowApy = averageBorrowApy
          }

convertPrice :: Integer -> Price -> Price
convertPrice dec pri = Tagged $ (unTagged pri `emul` (10 ^ dec)) `ediv` defaultUnit
  where
    defaultUnit :: FixedDecimal 0
    defaultUnit = 1_000_000

calculateAssetState :: Map Asset Price -> Map Asset AssetInformation -> Asset -> RiskParameters -> AppM AssetState
calculateAssetState priceMap assetInfoMap asset riskParams = do
  AppEnv
    { aeContractsConfig =
      ContractsConfigApi {ccaLendingFunctionBorrowApy, ccaLendingFunctionSupplyApy, ccaLendingFunctionUtilization}
    , aeTokenDecimalMap
    } <-
    Reader.ask
  let price = convertPrice (Map.findWithDefault 6 asset aeTokenDecimalMap) (Map.findWithDefault 0 asset priceMap)
      (lendAmount, borrowAmount) =
        case Map.lookup asset assetInfoMap of
          Just
            ( AssetInformation
                { aiSupplyAmount
                , aiBorrowAmount
                , aiCumulatedInterestRateSupplying
                , aiCumulatedInterestRateBorrowing
                }
              ) ->
              ( convertFrom aiCumulatedInterestRateSupplying aiSupplyAmount
              , convertFrom aiCumulatedInterestRateBorrowing aiBorrowAmount
              )
          Nothing -> (0, 0)
      lendValue = convertFrom price lendAmount
      borrowValue = convertFrom price borrowAmount
  utilization <- getUtilizationRatio ccaLendingFunctionUtilization borrowAmount lendAmount
  borrowApy <- getBorrowApy ccaLendingFunctionBorrowApy riskParams utilization
  supplyApy <- getSupplyApy ccaLendingFunctionSupplyApy riskParams borrowApy utilization
  pure $
    AssetState
      { asRiskParameters = riskParams
      , asPrice = price
      , asTotalSupply = lendAmount
      , asTotalBorrow = borrowAmount
      , asTotalSuppliedValue = lendValue
      , asTotalBorrowedValue = borrowValue
      , asUtilization = utilization
      , asBorrowApy = borrowApy
      , asSupplyApy = supplyApy
      , asDisplayName = getDisplayName $ rpAssetClassData riskParams
      }

calculateLendingValue :: Map Asset RiskParameters -> PoolDatum -> CA.Value
calculateLendingValue rp PoolDatum {pdAssets} =
  fromAssetMap rp $
    Map.map
      ( \AssetInformation
          { aiSupplyAmount
          , aiBorrowAmount
          , aiCumulatedInterestRateSupplying
          , aiCumulatedInterestRateBorrowing
          } ->
            convertFrom aiCumulatedInterestRateSupplying aiSupplyAmount
              - convertFrom aiCumulatedInterestRateBorrowing aiBorrowAmount
      )
      pdAssets

-- \| Convert the Cardano Value to expected type, ignore the un-whitelisted tokens
toAssetMap :: Map Asset RiskParameters -> CA.Value -> Map Asset Actual
toAssetMap rp =
  foldMap
    ( \(k, CA.Quantity a) ->
        foldMap (`Map.singleton` fromInteger a) $ assetOfAssetId rp k
    )
    . CA.valueToList

-- \| Convert the given type to Cardano Value, ignore the un-whitelisted tokens
fromAssetMap :: Map Asset RiskParameters -> Map Asset Actual -> CA.Value
fromAssetMap riskParams =
  CA.valueFromList
    . Map.foldMapWithKey
      ( \k a ->
          foldMap
            (\x -> [(x, CA.Quantity $ toInteger a)])
            (assetIdOfAsset riskParams k)
      )

-- | Find the Asset of the given AssetClass
assetOf :: Map Asset RiskParameters -> AssetClass -> Maybe Asset
assetOf rp assetClass = case Map.toList (Map.filter ((assetClass ==) . rpAssetClassData) rp) of
  [(k, _)] -> Just k
  _ -> Nothing

-- | Find the Asset of the given Cardano Api AssetId
assetOfAssetId :: Map Asset RiskParameters -> CA.AssetId -> Maybe Asset
assetOfAssetId rp = assetOf rp . toPlutusAssetClass

-- | Find the Plutus AssetClass AssetId of the given Asset
assetIdOf :: Map Asset RiskParameters -> Asset -> Maybe AssetClass
assetIdOf rp asset =
  rpAssetClassData <$> Map.lookup asset rp

-- | Find the Cardano Api AssetId of the given Asset
assetIdOfAsset :: Map Asset RiskParameters -> Asset -> Maybe CA.AssetId
assetIdOfAsset rp asset =
  assetIdOf rp asset >>= fromPlutusAssetClass

calculateAllAsset :: (AssetState -> Fiat) -> (AssetState -> CumulativeRate) -> AssetState -> AllAssetInUsd
calculateAllAsset getValue getApy assetState =
  let totalValue = toFixedZero $ unTagged $ getValue assetState
      apy = unTagged $ getApy assetState
   in AllAssetInUsd
        { aauAllValue = totalValue
        , aauAllValueWithApy = apy `emul` totalValue
        }

getAverageApy :: AllAssetInUsd -> FixedDecimal 4
getAverageApy AllAssetInUsd {aauAllValue, aauAllValueWithApy} =
  if aauAllValue == 0
    then 0
    else convertExp (aauAllValueWithApy `ediv` aauAllValue)

getDisplayName :: AssetClass -> String
getDisplayName AssetClass {name} =
  if name == adaToken then "Ada" else toString name
