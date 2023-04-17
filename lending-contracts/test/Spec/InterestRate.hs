{-# LANGUAGE OverloadedStrings #-}

module Spec.InterestRate where

import Data.Map (Map)
import Data.Map qualified as Map
import Plutarch.Api.V2 (PValidator)
import Plutarch.Api.V2 qualified as Plutarch
import Plutarch.Context
  ( SpendingBuilder
  , UTXO
  , credential
  , input
  , mintWith
  , output
  , referenceInput
  , timeRange
  , txId
  , withInlineDatum
  , withRedeemer
  , withRefTxId
  , withSpendingOutRefId
  , withValue
  )
import Plutarch.Test.Precompiled ((@>))
import Plutarch.Test.Precompiled qualified as Plutarch
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V2
  ( Credential (ScriptCredential)
  , Interval (Interval)
  , POSIXTime (POSIXTime)
  , POSIXTimeRange
  , PubKeyHash (PubKeyHash)
  , Validator
  )
import Test.Tasty (TestTree)

import Common
  ( valueFromAsset
  , withValidatorData
  )
import Lending.Contracts.Oracle.OnChain (oracleValidatorTerm)
import Lending.Contracts.Pool.OnChain
  ( validatePoolTerm
  )
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (CumulativeRate)
import Lending.Types.Oracle (OracleDatum (OracleDatum), OracleScriptParams (OracleScriptParams))
import Lending.Types.Pool
  ( AssetInformation (AssetInformation)
  , PoolDatum (PoolDatum)
  , PoolRedeemer (UpdatePoolRedeemer)
  )
import Sample
  ( adaAssetClass
  , adaToken
  , baseRate
  , configTest
  , managerAuthToken
  , minAdaUtxo
  , octRedeemer
  , operatorMigrationNft
  , operatorOracleNft
  , oracleAuthToken
  , oracleCheckerToken
  , poolAuthToken
  )
import Spec.Account
  ( managerInput
  )

poolValidator :: ClosedTerm PValidator
poolValidator = validatePoolTerm managerAuthToken operatorMigrationNft

oracleValidator :: Validator
oracleValidator = Plutarch.mkValidator configTest (oracleValidatorTerm (OracleScriptParams operatorOracleNft))

inputLastUpdatedTime :: POSIXTime
inputLastUpdatedTime = POSIXTime 0

outputLastUpdatedTime :: POSIXTime
outputLastUpdatedTime = POSIXTime 31_536_000_000

supplyAmount :: Integer
supplyAmount = 2_000_000_000

borrowAmount :: Integer
borrowAmount = 1_000_000_000

newCumulativeBorrowingRate :: CumulativeRate
newCumulativeBorrowingRate = 1.312727272727272727

newCumulativeLendingRate :: CumulativeRate
newCumulativeLendingRate = 1.148545454545454545

assetList :: CumulativeRate -> CumulativeRate -> Map Asset AssetInformation
assetList lRate bRate =
  Map.singleton adaToken (AssetInformation (fromInteger supplyAmount) (fromInteger borrowAmount) lRate bRate)

inputAssetList :: Map Asset AssetInformation
inputAssetList = assetList baseRate baseRate

inputPoolDatum :: PoolDatum
inputPoolDatum = PoolDatum inputAssetList inputLastUpdatedTime

outputAssetList :: Map Asset AssetInformation
outputAssetList = assetList newCumulativeLendingRate newCumulativeBorrowingRate

outputPoolDatum :: PoolDatum
outputPoolDatum = PoolDatum outputAssetList outputLastUpdatedTime

closedBoundedInterval :: POSIXTime -> POSIXTime -> POSIXTimeRange
closedBoundedInterval from to = Interval (Interval.lowerBound from) (Interval.strictUpperBound to)

validTimeRange :: POSIXTimeRange
validTimeRange = closedBoundedInterval 31_536_000_000 31_536_060_000

poolRedeemer :: PoolRedeemer
poolRedeemer = UpdatePoolRedeemer

poolCredential :: Credential
poolCredential = ScriptCredential $ Plutarch.validatorHash (Plutarch.mkValidator configTest poolValidator)

oracleCredential :: Credential
oracleCredential = ScriptCredential $ Plutarch.validatorHash oracleValidator

poolUtxo :: PoolDatum -> Integer -> Integer -> UTXO
poolUtxo datum adaAmount nftAmount =
  mconcat
    [ credential poolCredential
    , withInlineDatum datum
    , withValue (valueFromAsset adaAssetClass adaAmount <> valueFromAsset poolAuthToken nftAmount)
    , withRefTxId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withRedeemer UpdatePoolRedeemer
    ]

inputPoolUtxo :: UTXO
inputPoolUtxo = poolUtxo inputPoolDatum (minAdaUtxo + supplyAmount - borrowAmount) 1

outputPoolUtxo :: UTXO
outputPoolUtxo = poolUtxo outputPoolDatum (minAdaUtxo + supplyAmount - borrowAmount) 1

batcherPkh :: PubKeyHash
batcherPkh = PubKeyHash "db8a9a2c692ae19a6c5d4184da07293fe1c4a5b6263eac4c0a9581b1"

oracleDatum :: OracleDatum
oracleDatum = OracleDatum $ Map.singleton adaToken 1_500_000

oracleInput :: UTXO
oracleInput =
  mconcat
    [ credential oracleCredential
    , withInlineDatum oracleDatum
    , withValue (valueFromAsset oracleAuthToken 1 <> valueFromAsset adaAssetClass 2_000_000)
    , withRefTxId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

baseCtx :: SpendingBuilder
baseCtx =
  mconcat
    [ txId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , referenceInput managerInput
    , referenceInput oracleInput
    , withSpendingOutRefId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , timeRange validTimeRange
    , mintWith octRedeemer (valueFromAsset oracleCheckerToken 1)
    ]

successCtx :: SpendingBuilder
successCtx =
  baseCtx
    <> input inputPoolUtxo
    <> output outputPoolUtxo

specs :: TestTree
specs =
  Plutarch.tryFromPTerm "Cumulative rate" poolValidator $
    withValidatorData inputPoolDatum poolRedeemer successCtx
      @> "Calculate correctly"
