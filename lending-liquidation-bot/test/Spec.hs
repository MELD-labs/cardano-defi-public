{-# LANGUAGE NumericUnderscores #-}

import Data.Map qualified as Map
import Data.Monoid (Sum (Sum))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)
import Lending.Types.Manager
  ( GlobalRiskParameters
      ( GlobalRiskParameters
      , grpBatcherFee
      , grpCloseFactor
      , grpCloseFactorHealthFactorThreshold
      , grpCloseFactorMinCollateralThreshold
      , grpLiquidatorIncentive
      , grpMaxLiquidationCloseFactor
      , grpMinAdaUtxo
      , grpProtocolIncentive
      )
  )
import Lending.Types.Pool (AssetInformation (AssetInformation))
import LiquidationBot.Handler (chooseCollateral, fiatValue)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Liquidation bot"
    [ testFiatValue
    , testChooseCollateral
    ]

adaToken :: Asset
adaToken = 0

testMeldAsset :: Asset
testMeldAsset = 1

testToken1Asset :: Asset
testToken1Asset = 2

testToken2Asset :: Asset
testToken2Asset = 3

assetPrices :: Map.Map Asset Price
assetPrices = Map.fromList [(adaToken, 15), (testMeldAsset, 1.5), (testToken1Asset, 0.5), (testToken2Asset, 10)]

testFiatValue :: TestTree
testFiatValue = testCase "Test fiat value calculation" $ do
  let actualMap = Map.fromList [(0, 3_000_000), (2, 7_000_000)]
      expected = Sum 48_500_000
   in assertEqual "" expected (fiatValue assetPrices actualMap)

testChooseCollateral :: TestTree
testChooseCollateral = testCase "Test collateral choosing" $ do
  let gRiskparams =
        GlobalRiskParameters
          { grpCloseFactor = undefined
          , grpMaxLiquidationCloseFactor = undefined
          , grpCloseFactorHealthFactorThreshold = undefined
          , grpCloseFactorMinCollateralThreshold = undefined
          , grpLiquidatorIncentive = 0.1
          , grpProtocolIncentive = 0.05
          , grpMinAdaUtxo = undefined
          , grpBatcherFee = undefined
          }
      assetInfo = AssetInformation 1 1 1 1
      assetInfoMap =
        Map.fromList
          [ (adaToken, assetInfo)
          , (testMeldAsset, assetInfo)
          , (testToken1Asset, assetInfo)
          , (testToken2Asset, assetInfo)
          ]
      suppliedAsset = Map.fromList [(adaToken, 15_000_000), (testToken1Asset, 15_000_000)]
      liquidatingDebt = Map.fromList [(testMeldAsset, 70), (testToken2Asset, 5)] -- 155$
      liquidatingCollateralAsset = [adaToken, testToken1Asset]
      rs =
        chooseCollateral
          gRiskparams
          assetPrices
          assetInfoMap
          suppliedAsset
          liquidatingDebt
          liquidatingCollateralAsset
      expected = Map.fromList [(adaToken, 11), (testToken1Asset, 10)] -- 170$
   in assertEqual "" (Just expected) rs
