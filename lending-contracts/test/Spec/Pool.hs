{-# LANGUAGE OverloadedStrings #-}

module Spec.Pool where

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
  , pubKey
  , referenceInput
  , timeRange
  , txId
  , withInlineDatum
  , withRedeemer
  , withRefTxId
  , withSpendingOutRefId
  , withValue
  )
import Plutarch.Test.Precompiled ((@!>), (@>))
import Plutarch.Test.Precompiled qualified as Plutarch
import PlutusLedgerApi.V1 (POSIXTimeRange)
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Value qualified as PlutusValue
import PlutusLedgerApi.V2
  ( Credential (ScriptCredential)
  , Interval (Interval)
  , POSIXTime (POSIXTime)
  , PubKeyHash (PubKeyHash)
  , Validator
  , Value
  )
import Test.Tasty (TestTree)

import Common
  ( TypeActionWithTreasury (SupplyTreasury, WithdrawTreasury)
  , calculatePercent
  , valueFromAsset
  , withValidatorData
  )
import Lending.Contracts.Oracle.OnChain (oracleValidatorTerm)
import Lending.Contracts.Pool.OnChain
  ( validatePoolTerm
  )
import Lending.Types.Account
  ( AccountDatum
      ( AccountDatum
      , adBorrowings
      , adCollateralAssets
      , adCollateralUpdate
      , adNormalRequests
      , adSupplies
      )
  , AccountRedeemer (AccountApplyRedeemer)
  , ClearRequest (ClearBorrowing, ClearSupplying)
  , Request (BorrowRequest, RepayRequest, SupplyRequest, WithdrawRequest)
  , adClearRequests
  )
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (CumulativeRate, Decimal)
import Lending.Types.Oracle (OracleDatum (OracleDatum), OracleScriptParams (OracleScriptParams))
import Lending.Types.Percent (percent)
import Lending.Types.Pool
  ( AssetInformation (AssetInformation)
  , PoolDatum (PoolDatum, pdAssets, pdLastUpdatedTime)
  , PoolRedeemer (MigratePoolRedeemer, UpdatePoolRedeemer, UpdateTreasuryPoolRedeemer)
  )
import Sample
  ( accountAuthToken
  , adaAssetClass
  , adaToken
  , baseRate
  , batcherFee
  , configTest
  , defaultExtraLovelace
  , managerAuthToken
  , minAdaUtxo
  , octRedeemer
  , operatorMigrationNft
  , operatorOracleNft
  , operatorPoolNft
  , oracleAuthToken
  , oracleCheckerToken
  , poolAuthToken
  , testMeldAsset
  , testMeldAssetClass
  , updatedBorrowingRate
  , updatedLendingRate
  , userNFT
  )
import Spec.Account
  ( accountCredential
  , accountValueWithoutMinAda
  , calculateMinAdaValueAccount
  , managerInput
  , minAdaValue
  , userAddress
  , userInputUtxo
  , userPkh
  )

poolValidator :: ClosedTerm PValidator
poolValidator = validatePoolTerm managerAuthToken operatorMigrationNft

oracleValidator :: Validator
oracleValidator = Plutarch.mkValidator configTest (oracleValidatorTerm (OracleScriptParams operatorOracleNft))

inputLastUpdatedTime :: POSIXTime
inputLastUpdatedTime = POSIXTime 0

outputLastUpdatedTime :: POSIXTime
outputLastUpdatedTime = POSIXTime 1_000

supplyAmount :: Integer
supplyAmount = 1_000_000

clearBorrowingLimited :: Integer
clearBorrowingLimited = 2_100_000

withdrawAmount :: Integer
withdrawAmount = 500_000

borrowAmount :: Integer
borrowAmount = 500_000

collateralAmount :: Integer
collateralAmount = 550_000

protocolPercent :: Decimal
protocolPercent = percent 5

protocolIncentiveAmount :: Integer
protocolIncentiveAmount = calculatePercent collateralAmount protocolPercent

supplyRequest :: Request
supplyRequest = SupplyRequest adaToken $ fromInteger supplyAmount

withdrawRequest :: Request
withdrawRequest = WithdrawRequest adaToken (fromInteger withdrawAmount) userAddress

borrowRequest :: Request
borrowRequest = BorrowRequest adaToken (fromInteger borrowAmount) userAddress

repayRequest :: Request
repayRequest = RepayRequest adaToken $ fromInteger borrowAmount

genesisPoolDatum :: PoolDatum
genesisPoolDatum = PoolDatum mempty inputLastUpdatedTime

assetList :: Asset -> Integer -> Integer -> CumulativeRate -> CumulativeRate -> Map Asset AssetInformation
assetList asset lendAmt borrowAmt lendRate borrowRate =
  Map.singleton asset (AssetInformation (fromInteger lendAmt) (fromInteger borrowAmt) lendRate borrowRate)

poolNftValue :: Value
poolNftValue = valueFromAsset poolAuthToken 1

suppliedPoolDatum :: PoolDatum
suppliedPoolDatum = PoolDatum (assetList adaToken supplyAmount 0 baseRate baseRate) outputLastUpdatedTime

poolDatumAfterRepay :: PoolDatum
poolDatumAfterRepay =
  PoolDatum (assetList adaToken supplyAmount 1 updatedLendingRate updatedBorrowingRate) outputLastUpdatedTime

clearBorrowingPoolDatum :: PoolDatum
clearBorrowingPoolDatum =
  PoolDatum (assetList adaToken supplyAmount 0 updatedLendingRate updatedBorrowingRate) outputLastUpdatedTime

clearSupplyingPoolDatum :: PoolDatum
clearSupplyingPoolDatum =
  PoolDatum (assetList adaToken 0 0 updatedLendingRate updatedBorrowingRate) outputLastUpdatedTime

borrowedPoolDatum :: PoolDatum
borrowedPoolDatum = suppliedPoolDatum {pdAssets = assetList adaToken supplyAmount borrowAmount 1 1}

toInputDatum :: PoolDatum -> PoolDatum
toInputDatum datum = datum {pdLastUpdatedTime = inputLastUpdatedTime}

poolDatumBeforeLiquidate :: PoolDatum
poolDatumBeforeLiquidate =
  PoolDatum
    ( assetList adaToken supplyAmount 0 1 1
        <> assetList testMeldAsset supplyAmount borrowAmount 1 1
    )
    inputLastUpdatedTime

poolDatumAfterLiquidate :: PoolDatum
poolDatumAfterLiquidate =
  PoolDatum
    ( assetList adaToken (supplyAmount - collateralAmount - protocolIncentiveAmount) 0 1 1
        <> assetList testMeldAsset supplyAmount 0 updatedLendingRate updatedBorrowingRate
    )
    outputLastUpdatedTime

poolDatumWithdraw :: PoolDatum
poolDatumWithdraw = PoolDatum (assetList adaToken 2_000_000_000 1_000_000_000 1.16 1.36) inputLastUpdatedTime

accountDatumWithRequests :: [Request] -> AccountDatum
accountDatumWithRequests requests =
  AccountDatum
    mempty
    mempty
    mempty
    userNFT
    requests
    Nothing
    Nothing
    mempty
    defaultExtraLovelace

accountDatumAfterSupply :: AccountDatum
accountDatumAfterSupply =
  AccountDatum
    (Map.fromList [(adaToken, fromInteger supplyAmount)])
    mempty
    mempty
    userNFT
    []
    Nothing
    Nothing
    mempty
    defaultExtraLovelace

accountDatumAfterBorrow :: AccountDatum
accountDatumAfterBorrow =
  accountDatumAfterSupply
    { adCollateralAssets = Map.singleton adaToken True
    , adBorrowings = Map.fromList [(adaToken, fromInteger borrowAmount)]
    }

accountDatumAfterLiquidate :: AccountDatum
accountDatumAfterLiquidate =
  AccountDatum
    (Map.fromList [(adaToken, 0)])
    (Map.fromList [(testMeldAsset, 0)])
    (Map.fromList [(adaToken, True)])
    userNFT
    []
    Nothing
    Nothing
    mempty
    defaultExtraLovelace

accountDatumAfterRepay :: AccountDatum
accountDatumAfterRepay = accountDatumAfterBorrow {adBorrowings = Map.fromList [(adaToken, 1)]}

accountDatumAfterClearBorrowing :: AccountDatum
accountDatumAfterClearBorrowing =
  accountDatumAfterBorrow
    { adBorrowings = Map.fromList [(adaToken, 0)]
    , adSupplies = Map.fromList [(adaToken, fromInteger supplyAmount)]
    }

accountDatumAfterClearSupplying :: AccountDatum
accountDatumAfterClearSupplying =
  accountDatumAfterBorrow
    { adBorrowings = Map.fromList [(adaToken, 0)]
    , adSupplies = Map.fromList [(adaToken, 0)]
    }

closedBoundedInterval :: POSIXTime -> POSIXTime -> POSIXTimeRange
closedBoundedInterval from to = Interval (Interval.lowerBound from) (Interval.strictUpperBound to)

validTimeRange :: POSIXTimeRange
validTimeRange = closedBoundedInterval 1_000 61_000

poolRedeemer :: PoolRedeemer
poolRedeemer = UpdatePoolRedeemer

migratePoolRedeemer :: PoolRedeemer
migratePoolRedeemer = MigratePoolRedeemer

updateTreasuryPoolRedeemer :: PoolRedeemer
updateTreasuryPoolRedeemer = UpdateTreasuryPoolRedeemer

poolCredential :: Credential
poolCredential = ScriptCredential $ Plutarch.validatorHash (Plutarch.mkValidator configTest poolValidator)

oracleCredential :: Credential
oracleCredential = ScriptCredential $ Plutarch.validatorHash oracleValidator

accountUtxo :: AccountDatum -> Value -> UTXO
accountUtxo datum value =
  mconcat
    [ credential accountCredential
    , withInlineDatum datum
    , withValue (value <> valueFromAsset accountAuthToken 1)
    , withRefTxId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withRedeemer AccountApplyRedeemer
    ]

-- Note: Min ADA UTXO value of Account (calculated by formula) can change after apply
-- but we expect it not to
accountInOut :: AccountDatum -> AccountDatum -> SpendingBuilder
accountInOut inputDatum outputDatum =
  let ada = PlutusValue.assetClass PlutusValue.adaSymbol PlutusValue.adaToken
      getMaxValue :: Value -> Value -> Value
      getMaxValue val1 val2 =
        if PlutusValue.assetClassValueOf val1 ada >= PlutusValue.assetClassValueOf val2 ada
          then val1
          else val2
      minAdaVal = getMaxValue (calculateMinAdaValueAccount inputDatum) (calculateMinAdaValueAccount outputDatum)
      inputVal = minAdaVal <> accountValueWithoutMinAda inputDatum
      outputVal = minAdaVal <> accountValueWithoutMinAda outputDatum
   in input (accountUtxo inputDatum inputVal)
        <> output (accountUtxo outputDatum outputVal)

poolUtxo :: PoolDatum -> Value -> UTXO
poolUtxo datum additionValue =
  mconcat
    [ credential poolCredential
    , withInlineDatum datum
    , withValue (minAdaValue <> additionValue <> poolNftValue)
    , withRefTxId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withRedeemer UpdatePoolRedeemer
    ]

poolToMigrateUtxo :: PoolDatum -> Value -> UTXO
poolToMigrateUtxo datum value =
  mconcat
    [ credential poolCredential
    , withInlineDatum datum
    , withValue (value <> poolNftValue)
    , withRefTxId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withRedeemer MigratePoolRedeemer
    ]

poolUtxoToAnAddress :: Integer -> UTXO
poolUtxoToAnAddress adaAmount =
  mconcat
    [ pubKey userPkh
    , withValue (valueFromAsset adaAssetClass adaAmount <> poolNftValue)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

userUtxo :: Value -> UTXO
userUtxo value =
  mconcat
    [ pubKey userPkh
    , withValue value
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

batcherUtxo :: Integer -> UTXO
batcherUtxo adaAmt =
  mconcat
    [ pubKey batcherPkh
    , withValue (valueFromAsset adaAssetClass adaAmt)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

migrateUtxo :: UTXO
migrateUtxo = userInputUtxo $ minAdaValue <> valueFromAsset operatorMigrationNft 1

withdrawUtxo :: UTXO
withdrawUtxo = userInputUtxo $ minAdaValue <> valueFromAsset operatorPoolNft 1

poolGenesisUtxo :: UTXO
poolGenesisUtxo = poolUtxo genesisPoolDatum mempty

outputSuppliedPoolUtxo :: UTXO
outputSuppliedPoolUtxo = poolUtxo suppliedPoolDatum (valueFromAsset adaAssetClass supplyAmount)

outputAfterRepayPoolUtxo :: UTXO
outputAfterRepayPoolUtxo = poolUtxo poolDatumAfterRepay (valueFromAsset adaAssetClass supplyAmount)

outputAfterClearBorrowingPoolUtxo :: UTXO
outputAfterClearBorrowingPoolUtxo =
  poolUtxo
    clearBorrowingPoolDatum
    (valueFromAsset adaAssetClass supplyAmount)

outputAfterClearSupplyingPoolUtxo :: UTXO
outputAfterClearSupplyingPoolUtxo = poolUtxo clearSupplyingPoolDatum mempty

inputSuppliedPoolUtxo :: UTXO
inputSuppliedPoolUtxo =
  poolUtxo (toInputDatum suppliedPoolDatum) (valueFromAsset adaAssetClass supplyAmount)

inputSetCollateralPoolDatum :: PoolDatum
inputSetCollateralPoolDatum = PoolDatum (assetList adaToken supplyAmount 0 1 1) inputLastUpdatedTime

outputSetCollateralPoolDatum :: PoolDatum
outputSetCollateralPoolDatum = PoolDatum (assetList adaToken supplyAmount 0 1 1) outputLastUpdatedTime

inputPoolInSetCollateralTx :: UTXO
inputPoolInSetCollateralTx =
  poolUtxo inputSetCollateralPoolDatum (valueFromAsset adaAssetClass supplyAmount)

poolOutputAfterSupplyAndWithdraw :: UTXO
poolOutputAfterSupplyAndWithdraw =
  let datum =
        suppliedPoolDatum {pdAssets = assetList adaToken (supplyAmount - withdrawAmount) 0 1 1}
   in poolUtxo datum $ valueFromAsset adaAssetClass (supplyAmount - withdrawAmount)

outputPoolInSetCollateralTx :: UTXO
outputPoolInSetCollateralTx =
  poolUtxo outputSetCollateralPoolDatum (valueFromAsset adaAssetClass supplyAmount)

batcherPkh :: PubKeyHash
batcherPkh = PubKeyHash "db8a9a2c692ae19a6c5d4184da07293fe1c4a5b6263eac4c0a9581b1"

oracleDatum :: OracleDatum
oracleDatum = OracleDatum $ Map.fromList [(adaToken, 1_500_000), (testMeldAsset, 1_500_000)]

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

successSupplyCtx :: SpendingBuilder
successSupplyCtx =
  baseCtx
    <> input poolGenesisUtxo
    <> output outputSuppliedPoolUtxo
    <> accountInOut (accountDatumWithRequests [supplyRequest]) accountDatumAfterSupply
    <> output (batcherUtxo batcherFee)

failedSupplyCtxCapExceeded :: SpendingBuilder
failedSupplyCtxCapExceeded =
  let exceededCapSupplyAmount = 100_000_001
      exceededCapSupplyRequest = SupplyRequest adaToken $ fromInteger exceededCapSupplyAmount
      poolOutputDatum = PoolDatum (assetList adaToken exceededCapSupplyAmount 0 1 1) outputLastUpdatedTime
      poolOutput = poolUtxo poolOutputDatum (valueFromAsset adaAssetClass exceededCapSupplyAmount)
      accountOutputDatum =
        accountDatumAfterSupply {adSupplies = Map.singleton adaToken (fromInteger exceededCapSupplyAmount)}
   in baseCtx
        <> input poolGenesisUtxo
        <> output poolOutput
        <> accountInOut (accountDatumWithRequests [exceededCapSupplyRequest]) accountOutputDatum
        <> output (batcherUtxo batcherFee)

failedCtxNotIncludeAuthTokenAccount :: SpendingBuilder
failedCtxNotIncludeAuthTokenAccount =
  baseCtx
    <> input poolGenesisUtxo
    <> output outputSuppliedPoolUtxo
    <> input
      ( accountUtxo
          (accountDatumWithRequests [supplyRequest])
          $ valueFromAsset accountAuthToken (-1) <> valueFromAsset adaAssetClass supplyAmount
      )
    <> output (accountUtxo accountDatumAfterSupply $ valueFromAsset accountAuthToken (-1))

failedCtxNotIncludePoolNft :: SpendingBuilder
failedCtxNotIncludePoolNft =
  baseCtx
    <> input poolGenesisUtxo
    <> output
      ( poolUtxo suppliedPoolDatum $
          valueFromAsset adaAssetClass supplyAmount
            <> valueFromAsset poolAuthToken (-1)
      )
    <> accountInOut (accountDatumWithRequests [supplyRequest]) accountDatumAfterSupply

successCtxSupplyAndWithdraw :: SpendingBuilder
successCtxSupplyAndWithdraw =
  let accountInputDatum = accountDatumWithRequests [supplyRequest, withdrawRequest]
      accountOutputDatum =
        accountDatumAfterSupply
          { adSupplies = Map.fromList [(adaToken, fromInteger $ supplyAmount - withdrawAmount)]
          }
   in input poolGenesisUtxo
        <> output poolOutputAfterSupplyAndWithdraw
        <> accountInOut accountInputDatum accountOutputDatum
        <> output (userUtxo (valueFromAsset adaAssetClass $ minAdaUtxo + borrowAmount))
        <> output (batcherUtxo batcherFee)
        <> baseCtx

successBorrowCtx :: SpendingBuilder
successBorrowCtx =
  let accountInputDatum =
        accountDatumAfterSupply
          { adNormalRequests = [borrowRequest]
          , adCollateralAssets = Map.singleton adaToken True
          }
   in input inputSuppliedPoolUtxo
        <> output (poolUtxo borrowedPoolDatum (valueFromAsset adaAssetClass $ supplyAmount - borrowAmount))
        <> accountInOut accountInputDatum accountDatumAfterBorrow
        <> output (userUtxo (valueFromAsset adaAssetClass $ minAdaUtxo + borrowAmount))
        <> output (batcherUtxo batcherFee)
        <> baseCtx

successBorrowCtx2 :: SpendingBuilder
successBorrowCtx2 =
  let request = BorrowRequest adaToken (fromInteger borrowAmount `div` 2) userAddress
      accountInputDatum =
        accountDatumAfterSupply
          { adNormalRequests = [request, request]
          , adCollateralAssets = Map.singleton adaToken True
          }
   in input inputSuppliedPoolUtxo
        <> output (poolUtxo borrowedPoolDatum (valueFromAsset adaAssetClass $ supplyAmount - borrowAmount))
        <> accountInOut accountInputDatum accountDatumAfterBorrow
        <> output (userUtxo (valueFromAsset adaAssetClass $ minAdaUtxo + borrowAmount `div` 2))
        <> output (userUtxo (valueFromAsset adaAssetClass $ minAdaUtxo + borrowAmount `div` 2))
        <> output (batcherUtxo $ 2 * batcherFee)
        <> baseCtx

failedBorrowCtxCapExceeded :: SpendingBuilder
failedBorrowCtxCapExceeded =
  let exceededCapBorrowAmount = 600_001
      exceededCapBorrowRequest = BorrowRequest adaToken (fromInteger exceededCapBorrowAmount) userAddress
      poolOutputDatum =
        suppliedPoolDatum {pdAssets = assetList adaToken supplyAmount exceededCapBorrowAmount 1 1}
      accountInputDatum =
        accountDatumAfterSupply
          { adNormalRequests = [exceededCapBorrowRequest]
          , adCollateralAssets = Map.singleton adaToken True
          }
      accountOutputDatum =
        accountDatumAfterSupply
          { adCollateralAssets = Map.singleton adaToken True
          , adBorrowings = Map.fromList [(adaToken, fromInteger exceededCapBorrowAmount)]
          }
   in input inputSuppliedPoolUtxo
        <> output
          ( poolUtxo
              poolOutputDatum
              (valueFromAsset adaAssetClass $ supplyAmount - exceededCapBorrowAmount)
          )
        <> accountInOut accountInputDatum accountOutputDatum
        <> output (userUtxo (valueFromAsset adaAssetClass $ minAdaUtxo + exceededCapBorrowAmount))
        <> output (batcherUtxo batcherFee)
        <> baseCtx

successCtxRepay :: SpendingBuilder
successCtxRepay =
  input
    ( poolUtxo
        (toInputDatum borrowedPoolDatum)
        (valueFromAsset adaAssetClass $ supplyAmount - borrowAmount)
    )
    <> output outputAfterRepayPoolUtxo
    <> accountInOut (accountDatumAfterBorrow {adNormalRequests = [repayRequest]}) accountDatumAfterRepay
    <> output (batcherUtxo batcherFee)
    <> baseCtx

successCtxClearBorrowing :: SpendingBuilder
successCtxClearBorrowing =
  let accountInputDatum =
        accountDatumAfterBorrow
          { adClearRequests =
              Map.fromList [(adaToken, ClearBorrowing (fromInteger clearBorrowingLimited) userAddress)]
          }
   in input
        ( poolUtxo
            (toInputDatum borrowedPoolDatum)
            (valueFromAsset adaAssetClass $ supplyAmount - borrowAmount)
        )
        <> output outputAfterClearBorrowingPoolUtxo
        <> accountInOut accountInputDatum accountDatumAfterClearBorrowing
        <> output (userUtxo (valueFromAsset adaAssetClass $ clearBorrowingLimited - borrowAmount + minAdaUtxo))
        <> output (batcherUtxo batcherFee)
        <> baseCtx

successCtxClearSupplying :: SpendingBuilder
successCtxClearSupplying =
  let accountInputDatum =
        accountDatumAfterClearBorrowing
          { adClearRequests = Map.fromList [(adaToken, ClearSupplying userAddress)]
          }
   in input
        ( poolUtxo
            (toInputDatum clearBorrowingPoolDatum)
            (valueFromAsset adaAssetClass supplyAmount)
        )
        <> output outputAfterClearSupplyingPoolUtxo
        <> accountInOut accountInputDatum accountDatumAfterClearSupplying
        <> output (userUtxo (valueFromAsset adaAssetClass $ supplyAmount + minAdaUtxo))
        <> output (batcherUtxo batcherFee)
        <> baseCtx

successCtxSetCollateral :: SpendingBuilder
successCtxSetCollateral =
  let accountInputDatum = accountDatumAfterSupply {adCollateralUpdate = Just (Map.singleton adaToken True)}
      accountOuputDatum = accountDatumAfterSupply {adCollateralAssets = Map.singleton adaToken True}
   in input inputPoolInSetCollateralTx
        <> output outputPoolInSetCollateralTx
        <> accountInOut accountInputDatum accountOuputDatum
        <> baseCtx

successCtxLiquidate :: SpendingBuilder
successCtxLiquidate =
  let repayAllRequest = ClearBorrowing (fromInteger borrowAmount) userAddress
      withdrawAllRequest = ClearSupplying userAddress
      inputAccountDatum =
        AccountDatum
          (Map.fromList [(adaToken, fromInteger collateralAmount)])
          (Map.fromList [(testMeldAsset, fromInteger borrowAmount)])
          (Map.fromList [(adaToken, True)])
          userNFT
          []
          Nothing
          (Just $ Map.fromList [(adaToken, fromInteger protocolIncentiveAmount)])
          (Map.fromList [(testMeldAsset, repayAllRequest), (adaToken, withdrawAllRequest)])
          defaultExtraLovelace
   in input
        ( poolUtxo poolDatumBeforeLiquidate $
            valueFromAsset adaAssetClass supplyAmount
              <> valueFromAsset testMeldAssetClass (supplyAmount - borrowAmount)
        )
        <> output
          ( poolUtxo
              poolDatumAfterLiquidate
              ( valueFromAsset adaAssetClass (supplyAmount - collateralAmount)
                  <> valueFromAsset testMeldAssetClass supplyAmount
              )
          )
        <> accountInOut inputAccountDatum accountDatumAfterLiquidate
        <> output (userUtxo (valueFromAsset adaAssetClass $ minAdaUtxo + collateralAmount))
        <> output (userUtxo minAdaValue)
        <> baseCtx

randomPoolDatum :: PoolDatum
randomPoolDatum = genesisPoolDatum

migratePoolCtx :: SpendingBuilder
migratePoolCtx =
  input (poolToMigrateUtxo randomPoolDatum minAdaValue)
    <> input migrateUtxo
    <> output (poolUtxoToAnAddress minAdaUtxo)

actionWithTreasuryCtx :: TypeActionWithTreasury -> SpendingBuilder
actionWithTreasuryCtx typeAction =
  let lReceiptAmount = 2_000_000_000
      bReceiptAmount = 1_000_000_000
      lActualAmount = 2_320_000_000
      bActualAmount = 1_360_000_000
      curPoolAmount = lReceiptAmount - bReceiptAmount
      poolValueInput = minAdaValue <> valueFromAsset adaAssetClass curPoolAmount
      treasuryAmount = curPoolAmount + bActualAmount - lActualAmount

      poolValueOutput =
        minAdaValue
          <> valueFromAsset
            adaAssetClass
            (curPoolAmount + (if typeAction == SupplyTreasury then 20_000_000 else treasuryAmount))
   in input (poolUtxo poolDatumWithdraw poolValueInput)
        <> input withdrawUtxo
        <> output (poolUtxo poolDatumWithdraw poolValueOutput)
        <> referenceInput managerInput
        <> withSpendingOutRefId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
        <> txId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"

specs :: TestTree
specs =
  Plutarch.tryFromPTerm "Supply to Pool" poolValidator $ do
    withValidatorData genesisPoolDatum poolRedeemer successSupplyCtx
      @> "Applied Supply request successfully"
    withValidatorData genesisPoolDatum poolRedeemer failedCtxNotIncludeAuthTokenAccount
      @!> "Applying Requests failed: Missing Account auth token"
    withValidatorData genesisPoolDatum poolRedeemer failedCtxNotIncludePoolNft
      @!> "Applying Requests failed: Missing Pool NFT"
    withValidatorData genesisPoolDatum poolRedeemer successCtxSupplyAndWithdraw
      @> "Applied 1 Supply request and 1 Withdraw request successfully"
    withValidatorData (toInputDatum inputSetCollateralPoolDatum) poolRedeemer successCtxSetCollateral
      @> "Applied Set Collateral request successfully"
    withValidatorData (toInputDatum suppliedPoolDatum) poolRedeemer successBorrowCtx
      @> "Applied Borrow request successfully"
    withValidatorData (toInputDatum suppliedPoolDatum) poolRedeemer successBorrowCtx2
      @> "Applied 2 Borrow requests successfully"
    withValidatorData (toInputDatum borrowedPoolDatum) poolRedeemer successCtxRepay
      @> "Applied Repay request successfully"
    withValidatorData (toInputDatum borrowedPoolDatum) poolRedeemer successCtxClearBorrowing
      @> "Applied clear borrowing successfully"
    withValidatorData (toInputDatum clearBorrowingPoolDatum) poolRedeemer successCtxClearSupplying
      @> "Applied clear supplying successfully"
    withValidatorData poolDatumBeforeLiquidate poolRedeemer successCtxLiquidate
      @> "Applied Liquidate request successfully"
    withValidatorData randomPoolDatum migratePoolRedeemer migratePoolCtx
      @> "Can consume pool output with migrate nft"
    withValidatorData poolDatumWithdraw updateTreasuryPoolRedeemer (actionWithTreasuryCtx WithdrawTreasury)
      @> "Can consume pool output with operator nft and withdraw reward from treasury"
    withValidatorData poolDatumWithdraw updateTreasuryPoolRedeemer (actionWithTreasuryCtx SupplyTreasury)
      @> "Can consume pool output with operator nft and supply reward from treasury"
    withValidatorData genesisPoolDatum poolRedeemer failedSupplyCtxCapExceeded
      @!> "Applied Supply request failed: Supply cap exceeded"
    withValidatorData (toInputDatum suppliedPoolDatum) poolRedeemer failedBorrowCtxCapExceeded
      @!> "Applied Supply request failed: Borrow cap exceeded"
