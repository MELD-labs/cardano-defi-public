{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Account where

import Data.Default (def)
import Data.Map qualified as Map
import Data.Map.Internal (foldMapWithKey)
import Plutarch.Api.V2 (PValidator, mkValidator, validatorHash)
import Plutarch.Api.V2 qualified as Plutarch
import Plutarch.Context
  ( SpendingBuilder
  , UTXO
  , credential
  , input
  , mint
  , mintWith
  , output
  , pubKey
  , referenceInput
  , txId
  , withInlineDatum
  , withRedeemer
  , withRefTxId
  , withSpendingOutRefId
  , withValue
  )
import Plutarch.Test.Precompiled ((@!>), (@>))
import Plutarch.Test.Precompiled qualified as Plutarch
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Value (Value)
import PlutusLedgerApi.V2
  ( Address
  , Credential (ScriptCredential)
  , PubKeyHash
  , Validator
  )
import Test.Tasty (TestTree)

import Common
  ( valueFromAsset
  , withValidatorData
  )
import Lending.Contracts.Account.OnChain (accountValidatorTerm)
import Lending.Contracts.Manager.OnChain (managerValidator)
import Lending.Contracts.Oracle.OnChain (oracleValidatorTerm)
import Lending.Contracts.Oracle.Types ()
import Lending.Contracts.Pool.OnChain (validatePoolTerm)
import Lending.Types.Account
  ( AccountDatum
      ( AccountDatum
      , adBorrowings
      , adClearRequests
      , adCollateralUpdate
      , adNormalRequests
      , adSupplies
      )
  , AccountLiquidateRedeemerData
    ( AccountLiquidateRedeemerData
    , alrBorrowings
    , alrClearRequests
    , alrCollaterals
    , alrExtraLovelace
    , alrUserNft
    )
  , AccountRedeemer (AccountCloseRedeemer, AccountLiquidateRedeemer, AccountMigrateRedeemer, AccountUpdateRedeemer)
  , ClearRequest (ClearBorrowing, ClearSupplying)
  , Request (BorrowRequest, RepayRequest, SupplyRequest, WithdrawRequest)
  )
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, Decimal)
import Lending.Types.Manager
  ( GlobalRiskParameters (grpProtocolIncentive)
  , ManagerDatum (mdGlobalRiskParameters)
  , ManagerScriptParams (ManagerScriptParams)
  , RiskParameters (rpAssetClassData)
  )
import Lending.Types.Oracle (OracleDatum (OracleDatum), OracleScriptParams (OracleScriptParams))
import Lending.Types.Pool
  ( AssetInformation (AssetInformation)
  , PoolDatum (PoolDatum)
  )
import Plutarch.Extra.FixedDecimal (FixedDecimal, convertExp, ediv, emul, fromFixedZero)
import PlutusTx.Maybe (isJust)
import Sample
  ( accountAuthToken
  , adaAssetClass
  , adaToken
  , batcherFee
  , configTest
  , datumInGenesis
  , defaultExtraLovelace
  , managerAuthToken
  , managerDatum
  , minAdaRequest
  , minAdaUtxo
  , notWhitelistedAsset
  , octRedeemer
  , operatorMigrationNft
  , operatorOracleNft
  , oracleAuthToken
  , oracleCheckerToken
  , poolAuthToken
  , riskParameters
  , testMeldAsset
  , testMeldAssetClass
  , testToken1Asset
  , testToken2Asset
  , tokenPrices
  , userNFT
  )

userPkh :: PubKeyHash
userPkh = "db8a9a2c692ae19a6c5d4184da07293fe1c4a5b6263eac4c0a9581b0"

userAddress :: Address
userAddress = pubKeyHashAddress userPkh

burnedValue :: Value
burnedValue = valueFromAsset accountAuthToken (-1)

authTokenAccountValue :: Value
authTokenAccountValue = valueFromAsset accountAuthToken 1

userNftValue :: Value
userNftValue = valueFromAsset userNFT 1

accountValidator :: ClosedTerm PValidator
accountValidator = accountValidatorTerm managerAuthToken operatorMigrationNft

datumOutForSupply :: AccountDatum
datumOutForSupply =
  AccountDatum
    mempty
    mempty
    mempty
    userNFT
    [updateSupplyRequest]
    Nothing
    Nothing
    mempty
    defaultExtraLovelace

datumOutForSetCollateral :: AccountDatum
datumOutForSetCollateral =
  AccountDatum
    mempty
    mempty
    mempty
    userNFT
    []
    (Just $ Map.singleton adaToken True)
    Nothing
    mempty
    defaultExtraLovelace

accountSetCollateralWithRequestDatum :: AccountDatum
accountSetCollateralWithRequestDatum =
  AccountDatum
    mempty
    mempty
    mempty
    userNFT
    [updateSupplyRequest]
    (Just $ Map.singleton adaToken True)
    Nothing
    mempty
    defaultExtraLovelace

supplyAmount :: Integer
supplyAmount = 1_000_000

withdrawAmount :: Integer
withdrawAmount = 500_000

borrowAmount :: Integer
borrowAmount = 500_000

collateralAmount :: Integer
collateralAmount = 550_000

decimalBorrowAmount :: Decimal
decimalBorrowAmount = fromInteger borrowAmount

decimalCollateralAmount :: FixedDecimal 0
decimalCollateralAmount = fromInteger collateralAmount

loanToValueLiquidate :: Decimal
loanToValueLiquidate = convertExp (decimalBorrowAmount `ediv` decimalCollateralAmount)

liquidatorCollateralAmount :: Integer
liquidatorCollateralAmount = 275_000

failedLiquidatorCollateralAmount :: Integer
failedLiquidatorCollateralAmount = 300_000

decimalLiquidatorCollateralAmount :: Decimal
decimalLiquidatorCollateralAmount = fromInteger liquidatorCollateralAmount

liquidatingPercent :: Decimal
liquidatingPercent = decimalLiquidatorCollateralAmount `ediv` fromInteger liquidatorBorrowAmount

losingAmount :: Integer
losingAmount = liquidatorCollateralAmount + protocolIncentiveAmount

protocolIncentivePercent :: Decimal
protocolIncentivePercent = grpProtocolIncentive $ mdGlobalRiskParameters managerDatum

protocolIncentiveAmount :: Integer
protocolIncentiveAmount =
  fromFixedZero $
    convertExp $
      decimalLiquidatorCollateralAmount `ediv` liquidatingPercent `emul` protocolIncentivePercent

liquidatorBorrowAmount :: Integer
liquidatorBorrowAmount = 250_000

-- Note: Assume that number of asset types always <= 5
calculateMinAdaValueAccount :: AccountDatum -> Value
calculateMinAdaValueAccount _ = minAdaValue

getValue :: Asset -> Actual -> Value
getValue asset amt =
  foldMap -- Assume that the asset always exist in riskParams
    ((`valueFromAsset` toInteger amt) . rpAssetClassData)
    $ Map.lookup asset riskParameters

getRequestValue :: Request -> Value
getRequestValue (SupplyRequest asset amt) = getValue asset amt <> valueFromAsset adaAssetClass batcherFee
getRequestValue (RepayRequest asset amt) = getValue asset amt <> valueFromAsset adaAssetClass batcherFee
getRequestValue _ = valueFromAsset adaAssetClass minAdaRequest

getClearRequestValue :: Asset -> ClearRequest -> Value
getClearRequestValue asset (ClearBorrowing limited _) =
  valueFromAsset adaAssetClass batcherFee
    <> getValue asset limited
    <> minAdaValue
getClearRequestValue _ (ClearSupplying _) = valueFromAsset adaAssetClass minAdaRequest

accountValueWithoutMinAda :: AccountDatum -> Value
accountValueWithoutMinAda AccountDatum {adNormalRequests, adCollateralUpdate, adClearRequests} =
  let setCollateralFee =
        if isJust adCollateralUpdate && null adNormalRequests && null adClearRequests
          then valueFromAsset adaAssetClass batcherFee
          else mempty
      requestValue = foldMap getRequestValue adNormalRequests
      clearRequestValue = foldMapWithKey getClearRequestValue adClearRequests
   in setCollateralFee <> requestValue <> clearRequestValue

accountValue :: AccountDatum -> Value
accountValue ad = accountValueWithoutMinAda ad <> calculateMinAdaValueAccount ad

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

accountLiquidateDatum :: AccountDatum
accountLiquidateDatum =
  AccountDatum
    (Map.fromList [(adaToken, fromInteger collateralAmount)])
    (Map.fromList [(testMeldAsset, fromInteger borrowAmount)])
    (Map.fromList [(adaToken, True)])
    userNFT
    [updateSupplyRequest]
    Nothing
    Nothing
    mempty
    defaultExtraLovelace

updateSupplyRequest :: Request
updateSupplyRequest = SupplyRequest adaToken $ fromInteger supplyAmount

updateWithdrawRequest :: Request
updateWithdrawRequest = WithdrawRequest adaToken (fromInteger withdrawAmount) userAddress

updateBorrowRequest :: Request
updateBorrowRequest = BorrowRequest adaToken (fromInteger borrowAmount) userAddress

redeemerCloseAccount :: AccountRedeemer
redeemerCloseAccount = AccountCloseRedeemer

liquidateRedeemerData :: AccountLiquidateRedeemerData
liquidateRedeemerData =
  AccountLiquidateRedeemerData
    { alrBorrowings = Map.fromList [(testMeldAsset, fromInteger liquidatorBorrowAmount)]
    , alrCollaterals = Map.fromList [(adaToken, fromInteger liquidatorCollateralAmount)]
    , alrClearRequests = Map.fromList [(testMeldAsset, ClearBorrowing (fromInteger borrowAmount) userAddress)]
    , alrExtraLovelace = defaultExtraLovelace
    , alrUserNft = userNFT
    }

redeemerLiquidateRequest :: AccountRedeemer
redeemerLiquidateRequest = AccountLiquidateRedeemer liquidateRedeemerData

redeemerLiquidateRequestRepayNegativeAmount :: AccountRedeemer
redeemerLiquidateRequestRepayNegativeAmount =
  AccountLiquidateRedeemer $
    liquidateRedeemerData
      { alrBorrowings = Map.fromList [(testMeldAsset, fromInteger $ negate liquidatorBorrowAmount)]
      }

redeemerLiquidateRequestTakeNegativeCollateral :: AccountRedeemer
redeemerLiquidateRequestTakeNegativeCollateral =
  AccountLiquidateRedeemer $
    liquidateRedeemerData
      { alrCollaterals = Map.fromList [(adaToken, fromInteger $ negate liquidatorCollateralAmount)]
      }

failedLiquidateRequestRedeemer :: AccountRedeemer
failedLiquidateRequestRedeemer =
  AccountLiquidateRedeemer
    liquidateRedeemerData
      { alrCollaterals = Map.fromList [(adaToken, fromInteger failedLiquidatorCollateralAmount)]
      }

redeemerMigrateAccount :: AccountRedeemer
redeemerMigrateAccount = AccountMigrateRedeemer

accountCredential :: Credential
accountCredential = ScriptCredential $ Plutarch.validatorHash (Plutarch.mkValidator configTest accountValidator)

minAdaValue :: Value
minAdaValue = valueFromAsset adaAssetClass minAdaUtxo

userInputUtxo :: Value -> UTXO
userInputUtxo value =
  mconcat
    [ pubKey userPkh
    , withValue value
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

userInput :: UTXO
userInput =
  userInputUtxo $
    minAdaValue <> valueFromAsset userNFT 1 <> valueFromAsset testMeldAssetClass borrowAmount

migrateUtxo :: UTXO
migrateUtxo = userInputUtxo $ minAdaValue <> valueFromAsset operatorMigrationNft 1

userOutputClose :: UTXO
userOutputClose =
  let value = valueFromAsset adaAssetClass (2 * minAdaUtxo) <> valueFromAsset userNFT 1
   in userInputUtxo value

userInputWithoutNft :: UTXO
userInputWithoutNft = userInputUtxo minAdaValue

accountInputUtxo :: AccountDatum -> Value -> UTXO
accountInputUtxo datum additionValue =
  let accValue = accountValue datum
   in mconcat
        [ credential accountCredential
        , withInlineDatum datum
        , withValue (accValue <> additionValue <> valueFromAsset accountAuthToken 1)
        , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
        ]

accountInput :: UTXO
accountInput = accountInputUtxo datumInGenesis mempty

accountSupplyOutput :: UTXO
accountSupplyOutput =
  accountInputUtxo datumOutForSupply mempty

accountSetCollateralOutput :: UTXO
accountSetCollateralOutput =
  accountInputUtxo datumOutForSetCollateral mempty

accountSetCollateralWithRequestOutput :: UTXO
accountSetCollateralWithRequestOutput =
  accountInputUtxo accountSetCollateralWithRequestDatum mempty

accountFailedSupplyOutput :: UTXO
accountFailedSupplyOutput =
  accountInputUtxo datumOutForSupply $ valueFromAsset accountAuthToken (-1)

accountWithdrawInput :: UTXO
accountWithdrawInput =
  accountInputUtxo accountDatumAfterSupply mempty

accountWithdrawOutput :: UTXO
accountWithdrawOutput =
  let datum =
        AccountDatum
          (Map.fromList [(adaToken, fromInteger supplyAmount)])
          mempty
          mempty
          userNFT
          [updateWithdrawRequest]
          Nothing
          Nothing
          mempty
          defaultExtraLovelace
   in accountInputUtxo datum mempty

accountOutputWithNotDeposiableAsset :: UTXO
accountOutputWithNotDeposiableAsset =
  let invalidSupplyRequest = SupplyRequest notWhitelistedAsset $ fromInteger supplyAmount
      datum =
        AccountDatum mempty mempty mempty userNFT [invalidSupplyRequest] Nothing Nothing mempty defaultExtraLovelace
   in accountInputUtxo datum mempty

accountOutputWithInvalidCollateral :: UTXO
accountOutputWithInvalidCollateral =
  let datum = accountDatumAfterSupply {adCollateralUpdate = Just (Map.singleton testMeldAsset True)}
   in accountInputUtxo datum mempty

accountSupplyAndWithdrawOutput :: UTXO
accountSupplyAndWithdrawOutput =
  let datum =
        AccountDatum
          (Map.fromList [(adaToken, fromInteger supplyAmount)])
          mempty
          mempty
          userNFT
          [updateSupplyRequest, updateWithdrawRequest]
          Nothing
          Nothing
          mempty
          defaultExtraLovelace
   in accountInputUtxo datum mempty

accountInputWithSomeRequests :: UTXO
accountInputWithSomeRequests =
  let datum =
        AccountDatum
          (Map.fromList [(adaToken, fromInteger supplyAmount)])
          mempty
          mempty
          userNFT
          [updateSupplyRequest, updateWithdrawRequest]
          Nothing
          Nothing
          mempty
          defaultExtraLovelace
   in accountInputUtxo datum mempty

accountOutputAfterDelete :: UTXO
accountOutputAfterDelete =
  let datum =
        AccountDatum
          (Map.fromList [(adaToken, fromInteger supplyAmount)])
          mempty
          mempty
          userNFT
          [updateSupplyRequest]
          Nothing
          Nothing
          mempty
          defaultExtraLovelace
   in accountInputUtxo datum mempty

accountLiquidateInput :: UTXO
accountLiquidateInput =
  accountInputUtxo accountLiquidateDatum mempty

accountAfterLiquidateDatum :: AccountDatum
accountAfterLiquidateDatum =
  AccountDatum
    (Map.fromList [(adaToken, fromInteger $ collateralAmount - losingAmount)])
    (Map.fromList [(testMeldAsset, fromInteger $ borrowAmount - liquidatorBorrowAmount)])
    (Map.fromList [(adaToken, True)])
    userNFT
    [updateSupplyRequest]
    Nothing
    Nothing
    mempty
    defaultExtraLovelace

accountAfterLiquidateOutput :: UTXO
accountAfterLiquidateOutput = accountInputUtxo accountAfterLiquidateDatum mempty

newAccountLiquidateDatum :: AccountDatum
newAccountLiquidateDatum =
  let repayRequest = ClearBorrowing (fromInteger borrowAmount) userAddress
   in AccountDatum
        (Map.fromList [(adaToken, fromInteger liquidatorCollateralAmount)])
        (Map.fromList [(testMeldAsset, fromInteger liquidatorBorrowAmount)])
        (Map.fromList [(adaToken, True)])
        userNFT
        []
        Nothing
        (Just $ Map.fromList [(adaToken, fromInteger protocolIncentiveAmount)])
        (Map.fromList [(testMeldAsset, repayRequest)])
        defaultExtraLovelace

accountLiquidateOutput :: UTXO
accountLiquidateOutput = accountInputUtxo newAccountLiquidateDatum mempty

failedAccountAfterLiquidateOutput :: UTXO
failedAccountAfterLiquidateOutput =
  let losingAmount' = failedLiquidatorCollateralAmount + protocolIncentiveAmount
      datum =
        accountAfterLiquidateDatum
          { adSupplies = Map.fromList [(adaToken, fromInteger $ collateralAmount - losingAmount')]
          }
   in accountInputUtxo datum mempty

failedAccountLiquidateOutput :: UTXO
failedAccountLiquidateOutput =
  let datum =
        newAccountLiquidateDatum
          { adSupplies = Map.fromList [(adaToken, fromInteger failedLiquidatorCollateralAmount)]
          }
   in accountInputUtxo datum mempty

params :: ManagerScriptParams
params = ManagerScriptParams operatorMigrationNft managerAuthToken

mValidator :: Validator
mValidator = mkValidator def (managerValidator params)

managerCredential :: Credential
managerCredential = ScriptCredential $ validatorHash mValidator

oracleParams :: OracleScriptParams
oracleParams = OracleScriptParams operatorOracleNft

oracleValidator :: Validator
oracleValidator = mkValidator def (oracleValidatorTerm oracleParams)

oracleCredential :: Credential
oracleCredential = ScriptCredential $ validatorHash oracleValidator

poolValidator :: Validator
poolValidator = mkValidator def (validatePoolTerm managerAuthToken operatorMigrationNft)

poolCredential :: Credential
poolCredential = ScriptCredential $ validatorHash poolValidator

managerInput :: UTXO
managerInput =
  mconcat
    [ credential managerCredential
    , withInlineDatum managerDatum
    , withValue (valueFromAsset adaAssetClass minAdaUtxo <> valueFromAsset managerAuthToken 1)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

oracleInput :: UTXO
oracleInput =
  let oracleDatum = OracleDatum tokenPrices
   in mconcat
        [ credential oracleCredential
        , withInlineDatum oracleDatum
        , withValue (valueFromAsset oracleAuthToken 1 <> valueFromAsset adaAssetClass 2_000_000)
        , withRefTxId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
        ]

poolInput :: UTXO
poolInput =
  let assetInfo = AssetInformation (fromInteger collateralAmount) (fromInteger borrowAmount) 1 1
      poolDatum =
        PoolDatum
          ( Map.fromList
              [ (adaToken, assetInfo)
              , (testMeldAsset, assetInfo)
              , (testToken1Asset, assetInfo)
              , (testToken2Asset, assetInfo)
              ]
          )
          0
   in mconcat
        [ credential oracleCredential
        , withInlineDatum poolDatum
        , withValue (valueFromAsset poolAuthToken 1 <> valueFromAsset adaAssetClass 2_000_000)
        , withRefTxId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
        ]

baseSupplyCtx :: SpendingBuilder
baseSupplyCtx =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input accountInput
    , referenceInput managerInput
    , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

baseWithdrawCtx :: SpendingBuilder
baseWithdrawCtx =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input accountWithdrawInput
    , referenceInput managerInput
    , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

baseDeleteRequestsCtx :: SpendingBuilder
baseDeleteRequestsCtx =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input accountInputWithSomeRequests
    , referenceInput managerInput
    , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

baseLiquidateRequestsCtx :: SpendingBuilder
baseLiquidateRequestsCtx =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input accountLiquidateInput
    , referenceInput managerInput
    , referenceInput oracleInput
    , referenceInput poolInput
    , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

baseRepayRequestsCtx :: SpendingBuilder
baseRepayRequestsCtx = baseLiquidateRequestsCtx

successCtxSupply :: SpendingBuilder
successCtxSupply = baseSupplyCtx <> input userInput <> output accountSupplyOutput

successCtxSetCollateralAccount :: SpendingBuilder
successCtxSetCollateralAccount = baseSupplyCtx <> input userInput <> output accountSetCollateralOutput

successCtxSetCollateralWithRequest :: SpendingBuilder
successCtxSetCollateralWithRequest = baseSupplyCtx <> input userInput <> output accountSetCollateralWithRequestOutput

failedCtxSupply :: SpendingBuilder
failedCtxSupply = baseSupplyCtx <> input userInput <> output accountFailedSupplyOutput

failedCtxSupply1 :: SpendingBuilder
failedCtxSupply1 = baseSupplyCtx <> input userInputWithoutNft <> output accountSupplyOutput

failedCtxSupply2 :: SpendingBuilder
failedCtxSupply2 = baseSupplyCtx <> input accountInput <> input userInput <> output accountSupplyOutput

successCtxWithdraw :: SpendingBuilder
successCtxWithdraw = baseWithdrawCtx <> input userInput <> output accountWithdrawOutput

failedCtxNotDeposiableAsset :: SpendingBuilder
failedCtxNotDeposiableAsset = baseSupplyCtx <> input userInput <> output accountOutputWithNotDeposiableAsset

failedCtxInvalidCollateral :: SpendingBuilder
failedCtxInvalidCollateral = baseWithdrawCtx <> input userInput <> output accountOutputWithInvalidCollateral

successCtxSupplyAndWithdraw :: SpendingBuilder
successCtxSupplyAndWithdraw = baseWithdrawCtx <> input userInput <> output accountSupplyAndWithdrawOutput

failedCtxNegativeSupplyAmount :: SpendingBuilder
failedCtxNegativeSupplyAmount =
  let request = SupplyRequest adaToken (-100_000)
      accountOutput =
        accountInput <> withInlineDatum (datumInGenesis {adNormalRequests = [request]})
   in baseSupplyCtx <> input userInput <> output accountOutput

failedCtxNegativeWithdrawAmount :: SpendingBuilder
failedCtxNegativeWithdrawAmount =
  let request = WithdrawRequest adaToken (-100_000) userAddress
      accountOutput =
        accountWithdrawInput
          <> withInlineDatum (accountDatumAfterSupply {adNormalRequests = [request]})
   in baseWithdrawCtx <> input userInput <> output accountOutput

failedCtxNegativeBorrowAmount :: SpendingBuilder
failedCtxNegativeBorrowAmount =
  let request = BorrowRequest adaToken (-100_000) userAddress
      accountOutput =
        accountWithdrawInput
          <> withInlineDatum (accountDatumAfterSupply {adNormalRequests = [request]})
   in baseWithdrawCtx <> input userInput <> output accountOutput

failedCtxNegativeRepayAmount :: SpendingBuilder
failedCtxNegativeRepayAmount =
  let request = RepayRequest testMeldAsset (-100_000)
      accountOutput =
        accountLiquidateInput
          <> withInlineDatum (accountLiquidateDatum {adNormalRequests = [request]})
   in baseRepayRequestsCtx <> input userInput <> output accountOutput

successCtxCloseAccount :: SpendingBuilder
successCtxCloseAccount = baseSupplyCtx <> mint burnedValue <> input userInput <> output userOutputClose

successCtxDeleteRequests :: SpendingBuilder
successCtxDeleteRequests = baseDeleteRequestsCtx <> input userInput <> output accountOutputAfterDelete

successCtxLiquidateRequests :: SpendingBuilder
successCtxLiquidateRequests =
  baseLiquidateRequestsCtx
    <> mint authTokenAccountValue
    <> mint userNftValue
    <> mintWith octRedeemer (valueFromAsset oracleCheckerToken 1)
    <> input userInput
    <> output accountAfterLiquidateOutput
    <> output accountLiquidateOutput

failedCtxLiquidateRequests :: SpendingBuilder
failedCtxLiquidateRequests =
  baseLiquidateRequestsCtx
    <> mint authTokenAccountValue
    <> mint userNftValue
    <> mintWith octRedeemer (valueFromAsset oracleCheckerToken 1)
    <> input userInput
    <> output failedAccountAfterLiquidateOutput
    <> output failedAccountLiquidateOutput

accountUtxoToAnAddress :: UTXO
accountUtxoToAnAddress =
  mconcat
    [ pubKey userPkh
    , withValue (minAdaValue <> valueFromAsset accountAuthToken 1)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

testLiquidateChosenAssetsAndHasProtocolIncentive :: Plutarch.TestCompiled ()
testLiquidateChosenAssetsAndHasProtocolIncentive = do
  withValidatorData inputAccountDatum redeemerLiquidateRequest' successCtxLiquidateRequests'
    @> "Liquidate account with chosen debt and collateral, enough for protocol incentive successfully"
  where
    -- Account with 2 asset supplied and 2 asset borrowed
    inputAccountDatum :: AccountDatum
    inputAccountDatum =
      AccountDatum
        (Map.fromList [(adaToken, 10_000_000), (testToken1Asset, 10_000_000)]) -- Note: fiat: 20$, max ltv: 15.25$
        (Map.fromList [(testMeldAsset, 7_000_000), (testToken2Asset, 750_000)]) -- Note: Fiat: 18$
        (Map.fromList [(adaToken, True), (testToken1Asset, True)])
        userNFT
        [updateSupplyRequest]
        Nothing
        Nothing
        mempty
        defaultExtraLovelace
    -- Note: Liquidate percent: 40%, liquidator incentive: 7.5%
    redeemerLiquidateRequest' :: AccountRedeemer
    redeemerLiquidateRequest' =
      AccountLiquidateRedeemer $
        AccountLiquidateRedeemerData
          (Map.fromList [(testMeldAsset, 4_000_000)]) -- Fiat: 6$
          (Map.fromList [(adaToken, 4_300_000)]) -- Fiat: 6.45$
          (Map.fromList [(testMeldAsset, ClearBorrowing 4_400_000 userAddress)])
          defaultExtraLovelace
          userNFT

    successCtxLiquidateRequests' :: SpendingBuilder
    successCtxLiquidateRequests' =
      let accountLiquidateInput' = accountInputUtxo inputAccountDatum mempty
          continuingAccountDatum =
            inputAccountDatum
              { adSupplies = Map.fromList [(adaToken, 5_500_000), (testToken1Asset, 10_000_000)]
              , adBorrowings = Map.fromList [(testMeldAsset, 3_000_000), (testToken2Asset, 750_000)]
              }
          newAccountDatum =
            AccountDatum
              (Map.fromList [(adaToken, 4_300_000)])
              (Map.fromList [(testMeldAsset, 4_000_000)])
              (Map.fromList [(adaToken, True), (testToken1Asset, True)])
              userNFT
              mempty
              Nothing
              (Just (Map.fromList [(adaToken, 200_000)])) -- 5% protocol incentive
              (Map.fromList [(testMeldAsset, ClearBorrowing 4_400_000 userAddress)])
              defaultExtraLovelace
       in mconcat
            [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
            , input accountLiquidateInput'
            , referenceInput managerInput
            , referenceInput oracleInput
            , referenceInput poolInput
            , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
            , mint authTokenAccountValue
            , mint userNftValue
            , mintWith octRedeemer (valueFromAsset oracleCheckerToken 1)
            , input userInput
            , output $ accountInputUtxo continuingAccountDatum mempty
            , output $ accountInputUtxo newAccountDatum mempty
            ]

testLiquidateChosenAssetsAndHasNoProtocolIncentive :: Plutarch.TestCompiled ()
testLiquidateChosenAssetsAndHasNoProtocolIncentive = do
  withValidatorData inputAccountDatum redeemerLiquidateRequest' successCtxLiquidateRequests'
    @> "Liquidate account with chosen debt and collateral, no protocol incentive successfully"
  where
    -- Account with 2 asset supplied and 2 asset borrowed
    inputAccountDatum :: AccountDatum
    inputAccountDatum =
      AccountDatum
        (Map.fromList [(adaToken, 7_000_000), (testToken1Asset, 19_000_000)]) -- Note: fiat: 20$, max ltv: 15.25$
        (Map.fromList [(testMeldAsset, 7_000_000), (testToken2Asset, 750_000)]) -- Note: Fiat: 18$
        (Map.fromList [(adaToken, True), (testToken1Asset, True)])
        userNFT
        [updateSupplyRequest]
        Nothing
        Nothing
        mempty
        defaultExtraLovelace
    -- Note: Liquidate percent: 40%, liquidator incentive: 7.5%
    redeemerLiquidateRequest' :: AccountRedeemer
    redeemerLiquidateRequest' =
      AccountLiquidateRedeemer $
        AccountLiquidateRedeemerData
          (Map.fromList [(testMeldAsset, 6_500_000)]) -- Fiat: 9.75$
          (Map.fromList [(adaToken, 7_000_000)]) -- Fiat: 10.5$
          (Map.fromList [(testMeldAsset, ClearBorrowing 7_150_000 userAddress)])
          defaultExtraLovelace
          userNFT

    successCtxLiquidateRequests' :: SpendingBuilder
    successCtxLiquidateRequests' =
      let accountLiquidateInput' = accountInputUtxo inputAccountDatum mempty
          continuingAccountDatum =
            inputAccountDatum
              { adSupplies = Map.fromList [(adaToken, 0), (testToken1Asset, 19_000_000)]
              , adBorrowings = Map.fromList [(testMeldAsset, 500_000), (testToken2Asset, 750_000)]
              }
          newAccountDatum =
            AccountDatum
              (Map.fromList [(adaToken, 7_000_000)])
              (Map.fromList [(testMeldAsset, 6_500_000)])
              (Map.fromList [(adaToken, True), (testToken1Asset, True)])
              userNFT
              mempty
              Nothing
              (Just (Map.fromList [(adaToken, 0)]))
              (Map.fromList [(testMeldAsset, ClearBorrowing 7_150_000 userAddress)])
              defaultExtraLovelace
       in mconcat
            [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
            , input accountLiquidateInput'
            , referenceInput managerInput
            , referenceInput oracleInput
            , referenceInput poolInput
            , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
            , mint authTokenAccountValue
            , mint userNftValue
            , mintWith octRedeemer (valueFromAsset oracleCheckerToken 1)
            , input userInput
            , output $ accountInputUtxo continuingAccountDatum mempty
            , output $ accountInputUtxo newAccountDatum mempty
            ]

testLiquidateAccountWithInsufficientCollateral :: Plutarch.TestCompiled ()
testLiquidateAccountWithInsufficientCollateral = do
  withValidatorData inputAccountDatum redeemerLiquidateRequest' successCtxLiquidateRequests'
    @!> "Failed to liquidate account: Insufficient collateral amount"
  where
    -- Account with 2 asset supplied and 2 asset borrowed
    inputAccountDatum :: AccountDatum
    inputAccountDatum =
      AccountDatum
        (Map.fromList [(adaToken, 7_000_000), (testToken1Asset, 9_000_000), (testToken2Asset, 500_000)])
        -- Note: fiat: 20$, max ltv: 16,1$
        (Map.fromList [(testMeldAsset, 12_000_000)]) -- Note: Fiat: 18$
        (Map.fromList [(adaToken, True), (testToken1Asset, True), (testToken2Asset, True)])
        userNFT
        [updateSupplyRequest]
        Nothing
        Nothing
        mempty
        defaultExtraLovelace
    -- Note: Liquidate percent: 40%, liquidator incentive: 7.5%
    redeemerLiquidateRequest' :: AccountRedeemer
    redeemerLiquidateRequest' =
      AccountLiquidateRedeemer $
        AccountLiquidateRedeemerData
          (Map.fromList [(testMeldAsset, 10_100_000)]) -- Fiat: 15.15$
          (Map.fromList [(adaToken, 7_000_000), (testToken2Asset, 600_000)]) -- Fiat: 16.5$
          (Map.fromList [(testMeldAsset, ClearBorrowing 7_150_000 userAddress)])
          defaultExtraLovelace
          userNFT

    successCtxLiquidateRequests' :: SpendingBuilder
    successCtxLiquidateRequests' =
      let accountLiquidateInput' = accountInputUtxo inputAccountDatum mempty
          continuingAccountDatum =
            inputAccountDatum
              { adSupplies = Map.fromList [(adaToken, 0), (testToken1Asset, 9_000_000), (testToken2Asset, 0)]
              , adBorrowings = Map.fromList [(testMeldAsset, 1_900_000)]
              }
          newAccountDatum =
            AccountDatum
              (Map.fromList [(adaToken, 7_000_000), (testToken2Asset, 600_000)])
              (Map.fromList [(testMeldAsset, 10_100_000)])
              (Map.fromList [(adaToken, True), (testToken1Asset, True), (testToken2Asset, True)])
              userNFT
              mempty
              Nothing
              (Just (Map.fromList [(adaToken, 0), (testToken2Asset, -100_000)]))
              (Map.fromList [(testMeldAsset, ClearBorrowing 11_110_000 userAddress)])
              defaultExtraLovelace
       in mconcat
            [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
            , input accountLiquidateInput'
            , referenceInput managerInput
            , referenceInput oracleInput
            , referenceInput poolInput
            , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
            , mint authTokenAccountValue
            , mint userNftValue
            , mintWith octRedeemer (valueFromAsset oracleCheckerToken 1)
            , input userInput
            , output $ accountInputUtxo continuingAccountDatum mempty
            , output $ accountInputUtxo newAccountDatum mempty
            ]

randomAccountDatum :: AccountDatum
randomAccountDatum = datumInGenesis

randomAccountInput :: UTXO
randomAccountInput = accountInputToMigrateUtxo $ minAdaValue <> valueFromAsset accountAuthToken 1

accountInputToMigrateUtxo :: Value -> UTXO
accountInputToMigrateUtxo value =
  mconcat
    [ credential accountCredential
    , withInlineDatum randomAccountDatum
    , withValue value
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withRedeemer AccountMigrateRedeemer
    ]

migrateAccountCtx :: SpendingBuilder
migrateAccountCtx =
  input randomAccountInput
    <> input migrateUtxo
    <> output accountUtxoToAnAddress

specs :: TestTree
specs =
  Plutarch.tryFromPTerm "Account" accountValidator $ do
    withValidatorData datumInGenesis AccountUpdateRedeemer successCtxSupply
      @> "Add requests supply to account success"
    withValidatorData datumInGenesis AccountUpdateRedeemer failedCtxSupply
      @!> "Add requests supply to account fail for not including auth token at account"
    withValidatorData datumInGenesis AccountUpdateRedeemer failedCtxSupply1
      @!> "Add requests supply to account fail for not consuming user nft"
    withValidatorData datumInGenesis AccountUpdateRedeemer failedCtxSupply2
      @!> "Add requests supply to account fail for consuming more than one account"
    withValidatorData datumInGenesis AccountUpdateRedeemer failedCtxNotDeposiableAsset
      @!> "Add supply requests fail: Invalid supply asset"
    withValidatorData accountDatumAfterSupply AccountUpdateRedeemer successCtxWithdraw
      @> "Add requests withdraw to account success"
    withValidatorData accountDatumAfterSupply AccountUpdateRedeemer failedCtxNegativeSupplyAmount
      @!> "Failed to update account: Negative supply amount"
    withValidatorData accountDatumAfterSupply AccountUpdateRedeemer failedCtxNegativeWithdrawAmount
      @!> "Failed to update account: Negative withdraw amount"
    withValidatorData accountDatumAfterSupply AccountUpdateRedeemer failedCtxNegativeBorrowAmount
      @!> "Failed to update account: Negative borrow amount"
    withValidatorData accountDatumAfterSupply AccountUpdateRedeemer failedCtxNegativeRepayAmount
      @!> "Failed to update account: Negative repay amount"
    withValidatorData accountDatumAfterSupply AccountUpdateRedeemer successCtxSupplyAndWithdraw
      @> "Add requests supply and withdraw to account success"
    withValidatorData datumInGenesis redeemerCloseAccount successCtxCloseAccount
      @> "Close account success"
    withValidatorData accountDatumAfterSupply AccountUpdateRedeemer successCtxDeleteRequests
      @> "Delete requests successfully"
    withValidatorData accountLiquidateDatum redeemerLiquidateRequest successCtxLiquidateRequests
      @> "Liquidate requests successfully"
    withValidatorData accountLiquidateDatum failedLiquidateRequestRedeemer failedCtxLiquidateRequests
      @!> "Failed to liquidate request: Exceed the maximum amount of collateral that can be taken."
    testLiquidateChosenAssetsAndHasProtocolIncentive
    testLiquidateChosenAssetsAndHasNoProtocolIncentive
    testLiquidateAccountWithInsufficientCollateral
    withValidatorData accountLiquidateDatum redeemerLiquidateRequestRepayNegativeAmount successCtxLiquidateRequests
      @!> "Failed to liquidate account: Repay negative amount"
    withValidatorData accountLiquidateDatum redeemerLiquidateRequestTakeNegativeCollateral successCtxLiquidateRequests
      @!> "Failed to liquidate account: Take negative collateral amount"
    withValidatorData randomAccountDatum redeemerMigrateAccount migrateAccountCtx
      @> "Can consume account output with migrate nft"
    withValidatorData datumInGenesis AccountUpdateRedeemer successCtxSetCollateralAccount
      @> "Add set collateral to account success"
    withValidatorData datumInGenesis AccountUpdateRedeemer successCtxSetCollateralWithRequest
      @> "Add set collateral with request to account success"
