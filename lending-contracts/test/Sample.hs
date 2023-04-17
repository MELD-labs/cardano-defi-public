{-# LANGUAGE OverloadedStrings #-}

module Sample where

import Data.Map qualified as Map
import Data.Tagged (Tagged (Tagged))
import Plutarch.Api.V2 (PMintingPolicy)
import Plutarch.Api.V2 qualified as Plutarch
import Plutarch.Extra.AssetClass (AssetClass (AssetClass))
import Plutarch.Internal (Config (Config), TracingMode (DoTracing))
import PlutusLedgerApi.V2
  ( Address (Address)
  , Credential (PubKeyCredential, ScriptCredential)
  , CurrencySymbol
  , POSIXTime (POSIXTime)
  , TxId
  , TxOutRef (TxOutRef)
  )

import Lending.Contracts.AccountAuthToken.OnChain (accountAuthTokenName, accountPolicyTerm)
import Lending.Contracts.Nft (nftPolicyTerm)
import Lending.Types.Account (AccountDatum (AccountDatum))
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (CumulativeRate, Decimal, Fiat, Price)
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
  , ManagerDatum
    ( ManagerDatum
    , mdAccountAddress
    , mdAccountAuthToken
    , mdGlobalRiskParameters
    , mdMaxValidityDuration
    , mdOracleCheckerToken
    , mdPoolNft
    , mdRiskParameters
    , mdRiskParamsOperatorNft
    , mdTreasuryOperatorNft
    )
  , RiskParameters (RiskParameters)
  )
import Lending.Types.OracleCheckerToken (OracleCheckerTokenRedeemer (OracleCheckerTokenRedeemer))
import Lending.Types.Percent (percent)

-- Test asset tokens
adaToken :: Asset
adaToken = 0

adaAssetClass :: AssetClass
adaAssetClass = AssetClass "" ""

testMeldAsset :: Asset
testMeldAsset = 1

testMeldAssetClass :: AssetClass
testMeldAssetClass = AssetClass "e92f13e647afa0691006fb98833b60b61e6eb88d6180e7537bdb94a6" "MELD"

testToken1Asset :: Asset
testToken1Asset = 2

testToken1AssetClass :: AssetClass
testToken1AssetClass = AssetClass "e92f13e647afa0691006fb98833b60b61e6eb88d6180e7537bdb94a6" "TEST1"

testToken2Asset :: Asset
testToken2Asset = 3

testToken2AssetClass :: AssetClass
testToken2AssetClass = AssetClass "e92f13e647afa0691006fb98833b60b61e6eb88d6180e7537bdb94a6" "TEST2"

notWhitelistedAsset :: Asset
notWhitelistedAsset = 42

notWhitelistedAssetClass :: AssetClass
notWhitelistedAssetClass = AssetClass "e92f13e647afa0691006fb98833b60b61e6eb88d6180e7537bdb94a6" "INVALID"

randomAssetClass :: AssetClass
randomAssetClass = AssetClass "2ce391f183a27def3c20103b5df8b6380dee9c78d3438e0129dbfb71" "random-token"

-- Authentic tokens and NFTs
operatorManagerNft :: AssetClass
operatorManagerNft = AssetClass "db7364cc94a592cb9f8b97fc114124e372edbac9077af988e61b7158" "operator-manager-nft"

operatorOracleNft :: AssetClass
operatorOracleNft = AssetClass "db7364cc94a592cb9f8b97fc114124e372edbac9077af988e61b7158" "operator-oracle-nft"

operatorPoolNft :: AssetClass
operatorPoolNft = AssetClass "db7364cc94a592cb9f8b97fc114124e372edbac9077af988e61b7158" "operator-pool-nft"

operatorMigrationNft :: AssetClass
operatorMigrationNft = AssetClass "2ce391f183a27def3c20103b5df8b6380dee9c78d3438e0129dbfb71" "operator-migration-nft"

managerAuthToken :: AssetClass
managerAuthToken = AssetClass "a5bb0e5bb275a573d744a021f9b3bff73595468e002755b447e01559" "manager-auth-token"

--- User NFT
txOutRef :: TxOutRef
txOutRef = TxOutRef "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62" 0

nftPolicy :: ClosedTerm PMintingPolicy
nftPolicy = nftPolicyTerm txOutRef

nftCurrencySymbol :: CurrencySymbol
nftCurrencySymbol = Plutarch.mintingPolicySymbol (Plutarch.mkMintingPolicy configTest nftPolicy)

userNFT :: AssetClass
userNFT = AssetClass nftCurrencySymbol ""

--- Account
accountPolicy :: ClosedTerm PMintingPolicy
accountPolicy = accountPolicyTerm managerAuthToken operatorMigrationNft

accountCurrencySymbol :: CurrencySymbol
accountCurrencySymbol = Plutarch.mintingPolicySymbol (Plutarch.mkMintingPolicy configTest accountPolicy)

accountAuthToken :: AssetClass
accountAuthToken = AssetClass accountCurrencySymbol accountAuthTokenName

poolAuthToken :: AssetClass
poolAuthToken = AssetClass "db7364cc94a592cb9f8b97fc114124e372edbac9077af988e61b7158" "pool-auth-token"

oracleAuthToken :: AssetClass
oracleAuthToken = AssetClass "a5bb0e5bb275a573d744a021f9b3bff73595468e002755b447e01551" "oracle-auth-token"

oracleCheckerToken :: AssetClass
oracleCheckerToken = AssetClass "122c6dc9862aa0072c901a68ad9515aff0d0186a8e3b9d080503e2d9" "oracle-checker-token"

-- accountAddress
accountAddress :: Address
accountAddress = Address (ScriptCredential "df1005dec3ea7c3acc73b2ab92e2ee326ac84561db79a47ef1949df0") Nothing

-- A sample TxId for testing
sampleTxId :: TxId
sampleTxId = "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"

-- Protocol parameters
batcherFee :: Integer
batcherFee = 500_000

minAdaUtxo :: Integer
minAdaUtxo = 2_000_000

minAdaRequest :: Integer
minAdaRequest = minAdaUtxo + batcherFee

maxValidityDuration :: POSIXTime
maxValidityDuration = POSIXTime 60_000 -- 60 seconds = 60000 ms

closeFactor :: Decimal
closeFactor = percent 50

maxLiquidationCloseFactor :: Decimal
maxLiquidationCloseFactor = percent 100

minSafeHFCollateral :: Fiat
minSafeHFCollateral = 150_000_000

liquidatorIncentive :: Decimal
liquidatorIncentive = percent 10

protocolIncentive :: Decimal
protocolIncentive = percent 5

closeFactorHFThreshold :: Decimal
closeFactorHFThreshold = percent 95

riskParameters :: Map.Map Asset RiskParameters
riskParameters =
  Map.fromList
    [
      ( adaToken
      , RiskParameters
          (Tagged (percent 75))
          (Tagged (percent 85))
          600_000
          100_000_000
          adaAssetClass
          (percent 5)
          (percent 45)
          0
          (percent 4)
          (percent 300)
      )
    ,
      ( testMeldAsset
      , RiskParameters
          (Tagged (percent 0))
          (Tagged (percent 85))
          100_000_000
          100_000_000
          testMeldAssetClass
          (percent 5)
          (percent 45)
          0
          (percent 4)
          (percent 300)
      )
    ,
      ( testToken1Asset
      , RiskParameters
          (Tagged (percent 50))
          (Tagged (percent 65))
          100_000_000
          100_000_000
          testToken1AssetClass
          (percent 5)
          (percent 45)
          0
          (percent 4)
          (percent 300)
      )
    ,
      ( testToken2Asset
      , RiskParameters
          (Tagged (percent 0))
          (Tagged (percent 85))
          100_000_000
          100_000_000
          testToken2AssetClass
          (percent 5)
          (percent 45)
          0
          (percent 4)
          (percent 300)
      )
    ]

-- Test prices for tokens
tokenPrices :: Map.Map Asset Price
tokenPrices = Map.fromList [(adaToken, 1.5), (testMeldAsset, 1.5), (testToken1Asset, 0.5), (testToken2Asset, 10)]

octRedeemer :: OracleCheckerTokenRedeemer
octRedeemer = OracleCheckerTokenRedeemer tokenPrices

-- Rates
baseRate :: CumulativeRate
baseRate = 1

updatedLendingRate :: CumulativeRate
updatedLendingRate = 1.000000004710345463

updatedBorrowingRate :: CumulativeRate
updatedBorrowingRate = 1.000000009916516765

defaultExtraLovelace :: Integer
defaultExtraLovelace = 2_000_000

globalRiskParameters :: GlobalRiskParameters
globalRiskParameters =
  GlobalRiskParameters
    { grpCloseFactor = closeFactor
    , grpMaxLiquidationCloseFactor = maxLiquidationCloseFactor
    , grpCloseFactorHealthFactorThreshold = closeFactorHFThreshold
    , grpCloseFactorMinCollateralThreshold = minSafeHFCollateral
    , grpLiquidatorIncentive = liquidatorIncentive
    , grpProtocolIncentive = protocolIncentive
    , grpMinAdaUtxo = minAdaUtxo
    , grpBatcherFee = batcherFee
    }

managerDatum :: ManagerDatum
managerDatum =
  ManagerDatum
    { mdAccountAuthToken = accountAuthToken
    , mdRiskParameters = riskParameters
    , mdPoolNft = poolAuthToken
    , mdTreasuryOperatorNft = operatorPoolNft
    , mdRiskParamsOperatorNft = operatorManagerNft
    , mdAccountAddress = accountAddress
    , mdMaxValidityDuration = maxValidityDuration
    , mdOracleCheckerToken = oracleCheckerToken
    , mdGlobalRiskParameters = globalRiskParameters
    }

configTest :: Config
configTest = Config DoTracing

-- Account general datas
datumInGenesis :: AccountDatum
datumInGenesis = AccountDatum mempty mempty mempty userNFT [] Nothing Nothing mempty defaultExtraLovelace

mockAddress :: Address
mockAddress = Address (PubKeyCredential "0185545935760c5e370d01e6f4fedbb89b7fd79e115f2837cfab9ea8") Nothing
