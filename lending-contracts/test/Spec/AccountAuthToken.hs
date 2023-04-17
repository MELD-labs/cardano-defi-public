{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.AccountAuthToken where

import Plutarch.Context
  ( MintingBuilder
  , UTXO
  , address
  , input
  , mint
  , output
  , referenceInput
  , txId
  , withInlineDatum
  , withMinting
  , withRefIndex
  , withRefTxId
  , withValue
  )
import Plutarch.Extra.AssetClass (AssetClass (AssetClass))
import Plutarch.Test.Precompiled ((@!>), (@>))
import Plutarch.Test.Precompiled qualified as Plutarch
import PlutusLedgerApi.V1.Value (Value, singleton)
import PlutusLedgerApi.V2
  ( Address (Address)
  , Credential (ScriptCredential)
  )
import Test.Tasty (TestTree)

import Common
  ( valueFromAsset
  , withPolicyData
  )
import Lending.Contracts.AccountAuthToken.OnChain (accountAuthTokenName)
import Lending.Types.Account
  ( AccountDatum (AccountDatum, adNormalRequests)
  , AccountTokenRedeemer (Burn, Mint)
  , Request (BorrowRequest, RepayRequest, SupplyRequest, WithdrawRequest)
  )
import Lending.Types.Exchange (Actual)
import Sample
  ( accountAddress
  , accountCurrencySymbol
  , accountPolicy
  , adaAssetClass
  , adaToken
  , batcherFee
  , datumInGenesis
  , defaultExtraLovelace
  , minAdaUtxo
  , mockAddress
  , nftCurrencySymbol
  )
import Spec.Account (accountInputUtxo, managerInput)

invalidAccountAddress :: Address
invalidAccountAddress = Address (ScriptCredential "2ee326ac84561db79a47ef1949df0df1005dec3ea7c3acc73b2ab92e") Nothing

accountDatum :: AccountDatum
accountDatum =
  AccountDatum
    mempty
    mempty
    mempty
    (AssetClass nftCurrencySymbol "")
    []
    Nothing
    Nothing
    mempty
    defaultExtraLovelace

supplyAmount :: Actual
supplyAmount = 10_000_000

accountDatumWithSupplyRequest :: AccountDatum
accountDatumWithSupplyRequest =
  let requests = [SupplyRequest adaToken supplyAmount]
   in accountDatum {adNormalRequests = requests}

mintAuthTokenRedeemer :: AccountTokenRedeemer
mintAuthTokenRedeemer = Mint (AssetClass nftCurrencySymbol "")

burnAuthTokenRedeemer :: AccountTokenRedeemer
burnAuthTokenRedeemer = Burn

userInput :: UTXO
userInput =
  mconcat
    [ withValue (singleton "" "" 4_000_000)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withRefIndex 0
    ]

userInputCloseAccount :: UTXO
userInputCloseAccount =
  mconcat
    [ withValue (singleton "" "" 4_000_000 <> singleton nftCurrencySymbol "" 1)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withRefIndex 0
    ]

userOutputCloseAccount :: UTXO
userOutputCloseAccount =
  mconcat
    [ withValue (singleton "" "" 6_000_000 <> singleton nftCurrencySymbol "" 1)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withRefIndex 0
    ]

accountOutput :: UTXO
accountOutput =
  mconcat
    [ address accountAddress
    , withValue (singleton "" "" 2_000_000)
    , withValue (singleton accountCurrencySymbol accountAuthTokenName 1)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withRefIndex 1
    , withInlineDatum accountDatum
    ]

accountOutputWithRequest :: UTXO
accountOutputWithRequest =
  mconcat
    [ address accountAddress
    , withValue (singleton "" "" (2_000_000 + toInteger supplyAmount + batcherFee))
    , withValue (singleton accountCurrencySymbol accountAuthTokenName 1)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withRefIndex 1
    , withInlineDatum accountDatumWithSupplyRequest
    ]

accountOutputWithNegativeSupplyAmount :: UTXO
accountOutputWithNegativeSupplyAmount =
  let requests = [SupplyRequest adaToken supplyAmount, SupplyRequest adaToken (-100_000)]
   in accountOutput
        <> withValue (singleton "" "" 9_900_000)
        <> withInlineDatum accountDatum {adNormalRequests = requests}

accountOutputWithNegativeWithdrawAmount :: UTXO
accountOutputWithNegativeWithdrawAmount =
  let requests =
        [ SupplyRequest adaToken supplyAmount
        , WithdrawRequest adaToken 100_000 mockAddress
        , WithdrawRequest adaToken (-100_000) mockAddress
        ]
   in accountOutput
        <> withValue (singleton "" "" 10_000_000)
        <> withInlineDatum accountDatum {adNormalRequests = requests}

accountOutputWithNegativeBorrowAmount :: UTXO
accountOutputWithNegativeBorrowAmount =
  let requests =
        [ SupplyRequest adaToken supplyAmount
        , BorrowRequest adaToken 100_000 mockAddress
        , BorrowRequest adaToken (-50_000) mockAddress
        ]
   in accountOutput
        <> withValue (singleton "" "" 10_000_000)
        <> withInlineDatum accountDatum {adNormalRequests = requests}

accountOutputWithNegativeRepayAmount :: UTXO
accountOutputWithNegativeRepayAmount =
  let requests =
        [ SupplyRequest adaToken supplyAmount
        , BorrowRequest adaToken 100_000 mockAddress
        , RepayRequest adaToken (-100_000)
        ]
   in accountOutput
        <> withValue (singleton "" "" 10_000_000)
        <> withInlineDatum accountDatum {adNormalRequests = requests}

mintedValue :: Value
mintedValue = singleton accountCurrencySymbol accountAuthTokenName 1 <> singleton nftCurrencySymbol "" 1

burnedValue :: Value
burnedValue = singleton accountCurrencySymbol accountAuthTokenName (-1)

baseCtx :: MintingBuilder
baseCtx =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input userInput
    , referenceInput managerInput
    , mint mintedValue
    , withMinting accountCurrencySymbol
    ]

minAdaValue :: Value
minAdaValue = valueFromAsset adaAssetClass minAdaUtxo

accountInput :: UTXO
accountInput = accountInputUtxo datumInGenesis mempty

baseBurnAccountNft :: MintingBuilder
baseBurnAccountNft =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withMinting accountCurrencySymbol
    , input accountInput
    , referenceInput managerInput
    ]

successCtx :: MintingBuilder
successCtx = baseCtx <> output accountOutput

successCtxWithRequest :: MintingBuilder
successCtxWithRequest = baseCtx <> output accountOutputWithRequest <> referenceInput managerInput

wrongCtxWithRequest :: MintingBuilder
wrongCtxWithRequest =
  let accountOutputWithWrongValue = accountOutput <> withValue (singleton "" "" (negate batcherFee))
   in baseCtx <> output accountOutputWithWrongValue <> referenceInput managerInput

successCtxCloseAccount :: MintingBuilder
successCtxCloseAccount =
  baseBurnAccountNft <> mint burnedValue <> input userInputCloseAccount <> output userOutputCloseAccount

notMintNftCtx :: MintingBuilder
notMintNftCtx = successCtx <> mint (singleton nftCurrencySymbol "" (-1))

invalidAuthTokenAmountCtx :: MintingBuilder
invalidAuthTokenAmountCtx = successCtx <> mint (singleton accountCurrencySymbol accountAuthTokenName 1)

notSendAuthTokenToAccountCtx :: MintingBuilder
notSendAuthTokenToAccountCtx = baseCtx <> output (accountOutput <> address invalidAccountAddress)

createAccountWithSupplyNegativeAmountCtx :: MintingBuilder
createAccountWithSupplyNegativeAmountCtx = baseCtx <> output accountOutputWithNegativeWithdrawAmount

createAccountWithWithdrawNegativeAmountCtx :: MintingBuilder
createAccountWithWithdrawNegativeAmountCtx = baseCtx <> output accountOutputWithNegativeWithdrawAmount

createAccountWithBorrowNegativeAmountCtx :: MintingBuilder
createAccountWithBorrowNegativeAmountCtx = baseCtx <> output accountOutputWithNegativeBorrowAmount

createAccountWithRepayNegativeAmountCtx :: MintingBuilder
createAccountWithRepayNegativeAmountCtx = baseCtx <> output accountOutputWithNegativeRepayAmount

specs :: TestTree
specs =
  Plutarch.tryFromPTerm "Account AuthToken" accountPolicy $ do
    withPolicyData mintAuthTokenRedeemer successCtx
      @> "Mint authentic token"
    withPolicyData mintAuthTokenRedeemer successCtxWithRequest
      @> "Create an Account contains a supply request"
    withPolicyData mintAuthTokenRedeemer wrongCtxWithRequest
      @!> "Create an Account contains a supply request fail: Value does not match"
    withPolicyData mintAuthTokenRedeemer notMintNftCtx
      @!> "Mint authentic token fail: Not mint Owner Nft"
    withPolicyData mintAuthTokenRedeemer invalidAuthTokenAmountCtx
      @!> "Mint authentic token fail: Not send"
    withPolicyData mintAuthTokenRedeemer notSendAuthTokenToAccountCtx
      @!> "Mint authentic token fail: Not send auth token to Account script"
    withPolicyData mintAuthTokenRedeemer createAccountWithSupplyNegativeAmountCtx
      @!> "Mint authentic token fail: Negative Supply amount"
    withPolicyData mintAuthTokenRedeemer createAccountWithWithdrawNegativeAmountCtx
      @!> "Mint authentic token fail: Negative Withdraw amount"
    withPolicyData mintAuthTokenRedeemer createAccountWithBorrowNegativeAmountCtx
      @!> "Mint authentic token fail: Negative Withdraw amount"
    withPolicyData mintAuthTokenRedeemer createAccountWithRepayNegativeAmountCtx
      @!> "Mint authentic token fail: Negative Withdraw amount"
    withPolicyData burnAuthTokenRedeemer baseCtx
      @!> "Mint authentic token fail: Invalid redeemer"
    withPolicyData burnAuthTokenRedeemer successCtxCloseAccount
      @> "Burn authentic token"
