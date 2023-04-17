{-# LANGUAGE OverloadedStrings #-}

module Spec.OracleCheckerToken where

import Plutarch.Api.V2 (PMintingPolicy)
import Plutarch.Api.V2 qualified as Plutarch
import Plutarch.Context
  ( MintingBuilder
  , input
  , mint
  , output
  , referenceInput
  , txId
  , withMinting
  )
import Plutarch.Test.Precompiled ((@>))
import Plutarch.Test.Precompiled qualified as Plutarch
import PlutusLedgerApi.V1.Value (CurrencySymbol, Value)
import Test.Tasty (TestTree)

import Common
  ( valueFromAsset
  , withPolicyData
  )
import Lending.Contracts.OracleCheckerToken.OnChain (oracleCheckerPolicyTerm)
import Sample (accountAuthToken, configTest, octRedeemer, oracleAuthToken, oracleCheckerToken)
import Spec.Account
  ( accountAfterLiquidateOutput
  , accountLiquidateInput
  , accountLiquidateOutput
  , managerInput
  , oracleInput
  , poolInput
  , userInput
  , userNftValue
  )

oracleCheckerPolicy :: ClosedTerm PMintingPolicy
oracleCheckerPolicy = oracleCheckerPolicyTerm oracleAuthToken

authTokenAccountValue :: Value
authTokenAccountValue = valueFromAsset accountAuthToken 1

oracleCheckerCurrencySymbol :: CurrencySymbol
oracleCheckerCurrencySymbol =
  Plutarch.mintingPolicySymbol (Plutarch.mkMintingPolicy configTest oracleCheckerPolicy)

baseLiquidateRequestsCtx :: MintingBuilder
baseLiquidateRequestsCtx =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input accountLiquidateInput
    , referenceInput managerInput
    , referenceInput oracleInput
    , referenceInput poolInput
    , withMinting oracleCheckerCurrencySymbol
    ]

successLiquidateCtx :: MintingBuilder
successLiquidateCtx =
  baseLiquidateRequestsCtx
    <> mint authTokenAccountValue
    <> mint userNftValue
    <> mint (valueFromAsset oracleCheckerToken 1)
    <> input userInput
    <> output accountAfterLiquidateOutput
    <> output accountLiquidateOutput

specs :: TestTree
specs =
  Plutarch.tryFromPTerm "OperatorCheckerToken" oracleCheckerPolicy $ do
    withPolicyData octRedeemer successLiquidateCtx
      @> "Mint oracle checker token"
