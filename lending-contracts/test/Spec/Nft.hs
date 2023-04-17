{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Nft where

import Plutarch.Context
  ( MintingBuilder
  , UTXO
  , input
  , mint
  , txId
  , withMinting
  , withRefIndex
  , withRefTxId
  , withValue
  )
import Plutarch.Test.Precompiled ((@!>), (@>))
import Plutarch.Test.Precompiled qualified as Plutarch
import PlutusLedgerApi.V1.Value (singleton)
import Test.Tasty (TestTree)

import Common (withPolicyData)
import Sample (nftCurrencySymbol, nftPolicy)

userInput :: UTXO
userInput =
  mconcat
    [ withValue (singleton "" "" 3_000_000)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , withRefIndex 0
    ]

invalidUserInput :: UTXO
invalidUserInput = userInput <> withRefIndex 10

baseCtx :: MintingBuilder
baseCtx =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , mint (singleton nftCurrencySymbol "" 1)
    , withMinting nftCurrencySymbol
    ]

successCtx :: MintingBuilder
successCtx = baseCtx <> input userInput

invalidNftAmountCtx :: MintingBuilder
invalidNftAmountCtx = successCtx <> mint (singleton nftCurrencySymbol "" 1)

invalidTxOutRefCtx :: MintingBuilder
invalidTxOutRefCtx = baseCtx <> input invalidUserInput

specs :: TestTree
specs =
  Plutarch.tryFromPTerm "OwnerNft" nftPolicy $ do
    withPolicyData () successCtx
      @> "Mint Owner Nft"
    withPolicyData () invalidNftAmountCtx
      @!> "Mint Owner Nft fail: Invalid Nft amount"
    withPolicyData () invalidTxOutRefCtx
      @!> "Mint OnwerNft fail: Invalid TxOutRef"
