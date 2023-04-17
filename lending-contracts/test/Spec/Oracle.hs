{-# LANGUAGE OverloadedStrings #-}

module Spec.Oracle where

import Data.Default (def)
import Data.Map qualified as Map
import Plutarch.Api.V2 (PValidator)
import Plutarch.Api.V2 qualified as Plutarch
import Plutarch.Test.Precompiled ((@!>), (@>))
import Plutarch.Test.Precompiled qualified as Plutarch
import PlutusLedgerApi.V2 (Credential (ScriptCredential))
import Test.Tasty (TestTree)

import Common (valueFromAsset, withValidatorData)
import Lending.Contracts.Oracle.OnChain (oracleValidatorTerm)
import Lending.Contracts.Oracle.Types ()
import Lending.Types.Oracle (OracleDatum (OracleDatum), OracleScriptParams (OracleScriptParams))
import Plutarch.Context
  ( SpendingBuilder
  , UTXO
  , credential
  , input
  , output
  , txId
  , withInlineDatum
  , withRefTxId
  , withSpendingOutRefId
  , withValue
  )
import Sample (adaAssetClass, adaToken, operatorOracleNft, oracleAuthToken)

params :: OracleScriptParams
params = OracleScriptParams operatorOracleNft

datumInGenesis :: OracleDatum
datumInGenesis = OracleDatum mempty

datumAfterUpdate :: OracleDatum
datumAfterUpdate = OracleDatum $ Map.singleton adaToken 10

oValidator :: ClosedTerm PValidator
oValidator = oracleValidatorTerm params

-- | `ScriptCredential` used for the dummy validator.
oracleCredential :: Credential
oracleCredential = ScriptCredential $ Plutarch.validatorHash (Plutarch.mkValidator def oValidator)

oracleOutputUtxo :: UTXO
oracleOutputUtxo =
  mconcat
    [ credential oracleCredential
    , withValue (valueFromAsset adaAssetClass 2_000_000 <> valueFromAsset oracleAuthToken 1)
    , withInlineDatum datumAfterUpdate
    , withRefTxId "17c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

userInputWithOperatorOracleNft :: UTXO
userInputWithOperatorOracleNft =
  mconcat
    [ withValue (valueFromAsset adaAssetClass 2_000_000 <> valueFromAsset operatorOracleNft 1)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

userInputWithoutOperatorOracleNft :: UTXO
userInputWithoutOperatorOracleNft =
  mconcat
    [ withValue (valueFromAsset adaAssetClass 2_000_000)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

successCtx :: SpendingBuilder
successCtx =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input userInputWithOperatorOracleNft
    , output oracleOutputUtxo
    , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

failCtx :: SpendingBuilder
failCtx =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input userInputWithoutOperatorOracleNft
    , output oracleOutputUtxo
    , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

specs :: TestTree
specs =
  Plutarch.tryFromPTerm "Oracle" oValidator $ do
    withValidatorData datumInGenesis () successCtx
      @> "Update Oracle Success"
    withValidatorData datumInGenesis () failCtx
      @!> "Update Oracle Fail (not include operator oracle nft)"
