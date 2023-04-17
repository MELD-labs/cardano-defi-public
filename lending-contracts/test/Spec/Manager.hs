{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Manager where

import Data.Default (def)
import Data.Map (Map)
import Plutarch.Api.V2 (PValidator)
import Plutarch.Api.V2 qualified as Plutarch
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
import Plutarch.Test.Precompiled ((@!>), (@>))
import Plutarch.Test.Precompiled qualified as Plutarch
import PlutusLedgerApi.V2 (Credential (ScriptCredential))
import Test.Tasty (TestTree)

import Common
  ( valueFromAsset
  , withValidatorData
  )
import Lending.Contracts.Manager.OnChain (managerValidator)
import Lending.Types.Asset (Asset)
import Lending.Types.Manager
  ( GlobalRiskParameters (grpMinAdaUtxo)
  , ManagerDatum (mdAccountAuthToken, mdGlobalRiskParameters)
  , ManagerRedeemer (UpdateParamsRedeemer)
  , ManagerScriptParams (ManagerScriptParams)
  , RiskParameters
  )
import Sample
  ( adaAssetClass
  , managerAuthToken
  , managerDatum
  , operatorManagerNft
  , operatorMigrationNft
  , randomAssetClass
  )

params :: ManagerScriptParams
params = ManagerScriptParams operatorMigrationNft managerAuthToken

riskParameters :: Map Asset RiskParameters
riskParameters = mempty

redeemer :: ManagerRedeemer
redeemer = UpdateParamsRedeemer

mValidator :: ClosedTerm PValidator
mValidator = managerValidator params

-- | `ScriptCredential` used for the dummy validator.
managerCredential :: Credential
managerCredential = ScriptCredential $ Plutarch.validatorHash (Plutarch.mkValidator def mValidator)

managerUTXO :: UTXO
managerUTXO =
  mconcat
    [ credential managerCredential
    , withInlineDatum
        (managerDatum {mdGlobalRiskParameters = (mdGlobalRiskParameters managerDatum) {grpMinAdaUtxo = 2_500_000}})
    , withValue (valueFromAsset adaAssetClass 2_000_000 <> valueFromAsset managerAuthToken 1)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

managerUTXOUpdateAccountAuthToken :: UTXO
managerUTXOUpdateAccountAuthToken =
  mconcat
    [ credential managerCredential
    , withInlineDatum (managerDatum {mdAccountAuthToken = randomAssetClass})
    , withValue (valueFromAsset adaAssetClass 2_000_000 <> valueFromAsset managerAuthToken 1)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

userInputWithOperatorManagerNft :: UTXO
userInputWithOperatorManagerNft =
  mconcat
    [ withValue (valueFromAsset adaAssetClass 2_000_000 <> valueFromAsset operatorManagerNft 1)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

userInputWithMigrationOperatorNft :: UTXO
userInputWithMigrationOperatorNft =
  mconcat
    [ withValue (valueFromAsset adaAssetClass 2_000_000 <> valueFromAsset operatorMigrationNft 1)
    , withRefTxId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

successCtxWithManagerOperator :: SpendingBuilder
successCtxWithManagerOperator =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input userInputWithOperatorManagerNft
    , output managerUTXO
    , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

successCtxWithMigrationOperator :: SpendingBuilder
successCtxWithMigrationOperator =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input userInputWithMigrationOperatorNft
    , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

failCtx :: SpendingBuilder
failCtx =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input managerUTXO
    , output managerUTXO
    , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

failCtxManagerOperatorUpdate :: SpendingBuilder
failCtxManagerOperatorUpdate =
  mconcat
    [ txId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    , input userInputWithOperatorManagerNft
    , output managerUTXOUpdateAccountAuthToken
    , withSpendingOutRefId "e7c0f93f6c47e8aa23408e19eb89de6aad5c45a0582078effdf3cf4bc0faba62"
    ]

specs :: TestTree
specs =
  Plutarch.tryFromPTerm "Manager" mValidator $ do
    withValidatorData managerDatum redeemer successCtxWithManagerOperator @> "Update Manager with Manager Operator"
    withValidatorData managerDatum redeemer successCtxWithMigrationOperator @> "Update Manager with Migration Operator"
    withValidatorData managerDatum redeemer failCtx @!> "Update Manager fail"
    withValidatorData managerDatum redeemer failCtxManagerOperatorUpdate @!> "Manager Operator update fail"
