{-# LANGUAGE NamedFieldPuns #-}

module Lending.Api.Transactions.CloseAccount
  ( CloseAccountInput (..)
  , closeAccountConstraints
  )
where

import Cardano.Api qualified as CA

import Lending.Core.Errors (ScriptError)
import Lending.Types.Account (AccountRedeemer, AccountTokenRedeemer (Burn))
import TxBuilder.Api
  ( BuildConstraints
  , UtxoInput
  , WitnessScript (RefScript)
  , mustMintValue
  , mustSpendFromScript
  )
import TxBuilder.Api.Utils (mustReferenceInput)

data CloseAccountInput = CloseAccountInput
  { claiAccountInput :: UtxoInput
  , claiAccountRedeemer :: AccountRedeemer
  , claiAccountRefScriptUtxo :: UtxoInput
  , claiReferenceManagerInput :: UtxoInput
  , claiAccountAuthToken :: CA.AssetId
  , claiAccountAuthTokenScript :: UtxoInput
  }

-- | Build close Account UTxO constrains
closeAccountConstraints :: CloseAccountInput -> Either ScriptError BuildConstraints
closeAccountConstraints
  CloseAccountInput
    { claiAccountInput
    , claiAccountRefScriptUtxo
    , claiAccountRedeemer
    , claiReferenceManagerInput
    , claiAccountAuthToken
    , claiAccountAuthTokenScript
    } = do
    let mustSpendFromAccountConstraint =
          mustSpendFromScript
            claiAccountInput
            (RefScript CA.PlutusScriptV2 claiAccountRefScriptUtxo)
            CA.InlineScriptDatum
            claiAccountRedeemer
        mustBurnNftConstraint =
          mustMintValue claiAccountAuthToken (negate 1) (RefScript CA.PlutusScriptV2 claiAccountAuthTokenScript) Burn
    pure $
      mustSpendFromAccountConstraint
        <> mustReferenceInput claiReferenceManagerInput
        <> mustBurnNftConstraint
