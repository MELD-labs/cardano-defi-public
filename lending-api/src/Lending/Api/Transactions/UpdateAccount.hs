{-# LANGUAGE NamedFieldPuns #-}

module Lending.Api.Transactions.UpdateAccount
  ( UpdateAccountInput (..)
  , updateAccountConstraints
  )
where

import Cardano.Api qualified as CA

import Lending.Core.Errors (ScriptError)
import Lending.Types.Account (AccountDatum, AccountRedeemer)
import TxBuilder.Api
  ( BuildConstraints
  , UtxoInput
  , WitnessScript (RefScript)
  , mustPayTo
  , mustReferenceInput
  , mustSpendFromScript
  , toTxOutInlineDatum
  )

data UpdateAccountInput = UpdateAccountInput
  { uaiAccountInput :: UtxoInput
  , uaiAccountRedeemer :: AccountRedeemer
  , uaiAccountRefScriptUtxo :: UtxoInput
  , uaiAccountDatum :: AccountDatum
  , uaiAccountValue :: CA.Value
  , uaiAccountScriptAddress :: CA.AddressInEra CA.BabbageEra
  , uaiReferenceManagerInput :: UtxoInput
  }

-- | Build update Account UTxO constrains
updateAccountConstraints :: UpdateAccountInput -> Either ScriptError BuildConstraints
updateAccountConstraints
  UpdateAccountInput
    { uaiAccountInput
    , uaiAccountScriptAddress
    , uaiAccountDatum
    , uaiAccountValue
    , uaiAccountRefScriptUtxo
    , uaiAccountRedeemer
    , uaiReferenceManagerInput
    } = do
    let mustSpendFromAccountConstraint =
          mustSpendFromScript
            uaiAccountInput
            (RefScript CA.PlutusScriptV2 uaiAccountRefScriptUtxo)
            CA.InlineScriptDatum
            uaiAccountRedeemer
        mustPayToAccountConstraint =
          mustPayTo
            uaiAccountScriptAddress
            uaiAccountValue
            (toTxOutInlineDatum uaiAccountDatum)
    pure $
      mustSpendFromAccountConstraint
        <> mustPayToAccountConstraint
        <> mustReferenceInput uaiReferenceManagerInput
