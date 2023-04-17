{-# LANGUAGE NamedFieldPuns #-}

module Lending.Api.Transactions.CreateAccount
  ( CreateAccountInput (..)
  , createAccountConstraints
  )
where

import Cardano.Api qualified as CA

import Lending.Core.Errors (ScriptError)
import Lending.Types.Account (AccountDatum, AccountTokenRedeemer (Mint))
import Plutarch.Extra.AssetClass (toPlutusAssetClass)
import TxBuilder.Api
  ( BuildConstraints
  , UtxoInput
  , WitnessScript (AttachedScript, RefScript)
  , mustMintValue
  , mustPayTo
  , toTxOutInlineDatum
  )

data CreateAccountInput = CreateAccountInput
  { craiAccountScriptAddress :: CA.AddressInEra CA.BabbageEra
  , craiAccountDatum :: AccountDatum
  , craiAccountValue :: CA.Value
  , craiNftAssetId :: CA.AssetId
  , craiNftScript :: CA.PlutusScript CA.PlutusScriptV2
  , craiAccountTokenAssetId :: CA.AssetId
  , craiAccountTokenScript :: UtxoInput
  }

-- | Build creating Account UTxO constrains
createAccountConstraints :: CreateAccountInput -> Either ScriptError BuildConstraints
createAccountConstraints
  CreateAccountInput
    { craiAccountScriptAddress
    , craiAccountDatum
    , craiAccountValue
    , craiNftAssetId
    , craiNftScript
    , craiAccountTokenAssetId
    , craiAccountTokenScript
    } = do
    let mustPayToAccountConstraint =
          mustPayTo
            craiAccountScriptAddress
            craiAccountValue
            (toTxOutInlineDatum craiAccountDatum)
        mustMintNftConstraints =
          mustMintValue craiNftAssetId 1 (AttachedScript CA.PlutusScriptV2 craiNftScript) ()
        mustMintAccountTokenConstraints =
          mustMintValue
            craiAccountTokenAssetId
            1
            (RefScript CA.PlutusScriptV2 craiAccountTokenScript)
            (Mint $ toPlutusAssetClass craiNftAssetId)
    pure $
      mustPayToAccountConstraint
        <> mustMintNftConstraints
        <> mustMintAccountTokenConstraints
