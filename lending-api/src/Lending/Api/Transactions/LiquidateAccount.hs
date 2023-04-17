{-# LANGUAGE NamedFieldPuns #-}

module Lending.Api.Transactions.LiquidateAccount
  ( LiquidateAccountInput (..)
  , liquidateAccountConstraints
  )
where

import Cardano.Api qualified as CA

import Lending.Core.Errors (ScriptError)
import Lending.Types.Account (AccountDatum, AccountRedeemer, AccountTokenRedeemer (Mint))
import Plutarch.Extra.AssetClass (toPlutusAssetClass)
import TxBuilder.Api
  ( BuildConstraints
  , UtxoInput
  , WitnessScript (AttachedScript, RefScript)
  , mustMintValue
  , mustPayTo
  , mustSpendFromScript
  , toTxOutInlineDatum
  )

data LiquidateAccountInput = LiquidateAccountInput
  { laiAccountInput :: UtxoInput
  , laiAccountRefScriptUtxo :: UtxoInput
  , laiAccountRedeemer :: AccountRedeemer
  , laiAccountScriptAddress :: CA.AddressInEra CA.BabbageEra
  , laiLiquidatedAccountDatum :: AccountDatum
  , laiLiquidatedAccountValue :: CA.Value
  , laiNewAccountDatum :: AccountDatum
  , laiNewAccountValue :: CA.Value
  , laiNftAssetId :: CA.AssetId
  , laiNftScript :: CA.PlutusScript CA.PlutusScriptV2
  , laiAccountTokenAssetId :: CA.AssetId
  , laiAccountTokenScript :: UtxoInput
  }

liquidateAccountConstraints :: LiquidateAccountInput -> Either ScriptError BuildConstraints
liquidateAccountConstraints
  LiquidateAccountInput
    { laiAccountInput
    , laiAccountRefScriptUtxo
    , laiAccountRedeemer
    , laiAccountScriptAddress
    , laiLiquidatedAccountDatum
    , laiLiquidatedAccountValue
    , laiNewAccountDatum
    , laiNewAccountValue
    , laiNftAssetId
    , laiNftScript
    , laiAccountTokenAssetId
    , laiAccountTokenScript
    } = do
    let mustSpendFromAccountConstraint =
          mustSpendFromScript
            laiAccountInput
            (RefScript CA.PlutusScriptV2 laiAccountRefScriptUtxo)
            CA.InlineScriptDatum
            laiAccountRedeemer
        mustPayToAccountConstraint =
          mustPayTo
            laiAccountScriptAddress
            laiLiquidatedAccountValue
            (toTxOutInlineDatum laiLiquidatedAccountDatum)
            <> mustPayTo
              laiAccountScriptAddress
              laiNewAccountValue
              (toTxOutInlineDatum laiNewAccountDatum)
        mustMintNftConstraints =
          mustMintValue laiNftAssetId 1 (AttachedScript CA.PlutusScriptV2 laiNftScript) ()
        mustMintAccountTokenConstraints =
          mustMintValue
            laiAccountTokenAssetId
            1
            (RefScript CA.PlutusScriptV2 laiAccountTokenScript)
            (Mint $ toPlutusAssetClass laiNftAssetId)
    pure $
      mustSpendFromAccountConstraint
        <> mustPayToAccountConstraint
        <> mustMintNftConstraints
        <> mustMintAccountTokenConstraints
