{-# LANGUAGE NamedFieldPuns #-}

module Lending.Api.Transactions.UpdateTreasuryPool (UpdateTreasuryPoolInput (..), updateTreasuryPoolConstraints) where

import Cardano.Api qualified as CA

import Lending.Core.Errors (ScriptError)
import Lending.Types.Pool (PoolDatum, PoolRedeemer (UpdateTreasuryPoolRedeemer))
import TxBuilder.Api
  ( BuildConstraints
  , UtxoInput
  , WitnessScript (RefScript)
  , mustPayTo
  , mustSpendFromScript
  , toTxOutInlineDatum
  )
import TxBuilder.Api.Utils ()

data UpdateTreasuryPoolInput = UpdateTreasuryPoolInput
  { wpiPoolInput :: UtxoInput
  , wpiPoolScriptRefUtxo :: UtxoInput
  , wpiPoolDatum :: PoolDatum
  , wpiPoolValue :: CA.Value
  , wpiPoolScriptAddress :: CA.AddressInEra CA.BabbageEra
  }

updateTreasuryPoolConstraints :: UpdateTreasuryPoolInput -> Either ScriptError BuildConstraints
updateTreasuryPoolConstraints
  UpdateTreasuryPoolInput
    { wpiPoolInput
    , wpiPoolScriptRefUtxo
    , wpiPoolDatum
    , wpiPoolValue
    , wpiPoolScriptAddress
    } = do
    let mustSpendFromPoolScript =
          mustSpendFromScript
            wpiPoolInput
            (RefScript CA.PlutusScriptV2 wpiPoolScriptRefUtxo)
            CA.InlineScriptDatum
            UpdateTreasuryPoolRedeemer
        mustPayToPoolScript =
          mustPayTo
            wpiPoolScriptAddress
            wpiPoolValue
            (toTxOutInlineDatum wpiPoolDatum)
    pure $ mustSpendFromPoolScript <> mustPayToPoolScript
