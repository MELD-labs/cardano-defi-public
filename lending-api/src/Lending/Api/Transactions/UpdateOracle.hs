{-# LANGUAGE NamedFieldPuns #-}

module Lending.Api.Transactions.UpdateOracle (UpdateOracleInput (..), updateOracleConstraints) where

import Cardano.Api qualified as CA

import Lending.Core.Errors (ScriptError)
import Lending.Types.Oracle (OracleDatum)
import TxBuilder.Api
  ( BuildConstraints
  , UtxoInput
  , WitnessScript (RefScript)
  , mustPayTo
  , mustSpendFromScript
  , mustSpendFromWallet
  , toTxOutInlineDatum
  )
import TxBuilder.Api.Utils ()

data UpdateOracleInput = UpdateOracleInput
  { uoiOracleInput :: UtxoInput
  , uoiOracleOperatorInput :: UtxoInput
  , uoiOracleScriptRefUtxo :: UtxoInput
  , uoiOracleDatum :: OracleDatum
  , uoiOracleValue :: CA.Value
  , uoiOracleScriptAddress :: CA.AddressInEra CA.BabbageEra
  }

updateOracleConstraints :: UpdateOracleInput -> Either ScriptError BuildConstraints
updateOracleConstraints
  UpdateOracleInput
    { uoiOracleInput
    , uoiOracleOperatorInput
    , uoiOracleScriptRefUtxo
    , uoiOracleDatum
    , uoiOracleValue
    , uoiOracleScriptAddress
    } = do
    let mustSpendFromOperatorNft = mustSpendFromWallet uoiOracleOperatorInput
        mustSpendFromOracleScript =
          mustSpendFromScript
            uoiOracleInput
            (RefScript CA.PlutusScriptV2 uoiOracleScriptRefUtxo)
            CA.InlineScriptDatum
            ()
        mustPayToOracleScript =
          mustPayTo
            uoiOracleScriptAddress
            uoiOracleValue
            (toTxOutInlineDatum uoiOracleDatum)
    pure $ mustSpendFromOperatorNft <> mustSpendFromOracleScript <> mustPayToOracleScript
