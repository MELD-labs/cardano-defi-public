{-# LANGUAGE RecordWildCards #-}

module Lending.Oracle.Service.Transactions.UpdateOracle
  ( UpdateOracleInput (..)
  , updateOracleConstraints
  )
where

import Cardano.Api qualified as CA
import Cardano.Api.Shelley qualified as CA
import TxBuilder.Api
  ( BuildConstraints
  , WitnessScript (RefScript)
  , mustPayTo
  , mustSpendFromScript
  , toTxOutInlineDatum
  )
import TxBuilder.Api.Types (UtxoInput)

import Lending.Core.Utils (toTxSenderConstraints)
import Lending.Core.Utils qualified as Utils
import Lending.Types.Oracle (OracleDatum)

data UpdateOracleInput = UpdateOracleInput
  { uoiOracleInput :: UtxoInput
  , uoiOracleOperatorUtxos :: [UtxoInput]
  , uoiOracleScriptRefUtxo :: UtxoInput
  , uoiNewOracleDatum :: OracleDatum
  , uoiNewOracleValue :: CA.Value
  , uoiCollateralUtxos :: [UtxoInput]
  , uoiChangeAddress :: CA.AddressInEra CA.BabbageEra
  , uoiProtocolParams :: CA.ProtocolParameters
  , uoiSpendingValue :: CA.Value
  }

updateOracleConstraints :: UpdateOracleInput -> BuildConstraints
updateOracleConstraints UpdateOracleInput {..} =
  let mustSpendFromOracleScript =
        mustSpendFromScript
          uoiOracleInput
          (RefScript CA.PlutusScriptV2 uoiOracleScriptRefUtxo)
          CA.InlineScriptDatum
          ()
      mustPayToOracleScript =
        mustPayTo
          (Utils.utxoAddress uoiOracleInput)
          uoiNewOracleValue
          (toTxOutInlineDatum uoiNewOracleDatum)
      operatorConstraint = toTxSenderConstraints uoiOracleOperatorUtxos uoiCollateralUtxos uoiChangeAddress
   in mustSpendFromOracleScript <> mustPayToOracleScript <> operatorConstraint
