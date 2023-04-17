module Lending.Services.Transactions.Types (BatchingInput (..)) where

import Cardano.Api qualified as CA
import Plutus.V2.Ledger.Api (POSIXTime)

import Lending.Types.Account (AccountDatum)
import Lending.Types.Manager (ManagerDatum)
import Lending.Types.Oracle (OracleDatum)
import Lending.Types.Pool (PoolDatum)
import TxBuilder.Api.Types (UtxoInput, UtxoInputWithDatum)

data BatchingInput = BatchingInput
  { biManagerInput :: UtxoInputWithDatum ManagerDatum
  , biOracleInput :: UtxoInputWithDatum OracleDatum
  , biPoolInput :: UtxoInputWithDatum PoolDatum
  , biAccountInputs :: [UtxoInputWithDatum AccountDatum]
  , biPoolScriptRefUtxo :: UtxoInput
  , biAccountScriptRefUtxo :: UtxoInput
  , biOracleCheckerScriptRefUtxo :: UtxoInput
  , biBatcherWalletUtxos :: [UtxoInput]
  , biBatcherCollateralUtxos :: [UtxoInput]
  , biBatcherChangeAddress :: CA.AddressInEra CA.BabbageEra
  , biValidityRange :: (CA.TxValidityLowerBound CA.BabbageEra, CA.TxValidityUpperBound CA.BabbageEra)
  , biUpdatedTime :: POSIXTime
  }
