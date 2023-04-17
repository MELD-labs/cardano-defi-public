module Lending.Index.Query.Oracle (getOracleUtxo) where

import Control.Monad.Catch (throwM)
import Control.Monad.Extra (maybeM)
import Database.Persist qualified as Persist

import Lending.Core.Utils (extractUtxoInputWithDatum)
import Lending.Index.Exception (IndexException (IndexUtxoNotFoundException))
import Lending.Index.Oracle qualified as IO
import Lending.Index.Query.Types (SqlM)
import Lending.Scripts qualified as Scripts
import Lending.Types.Oracle (OracleDatum)
import TxBuilder.Api (UtxoInputWithDatum)

queryOracleUtxo :: SqlM (Maybe (Persist.Entity IO.Oracle))
queryOracleUtxo =
  Persist.selectFirst
    [IO.OracleClosingSlotNo Persist.==. Nothing]
    []

getOracleUtxo :: SqlM (UtxoInputWithDatum OracleDatum)
getOracleUtxo =
  maybeM (throwM $ IndexUtxoNotFoundException Scripts.Oracle) pure $
    (extractUtxoInputWithDatum IO.OracleRef IO.OracleDatum =<<) <$> queryOracleUtxo
