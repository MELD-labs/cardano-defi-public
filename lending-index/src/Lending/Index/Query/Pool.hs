module Lending.Index.Query.Pool (getPoolUtxo, queryPoolUtxo) where

import Control.Monad.Catch (throwM)
import Control.Monad.Extra (maybeM)
import Database.Persist qualified as Persist

import Lending.Core.Utils (extractUtxoInputWithDatum)
import Lending.Index.Exception (IndexException (IndexUtxoNotFoundException))
import Lending.Index.Pool qualified as IO
import Lending.Index.Query.Types (SqlM)
import Lending.Scripts qualified as Scripts
import Lending.Types.Pool (PoolDatum)
import TxBuilder.Api (UtxoInputWithDatum)

queryPoolUtxo :: SqlM (Maybe (Persist.Entity IO.Pool))
queryPoolUtxo =
  Persist.selectFirst
    [IO.PoolClosingSlotNo Persist.==. Nothing]
    []

getPoolUtxo :: SqlM (UtxoInputWithDatum PoolDatum)
getPoolUtxo =
  maybeM (throwM $ IndexUtxoNotFoundException Scripts.Pool) pure $
    (extractUtxoInputWithDatum IO.PoolRef IO.PoolDatum =<<) <$> queryPoolUtxo
