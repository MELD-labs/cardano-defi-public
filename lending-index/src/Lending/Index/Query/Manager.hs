module Lending.Index.Query.Manager
  ( queryManagerUtxo
  , getManagerUtxo
  )
where

import Control.Monad.Catch (throwM)
import Control.Monad.Extra (maybeM)
import Database.Persist qualified as Persist

import Lending.Core.Utils (extractUtxoInputWithDatum)
import Lending.Index.Exception (IndexException (IndexUtxoNotFoundException))
import Lending.Index.Manager qualified as IM
import Lending.Index.Query.Types (SqlM)
import Lending.Scripts qualified as Scripts
import Lending.Types (ManagerDatum)
import TxBuilder.Api (UtxoInputWithDatum)

queryManagerUtxo :: SqlM (Maybe (Persist.Entity IM.Manager))
queryManagerUtxo =
  Persist.selectFirst
    [IM.ManagerClosingSlotNo Persist.==. Nothing]
    []

getManagerUtxo :: SqlM (UtxoInputWithDatum ManagerDatum)
getManagerUtxo =
  maybeM (throwM $ IndexUtxoNotFoundException Scripts.Manager) pure $
    (extractUtxoInputWithDatum IM.ManagerRef IM.ManagerDatum =<<) <$> queryManagerUtxo
