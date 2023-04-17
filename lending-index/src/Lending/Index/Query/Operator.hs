module Lending.Index.Query.Operator (getOperatorNftUtxo) where

import Control.Monad.Catch qualified as Catch
import Database.Persist qualified as Persist

import Lending.Core.Utils (extractUtxoInput)
import Lending.Index.Exception (IndexException (IndexUtxoNotFoundException))
import Lending.Index.Operator (EntityField (OperatorClosingSlotNo, OperatorRef, OperatorScriptType))
import Lending.Index.Query.Types (SqlM)
import Lending.Scripts (LendingScript)
import TxBuilder.Api (UtxoInput)

getOperatorNftUtxo :: LendingScript -> SqlM UtxoInput
getOperatorNftUtxo scriptType =
  Persist.selectFirst
    [ OperatorScriptType Persist.==. scriptType
    , OperatorClosingSlotNo Persist.==. Nothing
    ]
    []
    >>= maybe (Catch.throwM (IndexUtxoNotFoundException scriptType)) pure . (>>= extractUtxoInput OperatorRef)
