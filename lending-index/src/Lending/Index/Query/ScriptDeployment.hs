module Lending.Index.Query.ScriptDeployment (queryScriptDeployment) where

import Cardano.Api qualified as CA
import Control.Monad.Catch (throwM)
import Database.Persist qualified as Persist

import Cardano.Index.Data.RawBytesHex (RawBytesHex (RawBytesHex))
import Cardano.Index.Data.Utxo (GetUtxo (getUtxo))
import Cardano.Index.Extra.ScriptDeployment
  ( CardanoScriptDeployment
  , EntityField (CardanoScriptDeploymentClosingSlotNo, CardanoScriptDeploymentRef, CardanoScriptDeploymentRefScriptHash)
  )
import Cardano.Index.PersistLens (view)
import Lending.Index.Exception (IndexException (IndexFindScriptException))
import Lending.Index.Query.Types (SqlM)
import TxBuilder.Api (UtxoInput (UtxoInput))

-- | Extract Cardano script deployment to reference script UTxO
extractScriptDeployment :: Persist.Entity CardanoScriptDeployment -> Maybe UtxoInput
extractScriptDeployment entity =
  UtxoInput (view CardanoScriptDeploymentRef entity) . CA.toCtxUTxOTxOut <$> getUtxo entity

getScriptDeployment :: CA.ScriptHash -> SqlM (Maybe (Persist.Entity CardanoScriptDeployment))
getScriptDeployment scriptHash =
  Persist.selectFirst
    [ CardanoScriptDeploymentClosingSlotNo Persist.==. Nothing
    , CardanoScriptDeploymentRefScriptHash Persist.==. RawBytesHex scriptHash
    ]
    []

-- | Get the deployed script UTxO with a specific `ScriptHash` from the database
queryScriptDeployment :: CA.ScriptHash -> SqlM UtxoInput
queryScriptDeployment scriptHash =
  getScriptDeployment scriptHash
    >>= maybe (throwM $ IndexFindScriptException scriptHash) pure . (>>= extractScriptDeployment)
