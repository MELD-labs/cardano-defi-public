{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Index.Operator where

import Cardano.Api (InAnyCardanoEra (InAnyCardanoEra))
import Cardano.Api.Shelley qualified as CA
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy qualified as Proxy
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB))
import Database.Persist qualified as Persist
import Database.Persist.TH qualified as Persist
import GHC.Generics (Generic)

import Cardano.Index.ChainPoint.Model (ChainPoint (ChainPoint, chainPointSlotNo), Key (ChainPointKey))
import Cardano.Index.Data.Utxo (AnyUtxo, GetUtxo, UsingSingleField (UsingSingleField), toAnyUtxo)
import Cardano.Index.Handler (IndexHandler)
import Cardano.Index.Handler.Auto (IndexFilter (filterOutput, watch), IndexNoContract)
import Cardano.Index.Handler.Auto.Store (IndexStateStore)
import Cardano.Index.Handler.Auto.Types (IndexOutput (IndexOutput, ioTxOut, ioTxOutRef))
import Cardano.Index.Handler.NonContinuing (IndexNonContinuing (getNewStateNonContinuing), UsingNonContinuing)
import Cardano.Index.Utils (inAnyEra)
import Lending.Index.Orphans ()
import Lending.Scripts (LendingScript)

Persist.share
  [Persist.mkPersistWith Persist.sqlSettings [Persist.entityDef (Proxy.Proxy @ChainPoint)]]
  [Persist.persistLowerCase|
    Operator json
      slotNo CA.SlotNo
      closingSlotNo CA.SlotNo Maybe
      utxo (JSONB AnyUtxo)
      ref CA.TxIn
      scriptType LendingScript

      Primary ref scriptType
      Foreign ChainPoint OnDeleteCascade fk_produce_operator_events slotNo
      Foreign ChainPoint OnDeleteSetNull fk_consume_operator_events closingSlotNo

      deriving Generic Show Eq
|]

deriving anyclass instance GetUtxo (UsingSingleField "utxo") Operator

newtype OperatorConfig = OperatorConfig
  { ocAuthTokens :: Map LendingScript CA.AssetId
  }

hasAuthToken :: CA.TxOut ctx era -> CA.AssetId -> Bool
hasAuthToken (CA.TxOut _ txOutValue _ _) asset = CA.selectAsset (CA.txOutValueToValue txOutValue) asset > 0

instance IndexFilter OperatorConfig IndexNoContract Operator where
  watch OperatorConfig {ocAuthTokens} (CA.Tx (CA.TxBody CA.TxBodyContent {txOuts}) _) =
    or (hasAuthToken <$> txOuts <*> Map.elems ocAuthTokens)

  filterOutput OperatorConfig {ocAuthTokens} = (`inAnyEra` flip any ocAuthTokens . hasAuthToken) . ioTxOut

maybeOperatorUtxo :: ChainPoint -> IndexOutput IndexNoContract -> LendingScript -> CA.AssetId -> Maybe Operator
maybeOperatorUtxo
  ChainPoint {chainPointSlotNo}
  IndexOutput {ioTxOutRef, ioTxOut = InAnyCardanoEra _ txOut}
  scriptType
  asset =
    if hasAuthToken txOut asset
      then
        Just $
          Operator
            { operatorClosingSlotNo = Nothing
            , operatorUtxo = JSONB (toAnyUtxo txOut)
            , operatorRef = ioTxOutRef
            , operatorSlotNo = chainPointSlotNo
            , operatorScriptType = scriptType
            }
      else Nothing

instance IndexNonContinuing OperatorConfig IndexNoContract Operator where
  getNewStateNonContinuing OperatorConfig {ocAuthTokens} chainPoint indexOutput =
    Map.elems (Map.mapMaybeWithKey (maybeOperatorUtxo chainPoint indexOutput) ocAuthTokens)

deriving via
  (UsingNonContinuing Operator OperatorConfig IndexNoContract Operator)
  instance
    (IndexStateStore Operator m) => IndexHandler m Operator
