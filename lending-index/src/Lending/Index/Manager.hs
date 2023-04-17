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

-- | Manager model contains Manager's UTxO information
module Lending.Index.Manager where

import Cardano.Api qualified as CA
import Data.Proxy qualified as Proxy
import Data.Void (Void)
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB))
import Database.Persist qualified as Persist
import Database.Persist.TH qualified as Persist
import GHC.Generics (Generic)
import Plutus.Script.Utils.Typed qualified as Scripts

import Cardano.Index.ChainPoint.Model (ChainPoint (ChainPoint, chainPointSlotNo), Key (ChainPointKey))
import Cardano.Index.Common (isOutputContainingAuthToken, isTxContainingAuthToken)
import Cardano.Index.Data.AddressText (AddressText (AddressText))
import Cardano.Index.Data.Utxo (GetUtxo, UsingMultipleFields (UsingMultipleFields))
import Cardano.Index.Handler (IndexHandler)
import Cardano.Index.Handler.Auto (IndexFilter (filterOutput, watch))
import Cardano.Index.Handler.Auto.Store (IndexStateStore)
import Cardano.Index.Handler.Auto.Types (IndexOutput (IndexOutput, ioDatum, ioTxOut, ioTxOutRef))
import Cardano.Index.Handler.NonContinuing (IndexNonContinuing (getNewStateNonContinuing), UsingNonContinuing)
import Cardano.Index.Utils (inAnyEra, isInline)
import Lending.Types.Manager (ManagerDatum, ManagerRedeemer)

Persist.share
  [Persist.mkPersistWith Persist.sqlSettings [Persist.entityDef (Proxy.Proxy @ChainPoint)]]
  [Persist.persistLowerCase|
    Manager json
      slotNo CA.SlotNo
      closingSlotNo CA.SlotNo Maybe
      scriptAddress (AddressText CA.AddressAny)
      datum (JSONB ManagerDatum)
      value (JSONB CA.Value)
      inline Bool
      ref CA.TxIn

      Primary ref
      Foreign ChainPoint OnDeleteCascade fk_produce_manager_events slotNo
      Foreign ChainPoint OnDeleteSetNull fk_consume_manager_events closingSlotNo

      deriving Generic Show Eq
|]

instance Scripts.ValidatorTypes Manager where
  type RedeemerType Manager = ManagerRedeemer
  type DatumType Manager = ManagerDatum

deriving anyclass instance
  GetUtxo
    (UsingMultipleFields "scriptAddress" "value" "datum" "inline" Void Manager)
    Manager

newtype ManagerConfig = ManagerConfig
  { ppcManagerNFT :: CA.AssetId
  -- ^ Manager authToken.
  }

instance IndexFilter ManagerConfig Manager Manager where
  watch = isTxContainingAuthToken . ppcManagerNFT

  filterOutput ManagerConfig {ppcManagerNFT} =
    (`inAnyEra` isOutputContainingAuthToken ppcManagerNFT) . ioTxOut

instance IndexNonContinuing ManagerConfig Manager Manager where
  getNewStateNonContinuing
    _
    ChainPoint {chainPointSlotNo}
    IndexOutput
      { ioTxOutRef
      , ioTxOut = CA.InAnyCardanoEra _ (CA.TxOut (CA.AddressInEra _ addr) txOutValue txOutDatum _)
      , ioDatum
      } =
      [ Manager
          { managerClosingSlotNo = Nothing
          , managerScriptAddress = AddressText (CA.toAddressAny addr)
          , managerDatum = JSONB ioDatum
          , managerValue = JSONB $ CA.txOutValueToValue txOutValue
          , managerInline = isInline txOutDatum
          , managerRef = ioTxOutRef
          , managerSlotNo = chainPointSlotNo
          }
      ]

deriving via
  (UsingNonContinuing Manager ManagerConfig Manager Manager)
  instance
    (IndexStateStore Manager m) => IndexHandler m Manager
