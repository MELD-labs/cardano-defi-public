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

-- | Oracle model contains Oracle's UTxO information
module Lending.Index.Oracle where

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
import Lending.Types.Oracle (OracleDatum)

Persist.share
  [Persist.mkPersistWith Persist.sqlSettings [Persist.entityDef (Proxy.Proxy @ChainPoint)]]
  [Persist.persistLowerCase|
    Oracle json
      slotNo CA.SlotNo
      closingSlotNo CA.SlotNo Maybe
      scriptAddress (AddressText CA.AddressAny)
      datum (JSONB OracleDatum)
      value (JSONB CA.Value)
      inline Bool
      ref CA.TxIn

      Primary ref
      Foreign ChainPoint OnDeleteCascade fk_produce_oracle_events slotNo
      Foreign ChainPoint OnDeleteSetNull fk_consume_oracle_events closingSlotNo

      deriving Generic Show Eq
|]

instance Scripts.ValidatorTypes Oracle where
  type RedeemerType Oracle = ()
  type DatumType Oracle = OracleDatum

deriving anyclass instance
  GetUtxo
    (UsingMultipleFields "scriptAddress" "value" "datum" "inline" Void Oracle)
    Oracle

newtype OracleConfig = OracleConfig
  { ocOracleNFT :: CA.AssetId
  -- ^ Oracle authToken.
  }

instance IndexFilter OracleConfig Oracle Oracle where
  watch = isTxContainingAuthToken . ocOracleNFT

  filterOutput OracleConfig {ocOracleNFT} =
    (`inAnyEra` isOutputContainingAuthToken ocOracleNFT) . ioTxOut

instance IndexNonContinuing OracleConfig Oracle Oracle where
  getNewStateNonContinuing
    _
    ChainPoint {chainPointSlotNo}
    IndexOutput
      { ioTxOutRef
      , ioTxOut = CA.InAnyCardanoEra _ (CA.TxOut (CA.AddressInEra _ addr) txOutValue txOutDatum _)
      , ioDatum
      } =
      [ Oracle
          { oracleClosingSlotNo = Nothing
          , oracleScriptAddress = AddressText (CA.toAddressAny addr)
          , oracleDatum = JSONB ioDatum
          , oracleValue = JSONB $ CA.txOutValueToValue txOutValue
          , oracleInline = isInline txOutDatum
          , oracleRef = ioTxOutRef
          , oracleSlotNo = chainPointSlotNo
          }
      ]

deriving via
  (UsingNonContinuing Oracle OracleConfig Oracle Oracle)
  instance
    (IndexStateStore Oracle m) => IndexHandler m Oracle
