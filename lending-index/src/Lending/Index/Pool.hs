{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Model contains Pool' UTxO information
module Lending.Index.Pool where

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
import Cardano.Index.Handler.Auto.Types
  ( IndexOutput (IndexOutput, ioDatum, ioTxOut, ioTxOutRef)
  )
import Cardano.Index.Handler.NonContinuing (IndexNonContinuing (getNewStateNonContinuing), UsingNonContinuing)
import Cardano.Index.Utils (inAnyEra, isInline)
import Lending.Types.Pool qualified as LTP

Persist.share
  [Persist.mkPersistWith Persist.sqlSettings [Persist.entityDef (Proxy.Proxy @ChainPoint)]]
  [Persist.persistLowerCase|
    Pool json
      slotNo CA.SlotNo
      closingSlotNo CA.SlotNo Maybe
      ref CA.TxIn
      scriptAddress (AddressText CA.AddressAny)
      datum (JSONB LTP.PoolDatum)
      value (JSONB CA.Value)
      inline Bool

      Primary ref
      Foreign ChainPoint OnDeleteCascade fk_produce_pool_events slotNo
      Foreign ChainPoint OnDeleteSetNull fk_consume_pool_events closingSlotNo

      deriving Generic Show Eq
|]

instance Scripts.ValidatorTypes Pool where
  type RedeemerType Pool = LTP.PoolRedeemer
  type DatumType Pool = LTP.PoolDatum

deriving anyclass instance
  GetUtxo
    (UsingMultipleFields "scriptAddress" "value" "datum" "inline" Void Pool)
    Pool

newtype PoolConfig = PoolConfig
  { pcPoolNFT :: CA.AssetId
  -- ^ Pool authToken.
  }

instance IndexFilter PoolConfig Pool Pool where
  watch = isTxContainingAuthToken . pcPoolNFT

  filterOutput PoolConfig {pcPoolNFT} = (`inAnyEra` isOutputContainingAuthToken pcPoolNFT) . ioTxOut

instance IndexNonContinuing PoolConfig Pool Pool where
  getNewStateNonContinuing
    _
    ChainPoint {chainPointSlotNo}
    IndexOutput
      { ioTxOutRef
      , ioTxOut = CA.InAnyCardanoEra _ (CA.TxOut (CA.AddressInEra _ addr) txOutValue txOutDatum _)
      , ioDatum
      } =
      [ Pool
          { poolClosingSlotNo = Nothing
          , poolScriptAddress = AddressText (CA.toAddressAny addr)
          , poolDatum = JSONB ioDatum
          , poolValue = JSONB $ CA.txOutValueToValue txOutValue
          , poolInline = isInline txOutDatum
          , poolRef = ioTxOutRef
          , poolSlotNo = chainPointSlotNo
          }
      ]

deriving via
  (UsingNonContinuing Pool PoolConfig Pool Pool)
  instance
    (IndexStateStore Pool m) => IndexHandler m Pool
