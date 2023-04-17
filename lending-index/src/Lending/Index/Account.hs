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

-- | Account model contains Account's UTxO information
module Lending.Index.Account where

import Cardano.Api qualified as CA
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Proxy qualified as Proxy
import Data.Void (Void)
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB, unJSONB))
import Database.Persist qualified as Persist
import Database.Persist.TH qualified as Persist
import GHC.Generics (Generic)
import Plutarch.Extra.AssetClass (AssetClass)
import Plutus.Script.Utils.Typed qualified as Scripts

import Cardano.Index.ChainPoint.Model (ChainPoint (ChainPoint, chainPointSlotNo), Key (ChainPointKey))
import Cardano.Index.Common (isOutputContainingAuthToken, isTxContainingAuthToken)
import Cardano.Index.Data.AddressText (AddressText (AddressText))
import Cardano.Index.Data.Utxo (GetUtxo, UsingMultipleFields (UsingMultipleFields))
import Cardano.Index.Handler (IndexHandler)
import Cardano.Index.Handler.Auto (IndexFilter (filterInput, filterOutput, watch))
import Cardano.Index.Handler.Auto.Store (IndexStateStore)
import Cardano.Index.Handler.Auto.Types
  ( IndexInput (IndexInput, iiRedeemer, iiStateList)
  , IndexOutput (IndexOutput, ioDatum, ioTxOut, ioTxOutRef)
  )
import Cardano.Index.Handler.Continuing
  ( IndexContinuing (classify, persistNewStates)
  , UsingContinuing
  , matching
  )
import Cardano.Index.Utils (inAnyEra, isInline)
import Lending.Types.Account qualified as LTA

Persist.share
  [Persist.mkPersistWith Persist.sqlSettings [Persist.entityDef (Proxy.Proxy @ChainPoint)]]
  [Persist.persistLowerCase|
    Account json
      slotNo CA.SlotNo
      closingSlotNo CA.SlotNo Maybe
      originalRef CA.TxIn
      ref CA.TxIn
      userNft (JSONB AssetClass)
      scriptAddress (AddressText CA.AddressAny)
      datum (JSONB LTA.AccountDatum)
      value (JSONB CA.Value)
      redeemer (JSONB LTA.AccountRedeemer) Maybe
      isLiquidationApplied Bool
      inline Bool
      hasRequests Bool

      Primary originalRef ref
      Foreign ChainPoint OnDeleteCascade fk_produce_account_events slotNo
      Foreign ChainPoint OnDeleteSetNull fk_consume_account_events closingSlotNo

      deriving Generic Show Eq
|]

instance Scripts.ValidatorTypes Account where
  type RedeemerType Account = LTA.AccountRedeemer
  type DatumType Account = LTA.AccountDatum

deriving anyclass instance
  GetUtxo
    (UsingMultipleFields "scriptAddress" "value" "datum" "inline" Void Account)
    Account

newtype AccountConfig = AccountConfig
  { acAccountAuthToken :: CA.AssetId
  -- ^ Account authToken.
  }

instance IndexFilter AccountConfig Account Account where
  watch = isTxContainingAuthToken . acAccountAuthToken

  filterInput _ = not . null . iiStateList

  filterOutput AccountConfig {acAccountAuthToken} =
    (`inAnyEra` isOutputContainingAuthToken acAccountAuthToken) . ioTxOut

instance IndexContinuing AccountConfig Account Account where
  classify = matching (\_ _ _ -> True)
  persistNewStates
    _
    ChainPoint {chainPointSlotNo}
    maybeInput
    IndexOutput
      { ioTxOutRef
      , ioTxOut = CA.InAnyCardanoEra _ (CA.TxOut (CA.AddressInEra _ addr) txOutValue txOutDatum _)
      , ioDatum = datum@(LTA.AccountDatum _ _ _ _ pRequests pCollateral pProtocolIncentive pClearRequest _)
      } =
      Maybe.maybeToList $
        let getOriginalRef :: IndexInput Account Account -> Maybe CA.TxIn
            getOriginalRef IndexInput {iiStateList} =
              Maybe.listToMaybe $ accountOriginalRef . Persist.entityVal <$> iiStateList
            getAccountDatum :: IndexInput Account Account -> Maybe LTA.AccountDatum
            getAccountDatum IndexInput {iiStateList} =
              Maybe.listToMaybe $ unJSONB . accountDatum . Persist.entityVal <$> iiStateList
            originalRef = Maybe.fromMaybe ioTxOutRef (maybeInput >>= getOriginalRef)
            oldAccProtocolIncentive = maybeInput >>= getAccountDatum >>= LTA.adProtocolIncentive
            redeemer = JSONB . iiRedeemer <$> maybeInput
         in pure $
              Account
                { accountClosingSlotNo = Nothing
                , accountScriptAddress = AddressText (CA.toAddressAny addr)
                , accountDatum = JSONB datum
                , accountValue = JSONB $ CA.txOutValueToValue txOutValue
                , accountInline = isInline txOutDatum
                , accountRef = ioTxOutRef
                , accountUserNft = JSONB $ LTA.adUserNft datum
                , accountOriginalRef = originalRef
                , accountSlotNo = chainPointSlotNo
                , accountHasRequests =
                    not (null pRequests)
                      || not (null pCollateral)
                      || not (Map.null pClearRequest)
                , accountRedeemer = redeemer
                , accountIsLiquidationApplied =
                    Maybe.isJust oldAccProtocolIncentive && Maybe.isNothing pProtocolIncentive
                }

deriving via
  (UsingContinuing Account AccountConfig Account Account)
  instance
    IndexStateStore Account m => IndexHandler m Account
