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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Index.Liquidation where

import Cardano.Api qualified as CA
import Data.List.Extra qualified as List
import Data.Maybe qualified as Maybe
import Data.Proxy qualified as Proxy
import Data.Text (Text)
import Database.Persist qualified as Persist
import Database.Persist.TH qualified as Persist
import GHC.Generics (Generic)

import Cardano.Index.ChainPoint.Model (ChainPoint (ChainPoint, chainPointSlotNo))
import Cardano.Index.Common (isOutputContainingAuthToken, isTxValid)
import Cardano.Index.Data.AddressText (AddressText (AddressText))
import Cardano.Index.Handler (IndexHandler (IndexContext, runHandler))
import Cardano.Index.Handler.Auto
  ( IndexScriptData (extractDatum, extractRedeemer)
  )
import Cardano.Index.Handler.Auto.Store
  ( IndexStateAppendStore (insertNewStates)
  , IndexStateStore (selectExistingStates)
  )
import Cardano.Index.Handler.Auto.Types
  ( IndexAutoCommonFields (iacfRef)
  , IndexInput (IndexInput, iiRedeemer, iiStateList, iiTxOutRef)
  , IndexOutput (IndexOutput, ioDatum, ioTxOutRef)
  )
import Cardano.Index.PersistLens (view)
import Control.Monad (join)
import Data.Foldable (traverse_)
import Lending.Core.Utils (txOutAddress)
import Lending.Index.Account (Account (accountOriginalRef))
import Lending.Types.Account (AccountDatum (adUserNft))
import Lending.Types.Account qualified as LTA
import Plutarch.Extra.AssetClass (fromPlutusAssetClass)

Persist.share
  [Persist.mkPersistWith Persist.sqlSettings [Persist.entityDef (Proxy.Proxy @ChainPoint)]]
  [Persist.persistLowerCase|
    Liquidation json
      txId Text
      liquidatorAddress (AddressText CA.AddressAny)
      liquidatedAccountOriginalRef CA.TxIn
      liquidatedAccountRef CA.TxIn
      continuingAccountRef CA.TxIn
      liquidatorAccountRef CA.TxIn
      slotNo CA.SlotNo

      Primary txId

      deriving Generic Show Eq

|]

newtype LiquidationConfig = LiquidationConfig
  { lcAccountAuthToken :: CA.AssetId
  }

instance
  ( IndexStateAppendStore Liquidation m
  , IndexStateStore Account m
  , Monad m
  )
  => IndexHandler m Liquidation
  where
  type IndexContext Liquidation = LiquidationConfig
  runHandler context ChainPoint {chainPointSlotNo} =
    traverse_ extractTx . filter shouldProcess
    where
      watch :: LiquidationConfig -> CA.Tx era -> Bool
      watch LiquidationConfig {lcAccountAuthToken} (CA.Tx (CA.TxBody CA.TxBodyContent {txOuts}) _) =
        length (filter (isOutputContainingAuthToken lcAccountAuthToken) txOuts) == 2

      shouldProcess :: CA.InAnyCardanoEra CA.Tx -> Bool
      shouldProcess (CA.InAnyCardanoEra _ tx) = isTxValid tx && watch context tx

      queryStates :: [CA.TxIn] -> m [(CA.TxIn, [Persist.Entity Account])]
      queryStates txIns =
        (\allExistingStates -> (\txIn -> (txIn, filter ((txIn ==) . view iacfRef) allExistingStates)) <$> txIns)
          <$> selectExistingStates txIns

      extractTx :: CA.InAnyCardanoEra CA.Tx -> m ()
      extractTx
        (CA.InAnyCardanoEra era tx@(CA.Tx txBody@(CA.TxBody CA.TxBodyContent {txIns, txOuts}) _)) = do
          existingStates <- queryStates (fst <$> txIns)
          let inputs = getAccountInputs existingStates
              outputs = getOutputs
              liqs = liquidations inputs outputs
          insertNewStates @Liquidation liqs
          where
            getAccountInputs :: [(CA.TxIn, [Persist.Entity Account])] -> [IndexInput Account Account]
            getAccountInputs = Maybe.mapMaybe getInput
              where
                getInput (txIn, stateEntity) =
                  IndexInput txIn
                    <$> extractRedeemer @Account tx txIn
                    <*> pure stateEntity

            getOutputs :: [IndexOutput Account]
            getOutputs =
              Maybe.catMaybes (List.zipWithFrom getOutput 0 txOuts)
              where
                txId = CA.getTxId txBody
                getOutput idx txOut =
                  IndexOutput (CA.TxIn txId (CA.TxIx idx)) (CA.InAnyCardanoEra era txOut)
                    <$> extractDatum @Account txOut

            toAddressAny :: CA.AddressInEra era -> CA.AddressAny
            toAddressAny (CA.AddressInEra _ addr) = CA.toAddressAny addr

            liquidations :: [IndexInput Account Account] -> [IndexOutput Account] -> [Liquidation]
            liquidations inputs outputs = case (inputs, outputs) of
              ( [ IndexInput
                    { iiRedeemer = LTA.AccountLiquidateRedeemer LTA.AccountLiquidateRedeemerData {}
                    , iiTxOutRef = inputTxIn
                    , iiStateList
                    }
                  ]
                , [ IndexOutput
                      { ioTxOutRef = continuingAccRef@(CA.TxIn txId _)
                      }
                    , IndexOutput
                      { ioDatum = LTA.AccountDatum {adUserNft}
                      , ioTxOutRef = liquidatorAccRef
                      }
                    ]
                ) ->
                  let txIdText = CA.serialiseToRawBytesHexText txId
                      ownerNft = fromPlutusAssetClass adUserNft
                      liquidatorUtxo = Maybe.listToMaybe . flip filter txOuts . isOutputContainingAuthToken <$> ownerNft
                      liquidatorAddress = AddressText . toAddressAny . txOutAddress <$> join liquidatorUtxo
                      originalRef = Maybe.listToMaybe $ accountOriginalRef . Persist.entityVal <$> iiStateList
                      result = case liquidatorAddress of
                        Just address ->
                          [ Liquidation
                              { liquidationTxId = txIdText
                              , liquidationLiquidatorAddress = address
                              , liquidationLiquidatedAccountOriginalRef = Maybe.fromJust originalRef -- FIXME
                              , liquidationLiquidatedAccountRef = inputTxIn
                              , liquidationContinuingAccountRef = continuingAccRef
                              , liquidationLiquidatorAccountRef = liquidatorAccRef
                              , liquidationSlotNo = chainPointSlotNo
                              }
                          ]
                        Nothing -> []
                   in result
              _ -> []
