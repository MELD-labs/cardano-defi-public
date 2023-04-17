{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Common utility functions and types
module Lending.Api.Common
  ( addressFromScriptHashes
  , getCurrentTime
  )
where

import Cardano.Api qualified as CA
import Cardano.Api.Extra.Query qualified as CAE
import Cardano.Api.Shelley qualified as CA
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader qualified as Reader

import Cardano.Api.Extra.Time (getSlotTime)
import Lending.Api.Env
  ( AppEnv (AppEnv, aeNodeConnection)
  , AppM
  )
import Lending.Api.Types.Exception (ServerError (UnableToGetCurrentSlot))
import Lending.Core.Errors (eitherE)
import Lending.Core.Utils (posixTimeFromUTCTime, toSlotNo)
import Plutus.V1.Ledger.Api (POSIXTime)

-- | Produce cardano address from script hash
addressFromScriptHashes
  :: CA.NetworkId
  -> CA.ScriptHash
  -> Maybe (CA.Hash CA.StakeKey)
  -> CA.AddressInEra CA.BabbageEra
addressFromScriptHashes networkId paymentValidatorHash mStakeValidatorHash =
  CA.makeShelleyAddressInEra
    networkId
    (CA.PaymentCredentialByScript paymentValidatorHash)
    (maybe CA.NoStakeAddress (CA.StakeAddressByValue . CA.StakeCredentialByKey) mStakeValidatorHash)

getCurrentTime :: AppM POSIXTime
getCurrentTime = do
  AppEnv {aeNodeConnection} <- Reader.ask
  CA.SlotNo slotNo <- getCurrentSlotNo
  slotToUTCTime <- liftIO $ Reader.runReaderT getSlotTime aeNodeConnection
  eitherE
    UnableToGetCurrentSlot
    (posixTimeFromUTCTime . fst <$> slotToUTCTime (CA.SlotNo slotNo))
  where
    getCurrentSlotNo :: AppM CA.SlotNo
    getCurrentSlotNo =
      Reader.withReaderT aeNodeConnection $
        Reader.mapReaderT lift $
          toSlotNo <$> CAE.executeQueryInMode (CA.QueryChainPoint CA.CardanoMode)
