{-# LANGUAGE RankNTypes #-}

module Lending.Services.Transactions.Utils
  ( extractAccountInput
  , extractPoolInput
  )
where

import Database.Persist qualified as Persist
import Lending.Core.Utils (extractUtxoInputWithDatum)
import Lending.Index.Account (Account, EntityField (AccountDatum, AccountRef))
import Lending.Index.Pool (EntityField (PoolDatum, PoolRef), Pool)
import Lending.Types qualified as Types
import TxBuilder.Api (UtxoInputWithDatum)

extractAccountInput :: Persist.Entity Account -> Maybe (UtxoInputWithDatum Types.AccountDatum)
extractAccountInput = extractUtxoInputWithDatum AccountRef AccountDatum

extractPoolInput :: Persist.Entity Pool -> Maybe (UtxoInputWithDatum Types.PoolDatum)
extractPoolInput = extractUtxoInputWithDatum PoolRef PoolDatum
