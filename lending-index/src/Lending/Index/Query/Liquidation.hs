{-# LANGUAGE RankNTypes #-}

module Lending.Index.Query.Liquidation
  ( queryAllLiquidations
  , queryLiquidationByLiquidatedAccRef
  , queryLiquidationsByLiquidatorAddress
  , queryMultipleLiquidations
  , queryLatestLiquidations
  )
where

import Cardano.Api qualified as CA
import Database.Persist qualified as Persist

import Cardano.Index.Data.AddressText (AddressText)
import Lending.Index.Liquidation qualified as IL
import Lending.Index.Query.Types (SqlM)

queryAllLiquidations :: SqlM [Persist.Entity IL.Liquidation]
queryAllLiquidations = Persist.selectList [] []

queryLiquidationByLiquidatedAccRef :: CA.TxIn -> SqlM (Maybe (Persist.Entity IL.Liquidation))
queryLiquidationByLiquidatedAccRef ref =
  Persist.selectFirst
    [IL.LiquidationLiquidatedAccountRef Persist.==. ref]
    []

queryMultipleLiquidations :: [CA.TxIn] -> SqlM [Persist.Entity IL.Liquidation]
queryMultipleLiquidations refs =
  Persist.selectList
    [IL.LiquidationLiquidatedAccountRef Persist.<-. refs]
    []

queryLiquidationsByLiquidatorAddress :: AddressText CA.AddressAny -> SqlM [Persist.Entity IL.Liquidation]
queryLiquidationsByLiquidatorAddress addr =
  Persist.selectList
    [IL.LiquidationLiquidatorAddress Persist.==. addr]
    []

queryLatestLiquidations :: AddressText CA.AddressAny -> CA.TxIn -> SqlM (Maybe (Persist.Entity IL.Liquidation))
queryLatestLiquidations liquidatorAddress accountOriginalRef =
  Persist.selectFirst
    [ IL.LiquidationLiquidatorAddress Persist.==. liquidatorAddress
    , IL.LiquidationLiquidatedAccountOriginalRef Persist.==. accountOriginalRef
    ]
    [Persist.Desc IL.LiquidationSlotNo]
