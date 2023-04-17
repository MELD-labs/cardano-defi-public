{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Lending.Core.MinAdaUtxo (calculateMinAdaAccountUtxo, getAsset) where

import Data.Set qualified as Set

import Data.Set (Set)
import Lending.Types.Account
  ( Request (BorrowRequest, RepayRequest, SupplyRequest, WithdrawRequest, brAsset, rrAsset, srAsset, wrAsset)
  )
import Lending.Types.Asset (Asset)

getAsset :: Request -> Asset
getAsset SupplyRequest {srAsset} = srAsset
getAsset WithdrawRequest {wrAsset} = wrAsset
getAsset BorrowRequest {brAsset} = brAsset
getAsset RepayRequest {rrAsset} = rrAsset

baseLovelace :: Integer
baseLovelace = 2_000_000

extraLovelacePerBatch :: Integer
extraLovelacePerBatch = 500_000

minAdaUtxoBatchSize :: Integer
minAdaUtxoBatchSize = 5

-- | Calculate MinAdaUtxo of Account Utxo by: numberOfBatch * minAdaUtxo
calculateMinAdaAccountUtxo :: Set Asset -> Integer
calculateMinAdaAccountUtxo assets =
  let assetCount = toInteger (Set.size assets)
      batchCount = assetCount `div` minAdaUtxoBatchSize
   in baseLovelace + extraLovelacePerBatch * batchCount
