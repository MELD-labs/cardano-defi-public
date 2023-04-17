{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lending.Api.Handler.SyncStatus (syncStatusApi) where

import Cardano.Api qualified as CA
import Cardano.Api.Extra.Query qualified as CAE
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Map (Map, foldrWithKey)
import Servant (ServerT)

import Cardano.Index.ChainPoint.Model (ChainPoint (ChainPoint, chainPointSlotNo))
import Data.Map.Strict (unionWith)
import Lending.Api.Env (AppEnv (aeNodeConnection, aeNormalConnectionPool), AppM, runSqlM)
import Lending.Api.Types.SyncStatus
  ( AmountAsset (AmountAsset, aaBorrowAmountAccount, aaBorrowPool, aaSupplyAmountAccount, aaSupplyPool)
  , SyncStatusApi
  , SyncStatusResponse (SyncStatusResponse)
  )
import Lending.Core.Utils (toSlotNo)
import Lending.Index.Query.Account (getAllValueOfOpeningAccount)
import Lending.Index.Query.ChainPoint (getLatestChainPoint)
import Lending.Index.Query.Pool (getPoolUtxo)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Receipt)
import Lending.Types.Pool
  ( AssetInformation (aiBorrowAmount, aiSupplyAmount)
  , PoolDatum (PoolDatum, pdAssets)
  )
import Plutarch.Extra.FixedDecimal (FixedDecimal, ediv)
import TxBuilder.Api (UtxoInputWithDatum (UtxoInputWithDatum))

syncStatusApi :: ServerT SyncStatusApi AppM
syncStatusApi = syncStatusH

syncStatusH :: AppM SyncStatusResponse
syncStatusH = do
  latestChainPoint@ChainPoint {chainPointSlotNo = CA.SlotNo lSlotNo} <-
    runSqlM getLatestChainPoint aeNormalConnectionPool
  listAmount <- runSqlM getAllValueOfOpeningAccount aeNormalConnectionPool
  CA.SlotNo cSlotNo <- getCurrentSlotNo
  UtxoInputWithDatum _ PoolDatum {pdAssets} <- runSqlM getPoolUtxo aeNormalConnectionPool
  let (supplyMap, borrowMap) = foldl unionAmount (mempty, mempty) listAmount
      statusAmount =
        fmap
          ( \(supplyAmountAccount, borrowAmountAccount, supplyAmountPool, borrowAmountPool) ->
              AmountAsset
                (sum supplyAmountAccount)
                (sum borrowAmountAccount)
                (sum supplyAmountPool)
                (sum borrowAmountPool)
          )
          (consMaps supplyMap borrowMap pdAssets)
      currentSlotNo = fromInteger (toInteger cSlotNo) :: FixedDecimal 4
      latestSlotNo = fromInteger (toInteger lSlotNo) :: FixedDecimal 8
      assetAnomaly =
        foldrWithKey
          ( \k amountAsset result ->
              if aaSupplyAmountAccount amountAsset /= aaSupplyPool amountAsset
                || aaBorrowAmountAccount amountAsset /= aaBorrowPool amountAsset
                then result <> [k]
                else result
          )
          []
          statusAmount
  pure $ SyncStatusResponse latestChainPoint (latestSlotNo `ediv` currentSlotNo) statusAmount assetAnomaly
  where
    getCurrentSlotNo :: AppM CA.SlotNo
    getCurrentSlotNo =
      Reader.withReaderT aeNodeConnection $
        Reader.mapReaderT lift $
          toSlotNo <$> CAE.executeQueryInMode (CA.QueryChainPoint CA.CardanoMode)
    unionAmount
      :: (Map Asset Receipt, Map Asset Receipt)
      -> (Map Asset Receipt, Map Asset Receipt)
      -> (Map Asset Receipt, Map Asset Receipt)
    unionAmount (curSupplyMap, curBorrowMap) (supplyData, borrowData) =
      (unionWith (+) curSupplyMap supplyData, unionWith (+) curBorrowMap borrowData)

consMaps
  :: Map Asset Receipt
  -> Map Asset Receipt
  -> Map Asset AssetInformation
  -> Map Asset (Maybe Receipt, Maybe Receipt, Maybe Receipt, Maybe Receipt)
consMaps ma mb mp = unionWith combinePool mp' (unionWith combineAccount ma' mb')
  where
    ma' = fmap (\a -> (Just a, Nothing, Nothing, Nothing)) ma
    mb' = fmap (\b -> (Nothing, Just b, Nothing, Nothing)) mb
    mp' = fmap (\p -> (Nothing, Nothing, Just (aiSupplyAmount p), Just (aiBorrowAmount p))) mp
    combineAccount (a, _, _, _) (_, b, _, _) = (a, b, Nothing, Nothing)
    combinePool (_, _, pa, pb) (a, b, _, _) = (a, b, pa, pb)
