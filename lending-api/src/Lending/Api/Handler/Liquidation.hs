module Lending.Api.Handler.Liquidation (liquidationApi) where

import Cardano.Api qualified as CA
import Database.Persist qualified as Persist
import Servant (ServerT)

import Lending.Api.Env
  ( AppEnv (aeNormalConnectionPool)
  , AppM
  , runSqlM
  )
import Lending.Api.Types.Liquidation
  ( BatchingInfo (BatchingInfo)
  , LiquidationApi
  , LiquidationData
    ( LiquidationData
    , ldBatchingInfo
    , ldContinuingAccount
    , ldLiquidatedAccount
    , ldLiquidatorAccount
    , ldLiquidatorAddress
    , ldTimestamp
    , ldTxId
    )
  , QueryLiquidationApi
  , QueryLiquidationResponse (QueryLiquidationResponse)
  )
import Lending.Core.AccountValue (AccountId (AccountId))
import Lending.Index.Account qualified as IA
import Lending.Index.Liquidation qualified as IL
import Lending.Index.Query.Account (getAccountDatumByAccountRef, queryLiquidationAppliedAccountByAccountId)
import Lending.Index.Query.Liquidation (queryMultipleLiquidations)
import Lending.Index.Query.Types (SqlM)

liquidationApi :: ServerT LiquidationApi AppM
liquidationApi = queryLiquidationApi

queryLiquidationApi :: ServerT QueryLiquidationApi AppM
queryLiquidationApi = queryLiquidationHandle

getBatchingInfo :: Persist.Entity IA.Account -> BatchingInfo
getBatchingInfo entity =
  let account = Persist.entityVal entity
      CA.TxIn txId _ = IA.accountRef account
   in BatchingInfo (CA.serialiseToRawBytesHexText txId) (IA.accountSlotNo account)

getLiquidationData :: Persist.Entity IL.Liquidation -> SqlM LiquidationData
getLiquidationData entity = do
  let liquidation = Persist.entityVal entity
  liquidatedAcc <-
    getAccountDatumByAccountRef (IL.liquidationLiquidatedAccountRef liquidation)
  continuingAcc <-
    getAccountDatumByAccountRef (IL.liquidationContinuingAccountRef liquidation)
  liquidatorAcc <-
    getAccountDatumByAccountRef (IL.liquidationLiquidatorAccountRef liquidation)
  appliedAcc <-
    queryLiquidationAppliedAccountByAccountId (AccountId $ IL.liquidationLiquidatorAccountRef liquidation)
  pure $
    LiquidationData
      { ldTxId = IL.liquidationTxId liquidation
      , ldLiquidatorAddress = IL.liquidationLiquidatorAddress liquidation
      , ldTimestamp = IL.liquidationSlotNo liquidation
      , ldBatchingInfo = getBatchingInfo <$> appliedAcc
      , ldLiquidatedAccount = liquidatedAcc
      , ldContinuingAccount = continuingAcc
      , ldLiquidatorAccount = liquidatorAcc
      }

extractLiquidationData :: Persist.Entity IL.Liquidation -> AppM LiquidationData
extractLiquidationData entity =
  runSqlM (getLiquidationData entity) aeNormalConnectionPool

queryLiquidationHandle :: [CA.TxIn] -> AppM QueryLiquidationResponse
queryLiquidationHandle refs = do
  entities <- runSqlM (queryMultipleLiquidations refs) aeNormalConnectionPool
  datas <- traverse extractLiquidationData entities
  pure $ QueryLiquidationResponse datas
