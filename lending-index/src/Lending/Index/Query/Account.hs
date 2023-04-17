{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Index.Query.Account
  ( getAccountUtxoByAccountId
  , getAllAccountUtxos
  , getMultipleAccountUtxosByOwnerNft
  , getExtractedAccountByAccountRef
  , getAccountUtxoByAccountRef
  , queryAccountByAccountId
  , queryHistoryAccountByAccountId
  , queryLiquidationAppliedAccountByAccountId
  , getAccountDatumByAccountRef
  , getAllValueOfOpeningAccount
  )
where

import Cardano.Api qualified as CA
import Control.Monad.Catch (throwM)
import Data.Aeson (decode)
import Data.Foldable (Foldable (fold))
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental
  ( Entity (entityVal)
  , Value (Value)
  , desc
  , from
  , in_
  , isNothing
  , just
  , leftJoin
  , limit
  , offset
  , on
  , orderBy
  , select
  , table
  , val
  , valList
  , where_
  , (&&.)
  , (==.)
  , (?.)
  , (^.)
  , (||.)
  , type (:&) ((:&))
  )
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB), (->>.))
import Database.Persist qualified as Persist

import Cardano.Api.Extra.AssetId (AssetIdText (unAssetIdText))
import Cardano.Index.ChainPoint.Model qualified as CP

import Data.Map (Map)
import Lending.Core.AccountValue
  ( AccountId (AccountId, aiOriginalRef)
  , ExtractedAccount (ExtractedAccount)
  )
import Lending.Core.Utils (extractUtxoInputWithDatum)
import Lending.Index.Account qualified as IA
import Lending.Index.Exception (IndexException (IndexAccountUtxoNotFoundException))
import Lending.Index.Query.Types (SqlM)
import Lending.Types.Account qualified as TA
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Receipt)
import Plutarch.Extra.AssetClass (AssetClass, toPlutusAssetClass)
import TxBuilder.Api (UtxoInputWithDatum (uiwdDatum))

queryAllAccountUtxos :: SqlM [Persist.Entity IA.Account]
queryAllAccountUtxos =
  Persist.selectList
    [ IA.AccountClosingSlotNo Persist.==. Nothing
    ]
    []

queryAccountByAccountId :: AccountId -> SqlM (Maybe (Persist.Entity IA.Account))
queryAccountByAccountId (AccountId originRef) =
  Persist.selectFirst
    [ IA.AccountOriginalRef Persist.==. originRef
    , IA.AccountClosingSlotNo Persist.==. Nothing
    ]
    []

queryLiquidationAppliedAccountByAccountId :: AccountId -> SqlM (Maybe (Persist.Entity IA.Account))
queryLiquidationAppliedAccountByAccountId (AccountId originRef) =
  Persist.selectFirst
    [ IA.AccountOriginalRef Persist.==. originRef
    , IA.AccountIsLiquidationApplied Persist.==. True
    ]
    []

queryAccountByAccountRef :: CA.TxIn -> SqlM (Maybe (Persist.Entity IA.Account))
queryAccountByAccountRef ref =
  Persist.selectFirst
    [ IA.AccountRef Persist.==. ref
    , IA.AccountClosingSlotNo Persist.==. Nothing
    ]
    []

queryAccountByAccountRefInAllAccounts :: CA.TxIn -> SqlM (Maybe (Persist.Entity IA.Account))
queryAccountByAccountRefInAllAccounts ref =
  Persist.selectFirst [IA.AccountRef Persist.==. ref] []

queryAccountUtxos :: [JSONB AssetClass] -> SqlM [Persist.Entity IA.Account]
queryAccountUtxos userNfts =
  Persist.selectList
    [ IA.AccountUserNft Persist.<-. userNfts
    , IA.AccountClosingSlotNo Persist.==. Nothing
    ]
    []

extractAccount :: Persist.Entity IA.Account -> Maybe ExtractedAccount
extractAccount entityAccount =
  let account = Persist.entityVal entityAccount
   in ExtractedAccount
        (AccountId $ IA.accountOriginalRef account)
        (IA.accountRef account)
        <$> extractUtxoInputWithDatum IA.AccountRef IA.AccountDatum entityAccount

getAllAccountUtxos :: SqlM [ExtractedAccount]
getAllAccountUtxos = mapMaybe extractAccount <$> queryAllAccountUtxos

getAccountUtxoByAccountId :: AccountId -> SqlM (UtxoInputWithDatum TA.AccountDatum)
getAccountUtxoByAccountId accId =
  maybe (throwM $ IndexAccountUtxoNotFoundException (aiOriginalRef accId)) pure
    . (extractUtxoInputWithDatum IA.AccountRef IA.AccountDatum =<<)
    =<< queryAccountByAccountId accId

getAccountUtxoByAccountRef :: CA.TxIn -> SqlM (UtxoInputWithDatum TA.AccountDatum)
getAccountUtxoByAccountRef =
  flip getExtractedAccountByAccountRef $ extractUtxoInputWithDatum IA.AccountRef IA.AccountDatum

getAccountDatumByAccountRef :: CA.TxIn -> SqlM TA.AccountDatum
getAccountDatumByAccountRef =
  flip getExtractedAccountByAccountRefInAllAccounts $
    (uiwdDatum <$>) . extractUtxoInputWithDatum IA.AccountRef IA.AccountDatum

getMultipleAccountUtxosByOwnerNft :: [AssetIdText] -> SqlM [ExtractedAccount]
getMultipleAccountUtxosByOwnerNft =
  (mapMaybe extractAccount <$>)
    . queryAccountUtxos
    . (JSONB . toPlutusAssetClass . unAssetIdText <$>)

getExtractedAccountByAccountRef :: CA.TxIn -> (Persist.Entity IA.Account -> Maybe a) -> SqlM a
getExtractedAccountByAccountRef ref extractor = do
  maybe (throwM $ IndexAccountUtxoNotFoundException ref) pure
    . (extractor =<<)
    =<< queryAccountByAccountRef ref

{- HLINT ignore "Fuse on/on" -}
queryHistoryAccountByAccountId
  :: AccountId
  -> Int64
  -> Int64
  -> SqlM [(IA.Account, Maybe (Maybe (JSONB TA.AccountRedeemer)), Maybe CA.TxIn, Maybe UTCTime, Maybe UTCTime)]
queryHistoryAccountByAccountId (AccountId originRef) limitHistory offsetHistory = do
  res <- select $ do
    (a1 :& a2 :& cp1 :& cp2) <-
      from
        $ table @IA.Account
          `leftJoin` table @IA.Account
        `on` ( \(a1 :& a2) ->
                (a1 ^. IA.AccountClosingSlotNo ==. a2 ?. IA.AccountSlotNo)
                  &&. ( just (a1 ^. IA.AccountOriginalRef)
                          ==. a2 ?. IA.AccountOriginalRef
                      )
             )
          `leftJoin` table @CP.ChainPoint
        `on` ( \(a1 :& _ :& cp1) ->
                just (a1 ^. IA.AccountSlotNo)
                  ==. cp1
                    ?. CP.ChainPointSlotNo
             )
          `leftJoin` table @CP.ChainPoint
        `on` ( \(a1 :& _ :& _ :& cp2) ->
                a1 ^. IA.AccountClosingSlotNo
                  ==. cp2
                    ?. CP.ChainPointSlotNo
             )
    where_ $
      (a1 ^. IA.AccountOriginalRef ==. val originRef)
        &&. ( ( ( (a1 ^. IA.AccountRedeemer) ->>. "tag"
                )
                  `in_` valList
                    [ Nothing
                    , Just "AccountLiquidateRedeemer"
                    , Just "AccountUpdateRedeemer"
                    ]
              )
                ||. isNothing (a1 ^. IA.AccountRedeemer)
            )
    limit limitHistory
    offset offsetHistory
    orderBy [desc (a1 ^. IA.AccountSlotNo)]
    pure
      ( a1
      , a2 ?. IA.AccountRedeemer
      , a2 ?. IA.AccountRef
      , cp1 ?. CP.ChainPointTime
      , cp2 ?. CP.ChainPointTime
      )

  pure $ convert <$> res
  where
    convert
      :: ( Entity IA.Account
         , Value (Maybe (Maybe (JSONB TA.AccountRedeemer)))
         , Value (Maybe CA.TxIn)
         , Value (Maybe UTCTime)
         , Value (Maybe UTCTime)
         )
      -> ( IA.Account
         , Maybe (Maybe (JSONB TA.AccountRedeemer))
         , Maybe CA.TxIn
         , Maybe UTCTime
         , Maybe UTCTime
         )
    convert
      ( account
        , Value mRedeemer
        , Value mRef
        , Value time1
        , Value time2
        ) = (entityVal account, mRedeemer, mRef, time1, time2)

getExtractedAccountByAccountRefInAllAccounts :: CA.TxIn -> (Persist.Entity IA.Account -> Maybe a) -> SqlM a
getExtractedAccountByAccountRefInAllAccounts ref extractor = do
  maybe (throwM $ IndexAccountUtxoNotFoundException ref) pure
    . (extractor =<<)
    =<< queryAccountByAccountRefInAllAccounts ref

getAllValueOfOpeningAccount :: SqlM [(Map Asset Receipt, Map Asset Receipt)]
getAllValueOfOpeningAccount = do
  res <- select $ do
    a <- from $ table @IA.Account
    where_ (isNothing (a ^. IA.AccountClosingSlotNo))
    pure (just (a ^. IA.AccountDatum) ->>. "adSupplies", just (a ^. IA.AccountDatum) ->>. "adBorrowings")
  pure $ convert <$> res
  where
    convert :: (Value (Maybe T.Text), Value (Maybe T.Text)) -> (Map Asset Receipt, Map Asset Receipt)
    convert (Value supplyMap, Value borrowMap) =
      ( fold (supplyMap >>= decode . encodeUtf8 . fromStrict)
      , fold (borrowMap >>= decode . encodeUtf8 . fromStrict)
      )
