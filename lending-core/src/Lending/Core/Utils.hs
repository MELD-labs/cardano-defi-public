{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Core.Utils
  ( extractUtxoInput
  , extractUtxoInputWithDatum
  , sumUtxoValue
  , selectUserUtxos
  , toTxSenderConstraints
  , fromCardanoAddressInEra
  , fromTxOutInlineDatum
  , toCardanoAddressInEra
  , queryInputsAndCollaterals
  , toSlotNo
  , posixTimeFromUTCTime
  , withDbPoolLending
  , toWalletUtxo
  , calculateUtxoValueSatisfyMinAda
  , utxoAddress
  , txOutAddress
  , fromTxOutDatumUtxoToTx
  , fromTxOutUtxoToTx
  , getChangeAddress
  , getStakingAddress
  , buildSignAndSubmitTx
  , loadTextEnvelope
  , queryApi
  , toTxInx
  , getUserInputs
  , isAdaOnly
  , calculateAmount
  , queryUserInputsAndCollateralBaseOnSpendingValue
  , queryUserInputsAndCollaterals
  , getAddress
  , getTxIdFromTxIn
  )
where

import Cardano.Api.Byron qualified as CA
import Cardano.Api.Shelley qualified as CA
import Control.Monad ((>=>))
import Control.Monad qualified as Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Logger qualified as Logger
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.Resource (MonadThrow, MonadUnliftIO)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB, unJSONB))
import Database.Persist qualified as Persist
import Database.Persist.Postgresql (ConnectionPool, ConnectionString)
import Database.Persist.Postgresql qualified as Persist
import Database.PostgreSQL.Simple qualified as PostgreSQL
import PlutusLedgerApi.V2 (POSIXTime (POSIXTime))
import PlutusTx qualified
import Servant.Client (ClientEnv, ClientError, ClientM)
import Servant.Client qualified as Client

import Cardano.Api.Extra.Adapters (fromCardanoAddressInEra, toAddressAny, toCardanoAddressInEra)
import Cardano.Api.Extra.NetworkParams (getNetworkParams)
import Cardano.Api.Extra.Node (NodeConnectInfo, NodeIO)
import Cardano.Api.Extra.Query (executeShelleyQuery)
import Cardano.Api.Extra.Tx (signAndSubmitTx)
import Cardano.Index.Data.AddressText (AddressText (unAddressText))
import Cardano.Index.Data.Utxo (GetUtxo (getUtxo), fromAnyUtxo)
import Cardano.Index.Extra.WalletUtxo
  ( CardanoWalletUtxo (CardanoWalletUtxo, cardanoWalletUtxoRef, cardanoWalletUtxoUtxo)
  )
import Cardano.Index.PersistLens (view)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, CumulativeRate, Receipt, convertFrom)
import Lending.Types.Pool (AssetInformation)
import TxBuilder.Api qualified as TB
import TxBuilder.Api.BuildTx (buildM)
import TxBuilder.Api.CoinSelection (coinSelection)
import TxBuilder.Api.Types
  ( BuildConstraints
  , UtxoInput (UtxoInput, uiTxIn, uiTxOut)
  , UtxoInputWithDatum (UtxoInputWithDatum)
  )

-- | Extract Persist's entity to `UtxoInputWithDatum`
extractUtxoInput
  :: forall state method
   . (GetUtxo method state, Persist.PersistEntity state)
  => Persist.EntityField state CA.TxIn
  -> Persist.Entity state
  -> Maybe UtxoInput
extractUtxoInput txInField entity = UtxoInput (view txInField entity) . CA.toCtxUTxOTxOut <$> getUtxo entity

-- | Extract Persist's entiry to `UtxoInputWithDatum`
extractUtxoInputWithDatum
  :: forall state datum method
   . (GetUtxo method state, Persist.PersistEntity state)
  => Persist.EntityField state CA.TxIn
  -> Persist.EntityField state (JSONB datum)
  -> Persist.Entity state
  -> Maybe (UtxoInputWithDatum datum)
extractUtxoInputWithDatum txInField datumField entity =
  UtxoInputWithDatum
    <$> extractUtxoInput txInField entity
    <*> pure (unJSONB (view datumField entity))

-- TODO: Confirm this number
dbPoolSize :: Int
dbPoolSize = 5

withDbPoolLending :: (MonadLoggerIO m, MonadUnliftIO m) => ConnectionString -> (ConnectionPool -> m a) -> m a
withDbPoolLending dbConnStr = Persist.withPostgresqlPoolModified setSchema dbConnStr dbPoolSize
  where
    setSchema conn = Monad.void (PostgreSQL.execute_ conn "SET search_path TO public, cardano")

-- | sum 'TxOut' values
sumUtxoValue :: [UtxoInput] -> CA.Value
sumUtxoValue = foldMap (TB.valueInTxOut . TB.uiTxOut)

calculateUtxoValueSatisfyMinAda
  :: CA.ProtocolParameters
  -> CA.AddressInEra CA.BabbageEra
  -> CA.Value
  -> CA.TxOutDatum CA.CtxTx CA.BabbageEra
  -> CA.Value
calculateUtxoValueSatisfyMinAda networkParams changeAddress remainingUserValue datum =
  let CA.TxOut _ txOutValue _ _ =
        TB.adjustMinAdaValue
          networkParams
          ( CA.TxOut
              changeAddress
              (CA.TxOutValue CA.MultiAssetInBabbageEra remainingUserValue)
              datum
              CA.ReferenceScriptNone
          )
   in CA.txOutValueToValue txOutValue

-- | Common sender's constraints that give transaction fee, collateral and change address
toTxSenderConstraints :: [UtxoInput] -> [UtxoInput] -> CA.AddressInEra CA.BabbageEra -> BuildConstraints
toTxSenderConstraints utxoInputs collateral address =
  spendConstraint <> collateralConstraint <> changeAddressConstraint
  where
    -- Currently allow only wallet's utxo
    spendConstraint :: BuildConstraints
    spendConstraint = foldMap TB.mustSpendFromWallet utxoInputs

    collateralConstraint :: BuildConstraints
    collateralConstraint = TB.setCollateral collateral

    changeAddressConstraint :: BuildConstraints
    changeAddressConstraint = TB.setChangeAddr address

queryInputsAndCollaterals :: [CA.TxIn] -> [CA.TxIn] -> CA.ProtocolParameters -> NodeIO ([UtxoInput], [UtxoInput])
queryInputsAndCollaterals inputs collaterals' pParams = do
  let maxCollateralInputs = maybe 3 (fromIntegral . toInteger) (CA.protocolParamMaxCollateralInputs pParams)
      collaterals = take maxCollateralInputs collaterals'
  allUtxos <-
    CA.unUTxO <$> executeShelleyQuery (CA.QueryUTxO (CA.QueryUTxOByTxIn (Set.fromList (inputs <> collaterals))))
  pure (toUtxoInputList inputs allUtxos, toUtxoInputList collaterals allUtxos)
  where
    toUtxoInputList txIns = (uncurry UtxoInput <$>) . Map.toList . Map.filterWithKey (const . (`elem` txIns))

selectUserUtxos
  :: (MonadFail m)
  => CA.Value
  -> AddressText (CA.AddressInEra CA.BabbageEra)
  -> [UtxoInput]
  -> CA.ProtocolParameters
  -> m [UtxoInput]
selectUserUtxos spendingValue address utxos pParams =
  case coinSelection spendingValue (unAddressText address) utxos pParams of
    Right selectedUtxos -> pure selectedUtxos
    Left e -> fail e

queryUserInputsAndCollaterals :: CA.AddressInEra CA.BabbageEra -> NodeIO ([UtxoInput], [UtxoInput])
queryUserInputsAndCollaterals address = do
  (userUtxos, protocolParams) <-
    (,)
      <$> getTxUtxosFromAddress @CA.BabbageEra (Set.singleton $ toAddressAny address)
      <*> executeShelleyQuery @CA.BabbageEra CA.QueryProtocolParameters
  pure (toUtxoInput userUtxos, getCollateralUtxo protocolParams userUtxos)

queryUserInputsAndCollateralBaseOnSpendingValue
  :: CA.AddressInEra CA.BabbageEra
  -> CA.Value
  -> NodeIO ([UtxoInput], [UtxoInput])
queryUserInputsAndCollateralBaseOnSpendingValue address spendingValue = do
  (userUtxos, collateralUtxos) <- queryUserInputsAndCollaterals address
  protocolParams <- executeShelleyQuery @CA.BabbageEra CA.QueryProtocolParameters
  case coinSelection spendingValue address userUtxos protocolParams of
    Right selectedUtxos -> pure (selectedUtxos, collateralUtxos)
    Left e -> fail e -- TODO: throw error type

getTxUtxosFromAddress
  :: (CA.IsShelleyBasedEra era, MonadThrow m, MonadReader NodeConnectInfo m, MonadIO m)
  => Set.Set CA.AddressAny
  -> m (CA.UTxO era)
getTxUtxosFromAddress = executeShelleyQuery . CA.QueryUTxO . CA.QueryUTxOByAddress

getCollateralUtxo :: CA.ProtocolParameters -> CA.UTxO CA.BabbageEra -> [UtxoInput]
getCollateralUtxo pParams =
  take maxCollateralInputs
    . toUtxoInput
    . CA.UTxO
    . Map.filter isAdaOnly
    . CA.unUTxO
  where
    maxCollateralInputs :: Int
    maxCollateralInputs =
      maybe 3 (fromIntegral . toInteger) (CA.protocolParamMaxCollateralInputs pParams)

toUtxoInput :: CA.UTxO CA.BabbageEra -> [UtxoInput]
toUtxoInput (CA.UTxO txMap) = uncurry UtxoInput <$> Map.toList txMap

toWalletUtxo :: CardanoWalletUtxo -> Maybe UtxoInput
toWalletUtxo CardanoWalletUtxo {cardanoWalletUtxoRef, cardanoWalletUtxoUtxo = JSONB utxo} =
  UtxoInput cardanoWalletUtxoRef . CA.toCtxUTxOTxOut <$> fromAnyUtxo utxo

toSlotNo :: CA.ChainPoint -> CA.SlotNo
toSlotNo CA.ChainPointAtGenesis = 0
toSlotNo (CA.ChainPoint slotNo _) = slotNo

posixTimeFromUTCTime :: UTCTime -> POSIXTime
posixTimeFromUTCTime =
  POSIXTime . truncate . (* 1000) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

fromTxOutInlineDatum :: PlutusTx.FromData a => CA.TxOutDatum ctx era -> Maybe a
fromTxOutInlineDatum (CA.TxOutDatumInline _ datum) =
  PlutusTx.fromBuiltinData $ PlutusTx.dataToBuiltinData $ CA.toPlutusData datum
fromTxOutInlineDatum _ = Nothing

utxoAddress :: UtxoInput -> CA.AddressInEra CA.BabbageEra
utxoAddress UtxoInput {uiTxOut = CA.TxOut address _ _ _} = address

txOutAddress :: CA.TxOut ctx era -> CA.AddressInEra era
txOutAddress (CA.TxOut address _ _ _) = address

fromTxOutDatumUtxoToTx :: CA.TxOutDatum CA.CtxUTxO CA.BabbageEra -> CA.TxOutDatum CA.CtxTx CA.BabbageEra
fromTxOutDatumUtxoToTx CA.TxOutDatumNone = CA.TxOutDatumNone
fromTxOutDatumUtxoToTx (CA.TxOutDatumHash s h) = CA.TxOutDatumHash s h
fromTxOutDatumUtxoToTx (CA.TxOutDatumInline s d) = CA.TxOutDatumInline s d

fromTxOutUtxoToTx :: CA.TxOut CA.CtxUTxO CA.BabbageEra -> CA.TxOut CA.CtxTx CA.BabbageEra
fromTxOutUtxoToTx (CA.TxOut a v d s) = CA.TxOut a v (fromTxOutDatumUtxoToTx d) s

loadTextEnvelope :: CA.HasTextEnvelope a => CA.AsType a -> FilePath -> IO a
loadTextEnvelope asType = CA.readFileTextEnvelope asType >=> either CA.throwErrorAsException pure

queryApi :: (MonadIO.MonadIO m) => (ClientError -> IO response) -> ClientEnv -> ClientM response -> m response
queryApi handleError env api =
  MonadIO.liftIO $
    Client.runClientM api env
      >>= either handleError pure

getChangeAddress :: CA.NetworkId -> CA.SigningKey CA.PaymentExtendedKey -> CA.AddressInEra CA.BabbageEra
getChangeAddress networkId =
  flip (CA.makeShelleyAddressInEra networkId) CA.NoStakeAddress
    . CA.PaymentCredentialByKey
    . CA.verificationKeyHash
    . CA.castVerificationKey
    . CA.getVerificationKey

getStakingAddress :: CA.NetworkId -> CA.SigningKey CA.StakeKey -> CA.StakeAddress
getStakingAddress networkId =
  CA.makeStakeAddress networkId
    . CA.StakeCredentialByKey
    . CA.verificationKeyHash
    . CA.getVerificationKey

buildSignAndSubmitTx :: CA.SigningKey CA.PaymentExtendedKey -> BuildConstraints -> NodeIO CA.TxId
buildSignAndSubmitTx deployerSigningKey constraints = do
  networkParams <- getNetworkParams @CA.BabbageEra
  Logger.runStdoutLoggingT $ buildM constraints networkParams (Just 1) >>= signAndSubmitTx deployerSigningKey

toTxInx :: CA.UTxO CA.BabbageEra -> [CA.TxIn]
toTxInx (CA.UTxO txmap) = Map.keys txmap

getUserInputs
  :: forall m
   . (MonadReader NodeConnectInfo m, MonadThrow m, MonadIO m)
  => CA.AddressInEra CA.BabbageEra
  -> m ([CA.TxIn], [CA.TxIn])
getUserInputs address = do
  (userUtxo, protocolParams) <-
    (,)
      <$> getTxUtxosFromAddress @CA.BabbageEra (Set.singleton $ toAddressAny address)
      <*> executeShelleyQuery @CA.BabbageEra CA.QueryProtocolParameters
  let userTxIn = getTxInFromUtxo userUtxo
      collateralTxIn = uiTxIn <$> getCollateralUtxo protocolParams userUtxo
  pure (userTxIn, collateralTxIn)
  where
    getTxInFromUtxo :: CA.UTxO era -> [CA.TxIn]
    getTxInFromUtxo = Map.keys . CA.unUTxO

isAdaOnly :: CA.TxOut ctx era -> Bool
isAdaOnly (CA.TxOut _ (CA.TxOutAdaOnly _ _) _ _) = True
isAdaOnly (CA.TxOut _ (CA.TxOutValue _ value) _ _) = CA.filterValue (CA.AdaAssetId ==) value == value

calculateAmount
  :: Map.Map Asset AssetInformation
  -> Map.Map Asset Receipt
  -> (AssetInformation -> CumulativeRate)
  -> Map.Map Asset Actual
calculateAmount mapAssets assetMap getRate = do
  let listAssets = Map.toList assetMap
      cal (asset, amount) =
        (asset, maybe 0 (flip convertFrom amount . getRate) (Map.lookup asset mapAssets))
   in Map.fromList $ cal <$> listAssets

getAddress :: Maybe (CA.Hash CA.StakeKey) -> CA.ScriptHash -> NodeIO (CA.AddressInEra CA.BabbageEra)
getAddress stakeKey scriptHash = do
  networkId <- Reader.asks CA.localNodeNetworkId
  pure
    ( CA.makeShelleyAddressInEra
        networkId
        (CA.PaymentCredentialByScript scriptHash)
        (maybe CA.NoStakeAddress (CA.StakeAddressByValue . CA.StakeCredentialByKey) stakeKey)
    )

getTxIdFromTxIn :: CA.TxIn -> CA.TxId
getTxIdFromTxIn (CA.TxIn txId _) = txId
