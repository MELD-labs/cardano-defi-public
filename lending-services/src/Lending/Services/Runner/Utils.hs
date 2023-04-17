{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Services.Runner.Utils
  ( queryAllAccountInputs
  , getServiceWalletUtxos
  )
where

import Cardano.Api.Extra.NetworkParams (getNetworkParams, pparams)
import Cardano.Api.Shelley qualified as CA
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Maybe (mapMaybe)
import Database.Persist qualified as Persist
import Plutarch.Extra.AssetClass (AssetClass, fromPlutusAssetClass)
import TxBuilder.Api
  ( UtxoInput (UtxoInput)
  , UtxoInputWithDatum
  , uiTxOut
  )
import TxBuilder.Api qualified as TB
import TxBuilder.Api.CoinSelection (coinSelection)

import Cardano.Index.Extra.WalletUtxo qualified as WalletUtxo
import Lending.Core.Utils (isAdaOnly, toWalletUtxo)
import Lending.Core.Utils qualified as Utils
import Lending.Index.Account qualified as Account
import Lending.Services.AppEnv
  ( AppEnv (aeNodeConnection)
  , AppM
  , SqlM
  , runSqlM
  )
import Lending.Services.Errors
  ( BatchingServiceError (BatchingServiceNoneBatcherUtxosError, BatchingServiceQueryBatcherUtxosCoinSelectionError)
  )
import Lending.Services.Transactions.Utils (extractAccountInput)
import Lending.Types
  ( AccountDatum
  )

getAccountInputs :: SqlM [Persist.Entity Account.Account]
getAccountInputs =
  Persist.selectList
    [ Account.AccountHasRequests Persist.==. True
    , Account.AccountClosingSlotNo Persist.==. Nothing
    ]
    [Persist.Asc Account.AccountRef]

queryAllAccountInputs :: AppM [UtxoInputWithDatum AccountDatum]
queryAllAccountInputs = mapMaybe extractAccountInput <$> runSqlM getAccountInputs

getWalletUtxos :: SqlM [WalletUtxo.CardanoWalletUtxo]
getWalletUtxos =
  (Persist.entityVal <$>)
    <$> Persist.selectList
      [WalletUtxo.CardanoWalletUtxoClosingSlotNo Persist.==. Nothing]
      []

queryWalletUtxos :: AppM [UtxoInput]
queryWalletUtxos = mapMaybe toWalletUtxo <$> runSqlM getWalletUtxos

getNetworkParameters :: AppM TB.NetworkParams
getNetworkParameters = do
  nodeConnection <- Reader.asks aeNodeConnection
  liftIO $ Reader.runReaderT (getNetworkParams @CA.BabbageEra) nodeConnection

getServiceWalletUtxos :: AssetClass -> AppM ([UtxoInput], [UtxoInput])
getServiceWalletUtxos oracleCheckerAsset = do
  utxos <- queryWalletUtxos
  oracleCheckerAssetId <-
    maybe (fail "Unable to parse oracle checker") pure (fromPlutusAssetClass oracleCheckerAsset)
  let CA.Quantity oracleCheckerTokenAmt =
        CA.selectAsset (Utils.sumUtxoValue utxos) oracleCheckerAssetId
  case utxos of
    walletUtxos@(UtxoInput {uiTxOut = CA.TxOut walletAddress _ _ _} : _) -> do
      networkParams <- getNetworkParameters
      let estimateTxFee = CA.lovelaceToValue 5_000_000 -- TODO: estimate this
          batcherSpendingValue =
            if oracleCheckerTokenAmt == 0
              then estimateTxFee
              else estimateTxFee <> CA.valueFromList [(oracleCheckerAssetId, CA.Quantity oracleCheckerTokenAmt)]
          protocolParams = pparams networkParams
          maxCollateralInputs :: Int
          maxCollateralInputs = maybe 3 (fromIntegral . toInteger) (CA.protocolParamMaxCollateralInputs protocolParams)
          collaterals = (take maxCollateralInputs . filter (isAdaOnly . uiTxOut)) walletUtxos
       in case coinSelection
            batcherSpendingValue
            walletAddress
            walletUtxos
            (pparams networkParams) of
            Right selectedWallets -> pure (selectedWallets, collaterals)
            Left s -> Catch.throwM (BatchingServiceQueryBatcherUtxosCoinSelectionError s)
    [] -> Catch.throwM BatchingServiceNoneBatcherUtxosError
