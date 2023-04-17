{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module Lending.Oracle.Api.Exceptions (OracleServiceException (..)) where

import Cardano.Api qualified as CA
import Control.Exception (Exception)
import Servant.Client (ClientError)

import Lending.Types.Asset (Asset)

data OracleServiceException
  = UnableToDecodeAssetOracleService Asset
  | ServiceOracleBuildTxError CA.TxBodyErrorAutoBalance
  | ServiceOracleCmcApiError ClientError
  deriving anyclass (Exception)

instance Show OracleServiceException where
  show :: OracleServiceException -> String
  show (UnableToDecodeAssetOracleService asset) =
    "Can not decode asset with value: " <> show asset
  show (ServiceOracleBuildTxError err) =
    "Can not build transaction for updating oracle with error message: " <> show err
  show (ServiceOracleCmcApiError err) =
    "Can not query asset price from CoinMarketCap with error message: " <> show err
