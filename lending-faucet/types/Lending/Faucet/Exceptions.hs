{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module Lending.Faucet.Exceptions (FaucetException (..)) where

import Cardano.Api qualified as CA
import Control.Exception (Exception)
import Plutarch.Extra.AssetClass (AssetClass)

data FaucetException
  = NotEnoughUtxoForUserRequest
  | FaucetMinUtxoConstraintError CA.MinimumUTxOError
  | NotHaveTxInError [CA.TxIn]
  | ExtractFaucetAssetClassError AssetClass
  deriving anyclass (Exception)

instance Show FaucetException where
  show :: FaucetException -> String
  show NotEnoughUtxoForUserRequest =
    show "Faucet's wallet does not have enough UTxO for request"
  show (FaucetMinUtxoConstraintError faucetMinUtxoError) =
    "Can not payback for faucet's wallet because of min ADA with error: " <> show faucetMinUtxoError
  show (NotHaveTxInError txIn) =
    "Faucet's wallet does not tx inputs: " <> show txIn
  show (ExtractFaucetAssetClassError assetClass) =
    "Can not extract asset class from faucet configuration with assetClass: " <> show assetClass
