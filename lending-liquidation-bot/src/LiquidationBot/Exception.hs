{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module LiquidationBot.Exception (LiquidationBotException (..)) where

import Cardano.Api qualified as CA
import Control.Exception (Exception)
import Control.Monad.Catch (SomeException)
import Servant.Client (ClientError)

import Cardano.Index.Data.AddressText (AddressText)
import Lending.Core.AccountValue (AccountId)

data LiquidationBotException
  = LiquidationMaxRetryReached String
  | LiquidationQueryApiError ClientError
  | LiquidationNewAccountNotFound AccountId
  | InsufficientAccountCollateral
  | AccountIdNotFound AccountId
  | LiquidateAccountError SomeException
  | CloseAccountError SomeException
  | LiquidationNotFound (AddressText CA.AddressAny) CA.TxIn
  deriving anyclass (Exception)

instance Show LiquidationBotException where
  show (LiquidationMaxRetryReached name) = "Max retries reached: " <> show name
  show (LiquidationQueryApiError e) = "Api query failed with error: " <> show e
  show (LiquidationNewAccountNotFound accId) =
    "Unable to find new account when liquidating account with id " <> show accId
  show InsufficientAccountCollateral = "Account's collateral is not enough, please decrease the repaying debt"
  show (AccountIdNotFound accId) = "Cannot find account with id " <> show accId
  show (LiquidateAccountError e) = "Can not Liquidate Account: " <> show e
  show (CloseAccountError e) = "Can not Close Account: " <> show e
  show (LiquidationNotFound addr txin) =
    "Can not find Liquidation of address: "
      <> show addr
      <> ", Liquidated Account Ref: "
      <> show txin
