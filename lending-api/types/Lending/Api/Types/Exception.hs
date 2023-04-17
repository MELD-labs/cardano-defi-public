{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Api.Types.Exception (ServerError (..), UserError (..)) where

import Cardano.Api qualified as CA
import Control.Exception (Exception (fromException, toException))
import Data.Map (Map)
import Ouroboros.Consensus.HardFork.History.Qry qualified as Qry

import Cardano.Api.Extra.AssetId (AssetIdText)
import Cardano.Index.Data.AddressText (AddressText)
import Data.Typeable (cast)
import Lending.Core.Api (AnyServerError (AnyServerError))
import Lending.Core.Errors (ScriptError)
import Lending.Index.Account qualified as IA
import Lending.Scripts (LendingScript)
import Lending.Types (AccountRedeemer)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, Receipt)
import Plutarch.Extra.AssetClass (AssetClass)

data ServerError
  = UnableToDeserializeOwnerNftScript
  | InvalidScriptNft LendingScript AssetClass
  | UnableToGetCurrentSlot Qry.PastHorizonException
  | InvalidScript ScriptError
  | UnableToParseUserNft AssetClass
  | ProcessingAccountNotFound AssetClass
  | InvalidRedeemerHistory IA.Account
  | NotSupportClosingRedeemerHistoryAccount AccountRedeemer
  | NotSupportRedeemerHistoryAccount AccountRedeemer IA.Account
  | InvalidClosingTime IA.Account
  | InvalidClosingRef IA.Account
  | InvalidTimeTx IA.Account

instance Exception ServerError where
  fromException e = do
    AnyServerError a <- fromException e
    cast a
  toException = toException . AnyServerError

instance Show ServerError where
  show UnableToDeserializeOwnerNftScript = "Unable to deserialize owner NFT script"
  show (InvalidScriptNft name assetId) = "Invalid " <> show name <> " NFT: " <> show assetId
  show (UnableToGetCurrentSlot time) = "Unable to get current slot: " <> show time
  show (InvalidScript e) = "Invalid script: " <> show e
  show (UnableToParseUserNft assetId) = "Unable to parse owner NFT: " <> show assetId
  show (ProcessingAccountNotFound assetId) = "Cannot find processing account: " <> show assetId
  show (InvalidRedeemerHistory accountData) =
    "Unable to get history accountId because of empty redeemer : " <> show accountData
  show (NotSupportClosingRedeemerHistoryAccount closingRedeemer) =
    "Do not support closing redeemer `"
      <> show closingRedeemer
      <> "` when we get history account"
  show (NotSupportRedeemerHistoryAccount accountRedeemer account) =
    "Do not support redeemer `"
      <> show accountRedeemer
      <> "` when we get history with account: "
      <> show account
  show (InvalidClosingTime accountData) =
    "Unable to get history accountId because of empty closing time : " <> show accountData
  show (InvalidClosingRef accountData) =
    "Unable to get history accountId because of empty closing ref : " <> show accountData
  show (InvalidTimeTx accountData) =
    "Unable to get history accountId because of empty time tx : " <> show accountData

data UserError
  = ExceedLoanToValue
  | InsufficientValueAccount (Map Asset Actual) (Map Asset Actual)
  | TooManyUtxoInRequest Int
  | UserUtxoNotFound (AddressText (CA.AddressInEra CA.BabbageEra))
  | UnclosableAccount (Map Asset Receipt) (Map Asset Receipt)
  | AccountOwnerNftNotFound AssetIdText
  | UnableToDecodeAsset Asset
  | NotWhiteListedAsset Asset
  | BalanceTxError CA.TxBodyErrorAutoBalance
  | InvalidAccountOwnerNft AssetClass
  deriving anyclass (Exception)

instance Show UserError where
  show ExceedLoanToValue = "Exceed loan-to-value account"
  show (InsufficientValueAccount supply borrow) =
    "Insufficient value account: " <> "\nSupply value: " <> show supply <> "\nBorrow value: " <> show borrow
  show (TooManyUtxoInRequest number) = "Too many UTXOs in request: " <> show number
  show (UserUtxoNotFound addr) = "Must spend at least 1 UTXO to build transaction: " <> show addr
  show (UnclosableAccount supplyMap borrowMap) =
    "Account still has supplied or borrowed amount: "
      <> "\n Supply map: "
      <> show supplyMap
      <> "\n Borrow map: "
      <> show borrowMap
  show (AccountOwnerNftNotFound aId) = "Can't find owner NFT in tx inputs: " <> show aId
  show (UnableToDecodeAsset asset) = "Unable to decode asset with asset id: " <> show asset
  show (NotWhiteListedAsset asset) = show asset <> " is not whitelisted asset in protocol"
  show (BalanceTxError msg) = "Balance tx error: " <> show msg
  show (InvalidAccountOwnerNft ac) = "Can't decode owner nft of account: " <> show ac
