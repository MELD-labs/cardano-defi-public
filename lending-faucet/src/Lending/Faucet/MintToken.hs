{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Faucet.MintToken (mintTokenApi) where

import Cardano.Api qualified as CA
import Control.Arrow ((&&&), (***))
import Control.Concurrent qualified as Concurrent
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.State (StateT (StateT))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Tuple.Extra (both)
import Servant (ServerT)
import System.Random qualified as Random
import System.Random.Shuffle qualified as Random

import Cardano.Api.Extra.NetworkParams (NetworkParams (NetworkParams, pparams))
import Cardano.Api.Extra.Nft (AssetNameReadable (unAssetNameReadable), getNftMetadata)
import Cardano.Api.Extra.Query (executeQueryInMode)
import Cardano.Api.Extra.Tx (signAndSubmitTx)
import Cardano.Index.Data.AddressText (AddressText (unAddressText))
import Lending.Core.Utils (getChangeAddress, queryInputsAndCollaterals, toSlotNo)
import Lending.Core.Utils qualified as Utils
import Lending.Faucet.Common (reloadFaucetUtxos, toValue)
import Lending.Faucet.Env
  ( AppEnv
      ( AppEnv
      , aeFaucetConfig
      , aeNetworkParams
      , aeNodeConnection
      , aeOperatorSigningKey
      )
  , AppM
  , FaucetConfig
    ( FaucetConfig
    , fcCollateral
    , fcMint
    , fcNft
    , fcNumberOfTokensPerUser
    , fcSpent
    )
  )
import Lending.Faucet.Types
  ( MintTokenApi
  , MintTokenRequest (MintTokenRequest, mtUserAddress)
  , MintTokenResponse (MintTokenResponse)
  )
import Plutarch.Extra.FixedDecimal (fixedNumerator)
import TxBuilder.Api
  ( BuildConstraints
  , WitnessScript (AttachedScript)
  , buildM
  , mustMintValueWithSimpleScript
  , mustPayTo
  , mustValidateIn
  , setMetadata
  )

mintTokenApi :: ServerT MintTokenApi AppM
mintTokenApi = mintTokenH

getFungibleScript :: CA.SigningKey CA.PaymentExtendedKey -> CA.SimpleScript CA.SimpleScriptV2
getFungibleScript =
  CA.RequireSignature
    . CA.verificationKeyHash
    . CA.castVerificationKey
    . CA.getVerificationKey

getNftScript :: CA.SigningKey CA.PaymentExtendedKey -> CA.SlotNo -> CA.SimpleScript CA.SimpleScriptV2
getNftScript signingKey validityLowerBound =
  CA.RequireAllOf [getFungibleScript signingKey, CA.RequireTimeAfter CA.TimeLocksInSimpleScriptV2 validityLowerBound]

mintScript :: CA.SimpleScript CA.SimpleScriptV2 -> Map CA.AssetName Integer -> (CA.Value, BuildConstraints)
mintScript script =
  Map.foldMapWithKey (curry (CA.valueFromList . pure . (CA.AssetId policyId *** CA.Quantity)))
    &&& Map.foldMapWithKey mintTokenName
  where
    policyId :: CA.PolicyId
    policyId = CA.scriptPolicyId (CA.SimpleScript CA.SimpleScriptV2 script)

    mintTokenName :: CA.AssetName -> Integer -> BuildConstraints
    mintTokenName tokenName amount =
      mustMintValueWithSimpleScript (CA.AssetId policyId tokenName) amount (AttachedScript CA.SimpleScriptV2 script)

mintTokenH :: MintTokenRequest -> AppM MintTokenResponse
mintTokenH MintTokenRequest {mtUserAddress} = do
  AppEnv
    { aeNetworkParams
    , aeFaucetConfig =
      FaucetConfig
        { fcMint
        , fcSpent
        , fcCollateral
        , fcNumberOfTokensPerUser
        , fcNft
        }
    } <-
    Reader.ask
  networkParams@NetworkParams {pparams} <- MonadIO.liftIO $ Concurrent.readMVar aeNetworkParams
  faucetSigningKey <- Reader.asks aeOperatorSigningKey
  faucetAddress <- Reader.asks (flip getChangeAddress faucetSigningKey . CA.localNodeNetworkId . aeNodeConnection)
  txInFaucetInUsed <- reloadFaucetUtxos (StateT List.uncons)
  Logger.logInfoN $ "Faucet is using TxIn: " <> Text.pack (show txInFaucetInUsed)
  finalConstraints <-
    Reader.withReaderT aeNodeConnection $
      Reader.mapReaderT MonadIO.liftIO $ do
        (faucetUtxo, collateralUtxo) <- queryInputsAndCollaterals [txInFaucetInUsed] [] pparams
        validityLowerBound <- toSlotNo <$> executeQueryInMode (CA.QueryChainPoint CA.CardanoMode)
        mintingTokens <-
          Map.mapKeys unAssetNameReadable . Map.fromList . take fcNumberOfTokensPerUser
            <$> Random.shuffleM (Map.toList fcMint)
        fungibleAmountMap <- traverse (Random.randomRIO . both fixedNumerator) mintingTokens
        let fungibleScript = getFungibleScript faucetSigningKey
            nftScript = getNftScript faucetSigningKey validityLowerBound
            nftMap = Map.mapKeys unAssetNameReadable fcNft
        metadata <-
          either
            (MonadIO.liftIO . CA.throwErrorAsException)
            (pure . CA.TxMetadataInEra CA.TxMetadataInBabbageEra)
            (getNftMetadata (Map.singleton (CA.scriptPolicyId (CA.SimpleScript CA.SimpleScriptV2 nftScript)) nftMap))
        let spendConstraint = Utils.toTxSenderConstraints faucetUtxo collateralUtxo faucetAddress

            spentValue = CA.valueFromList (toValue fcSpent)
            (fungibleMintValue, fungibleMintConstraint) = mintScript fungibleScript fungibleAmountMap
            (nftMintValue, nftMintConstraint) = mintScript nftScript (1 <$ nftMap)
            userValue = fungibleMintValue <> nftMintValue <> spentValue
            payToUserConstraint = mustPayTo (unAddressText mtUserAddress) userValue CA.TxOutDatumNone

            collateralValue = CA.lovelaceToValue (CA.Lovelace (fixedNumerator fcCollateral))
            payToUserCollateralConstraint = mustPayTo (unAddressText mtUserAddress) collateralValue CA.TxOutDatumNone

            metadataConstraint = setMetadata metadata

            validityConstraint =
              mustValidateIn
                ( CA.TxValidityLowerBound CA.ValidityLowerBoundInBabbageEra validityLowerBound
                , CA.TxValidityNoUpperBound CA.ValidityNoUpperBoundInBabbageEra
                )
        pure $
          spendConstraint
            <> fungibleMintConstraint
            <> nftMintConstraint
            <> payToUserConstraint
            <> payToUserCollateralConstraint
            <> metadataConstraint
            <> validityConstraint
  txId <-
    Reader.withReaderT aeNodeConnection $
      buildM finalConstraints networkParams (Just 1) >>= signAndSubmitTx faucetSigningKey
  Logger.logInfoN $ "Faucet funds for user with tx: " <> Text.pack (show txId)
  pure $ MintTokenResponse txId
