{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Services.Transactions.Batcher (batchingConstraints) where

import Cardano.Api qualified as CA
import Control.Monad.Logger qualified as Logger

import Lending.Core.Errors (maybeE)
import Lending.Core.Utils (sumUtxoValue)
import Lending.Services.AppEnv (AppM)
import Lending.Services.Errors
  ( BatchingServiceError (BatchingServiceParseOracleCheckerError)
  )
import Lending.Services.Transactions.Process (processPoolAndAccounts)
import Lending.Services.Transactions.Types
  ( BatchingInput
      ( BatchingInput
      , biBatcherChangeAddress
      , biBatcherCollateralUtxos
      , biBatcherWalletUtxos
      , biManagerInput
      , biOracleCheckerScriptRefUtxo
      , biOracleInput
      , biValidityRange
      )
  )
import Lending.Types.Manager (ManagerDatum (ManagerDatum, mdOracleCheckerToken))
import Lending.Types.Oracle (OracleDatum (OracleDatum, odAssetPrices))
import Lending.Types.OracleCheckerToken (OracleCheckerTokenRedeemer (OracleCheckerTokenRedeemer))
import Plutarch.Extra.AssetClass (fromPlutusAssetClass)
import TxBuilder.Api
  ( BuildConstraints
  , UtxoInputWithDatum (uiwdDatum)
  , WitnessScript (RefScript)
  , mustMintValue
  , mustPayTo
  , mustReferenceInput
  , mustSpendFromWallet
  , mustValidateIn
  , setChangeAddr
  , setCollateral
  )
import TxBuilder.Api.Types (UtxoInputWithDatum (UtxoInputWithDatum, uiwdUtxo))

constraintOfReferenceManager :: BatchingInput -> BuildConstraints
constraintOfReferenceManager BatchingInput {biManagerInput} =
  let UtxoInputWithDatum {uiwdUtxo = managerUtxo} = biManagerInput
   in mustReferenceInput managerUtxo

constraintOfReferenceOracle :: BatchingInput -> BuildConstraints
constraintOfReferenceOracle BatchingInput {biOracleInput} =
  let UtxoInputWithDatum {uiwdUtxo = oracleUtxo} = biOracleInput
   in mustReferenceInput oracleUtxo

spendBatcherInputsAndMintOracleCheckerToken :: BatchingInput -> AppM BuildConstraints
spendBatcherInputsAndMintOracleCheckerToken
  BatchingInput
    { biManagerInput = UtxoInputWithDatum {uiwdDatum = ManagerDatum {mdOracleCheckerToken}}
    , biOracleInput = UtxoInputWithDatum {uiwdDatum = OracleDatum {odAssetPrices}}
    , biBatcherWalletUtxos
    , biOracleCheckerScriptRefUtxo
    , biBatcherChangeAddress
    } =
    do
      Logger.logInfoN "Build BuildConstraints for spending batcher's inputs and minting oracle checker token"
      oracleCheckerAssetId <-
        maybeE
          (BatchingServiceParseOracleCheckerError mdOracleCheckerToken)
          (fromPlutusAssetClass mdOracleCheckerToken)
      let CA.Quantity oracleCheckerTokenAmt =
            CA.selectAsset (sumUtxoValue biBatcherWalletUtxos) oracleCheckerAssetId
          (mintAmt, payOctTokenToBatcher) =
            if oracleCheckerTokenAmt == 0
              then
                ( 1
                , mustPayTo
                    biBatcherChangeAddress
                    (CA.valueFromList [(oracleCheckerAssetId, CA.Quantity 1), (CA.AdaAssetId, 2_000_000)])
                    CA.TxOutDatumNone
                )
              else (negate oracleCheckerTokenAmt, mempty)
          octScript = RefScript CA.PlutusScriptV2 biOracleCheckerScriptRefUtxo
          oracleCheckerMintingConstraints =
            mustMintValue oracleCheckerAssetId mintAmt octScript (OracleCheckerTokenRedeemer odAssetPrices)
      pure $ oracleCheckerMintingConstraints <> payOctTokenToBatcher

setCollateralConstraint batchingInp =
  return $ setCollateral $ biBatcherCollateralUtxos batchingInp

batchingConstraints :: BatchingInput -> AppM BuildConstraints
batchingConstraints batchingInp = do
  poolAndAccountConstraints <- processPoolAndAccounts batchingInp
  oracleCheckerConstraints <- spendBatcherInputsAndMintOracleCheckerToken batchingInp
  collateralConstraint <- setCollateralConstraint batchingInp
  pure $
    poolAndAccountConstraints
      <> constraintOfReferenceManager batchingInp
      <> constraintOfReferenceOracle batchingInp
      <> oracleCheckerConstraints
      <> foldMap mustSpendFromWallet (biBatcherWalletUtxos batchingInp)
      <> setChangeAddr (biBatcherChangeAddress batchingInp)
      <> collateralConstraint
      <> mustValidateIn (biValidityRange batchingInp)
