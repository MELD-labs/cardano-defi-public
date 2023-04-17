{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Services.Transactions.Process (processPoolAndAccounts) where

import Cardano.Api qualified as CA
import Control.Monad qualified as Monad
import Control.Monad.Extra qualified as Monad
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.Resource qualified as Catch
import Data.List qualified as List
import PlutusTx qualified
import PlutusTx.Monoid qualified as PlutusTx

import Cardano.Api.Extra.Adapters (toCardanoTxOut, toCardanoValue)
import Lending.Core.AccountValue (fillNewAssets, getDeltaTime, tryProcessAccount, tryUpdateInterestRate, tryUpdatePool)
import Lending.Services.AppEnv (AppEnv (AppEnv, aeContractsConfig, aeNetworkId), AppM)
import Lending.Services.Config
  ( ServiceContractsConfig
      ( ServiceContractsConfig
      , sccLendingFunctionInterestRate
      , sccLendingFunctionProcessAccount
      , sccLendingFunctionUpdatePool
      )
  )
import Lending.Services.Errors (BatchingServiceError (BatchingServicePoolValueError))
import Lending.Services.Transactions.Types
  ( BatchingInput
      ( BatchingInput
      , biAccountInputs
      , biAccountScriptRefUtxo
      , biManagerInput
      , biOracleInput
      , biPoolInput
      , biPoolScriptRefUtxo
      , biUpdatedTime
      )
  )
import Lending.Types.Account
  ( AccountRedeemer (AccountApplyRedeemer)
  )
import Lending.Types.Manager
  ( GlobalRiskParameters (grpMinAdaUtxo)
  , ManagerDatum (ManagerDatum, mdAccountAuthToken, mdGlobalRiskParameters, mdRiskParameters)
  )
import Lending.Types.Oracle (OracleDatum (OracleDatum, odAssetPrices))
import Lending.Types.Pool
  ( PoolDatum (PoolDatum, pdAssets, pdLastUpdatedTime)
  , PoolRedeemer (UpdatePoolRedeemer)
  )
import Lending.Types.Pool.OffChain
  ( MatcherContextData
      ( MatcherContextData
      , mcdAccountAuthToken
      , mcdAssetPrices
      , mcdAssets
      , mcdMinAdaUtxo
      , mcdRiskParameters
      )
  , RequestResultData (RequestResultData, rrdAccountStateChange, rrdOutputs)
  , StateChangeData (StateChangeData, scdPoolPayable, scdPoolReceivable)
  )
import TxBuilder.Api
  ( BuildConstraints
  , UtxoInput (UtxoInput, uiTxIn, uiTxOut)
  , UtxoInputWithDatum (UtxoInputWithDatum, uiwdDatum, uiwdUtxo)
  , WitnessScript (RefScript)
  , mustPayTo
  , mustSpendFromScript
  , toTxOutInlineDatum
  )

spendTxOut :: PlutusTx.ToData rdm => UtxoInput -> rdm -> UtxoInput -> BuildConstraints
spendTxOut scriptRefUtxo redeemer input =
  mustSpendFromScript input (RefScript CA.PlutusScriptV2 scriptRefUtxo) CA.InlineScriptDatum redeemer

payTxOut :: CA.TxOut CA.CtxTx CA.BabbageEra -> BuildConstraints
payTxOut (CA.TxOut address value datum _) = mustPayTo address (CA.txOutValueToValue value) datum

getPoolConstraint :: UtxoInput -> UtxoInput -> PoolDatum -> StateChangeData -> AppM BuildConstraints
getPoolConstraint
  poolScriptRefUtxo
  poolInput@UtxoInput {uiTxOut = (CA.TxOut address inputValue _ _)}
  newDatum
  StateChangeData {scdPoolReceivable, scdPoolPayable} =
    do
      Logger.logInfoN "Build spend and pay Pool constraints"
      netValueChange <- either Catch.throwM pure (toCardanoValue (scdPoolReceivable <> PlutusTx.inv scdPoolPayable))
      let newValue = CA.txOutValueToValue inputValue <> netValueChange
      Monad.unless (all ((>= 0) . snd) $ CA.valueToList newValue) $
        Catch.throwM (BatchingServicePoolValueError newValue)
      let payToPoolConstraint = mustPayTo address newValue (toTxOutInlineDatum newDatum)
          spendPoolConstraint = spendTxOut poolScriptRefUtxo UpdatePoolRedeemer poolInput
      pure (spendPoolConstraint <> payToPoolConstraint)

processPoolAndAccounts :: BatchingInput -> AppM BuildConstraints
processPoolAndAccounts
  BatchingInput
    { biManagerInput =
      UtxoInputWithDatum
        { uiwdDatum = ManagerDatum {mdRiskParameters, mdGlobalRiskParameters, mdAccountAuthToken}
        }
    , biOracleInput = UtxoInputWithDatum {uiwdDatum = OracleDatum {odAssetPrices}}
    , biPoolInput =
      UtxoInputWithDatum
        { uiwdUtxo = poolInput
        , uiwdDatum = PoolDatum {pdAssets, pdLastUpdatedTime = oldTime}
        }
    , biAccountInputs
    , biPoolScriptRefUtxo
    , biAccountScriptRefUtxo
    , biUpdatedTime
    } =
    do
      Logger.logInfoN "Build BuildConstraints for processing Pool and Accounts"
      AppEnv
        { aeContractsConfig =
          ServiceContractsConfig
            { sccLendingFunctionInterestRate
            , sccLendingFunctionProcessAccount
            , sccLendingFunctionUpdatePool
            }
        } <-
        Reader.ask
      let deltaTime = getDeltaTime oldTime biUpdatedTime
          accounts = List.sortOn (uiTxIn . uiwdUtxo) biAccountInputs
      networkId <- Reader.asks aeNetworkId
      interestRateUpdatedAssetMap <-
        fillNewAssets (uiwdDatum <$> accounts)
          <$> tryUpdateInterestRate sccLendingFunctionInterestRate mdRiskParameters pdAssets deltaTime
      let ctx =
            MatcherContextData
              { mcdAssetPrices = odAssetPrices
              , mcdAssets = interestRateUpdatedAssetMap
              , mcdRiskParameters = mdRiskParameters
              , mcdMinAdaUtxo = grpMinAdaUtxo mdGlobalRiskParameters
              , mcdAccountAuthToken = mdAccountAuthToken
              }

      Logger.logInfoN "Try to process accounts"
      RequestResultData {rrdOutputs, rrdAccountStateChange} <-
        Monad.mconcatMapM (tryProcessAccount sccLendingFunctionProcessAccount ctx . uiTxOut . uiwdUtxo) accounts
      accountAndUserOutputs <- traverse (either Catch.throwM pure . toCardanoTxOut networkId) rrdOutputs
      newAssetMap <-
        tryUpdatePool sccLendingFunctionUpdatePool mdRiskParameters rrdAccountStateChange deltaTime pdAssets
      let spendAccountConstraints =
            foldMap (spendTxOut biAccountScriptRefUtxo AccountApplyRedeemer . uiwdUtxo) accounts
          payToAccountConstraints = foldMap payTxOut accountAndUserOutputs
          accountConstraints = spendAccountConstraints <> payToAccountConstraints
          newPoolDatum = PoolDatum newAssetMap biUpdatedTime
      poolConstraint <- getPoolConstraint biPoolScriptRefUtxo poolInput newPoolDatum rrdAccountStateChange
      pure (poolConstraint <> accountConstraints)
