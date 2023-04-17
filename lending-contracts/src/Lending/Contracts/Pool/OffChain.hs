{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Contracts.Pool.OffChain (processAccountOffChain, updatePoolOffChain) where

import Plutarch.Api.V2 (AmountGuarantees (Positive), KeyGuarantees (Sorted), PMap, PTxOut, PValue)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.Extra.AssetClass (PAssetClassData, ptoScottEncoding)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Lift (PConstantDecl, PLifted, PUnsafeLiftDecl)
import Plutarch.Monadic qualified as P
import Ply.Plutarch (PlyArgOf)

import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Exchange (PPrice, PReceipt)
import Lending.Contracts.Manager.Types (PRiskParameters)
import Lending.Contracts.Pool.Account
  ( MatcherContext (MatcherContext, mcAccountAuthToken, mcAssetInfoMap, mcMinAdaUtxo, mcPriceMap, mcRiskParameters)
  , RequestResult (RequestResult, rrAccountStateChange, rrOutputs)
  , StateChange (StateChange, scBorrow, scPoolPayable, scPoolReceivable, scSupply)
  , processAccount
  )
import Lending.Contracts.Pool.OnChain (updatePool)
import Lending.Contracts.Pool.Types (PAssetInformation)
import Lending.Types.Pool.OffChain (MatcherContextData, RequestResultData, StateChangeData)

newtype PMatcherContextData (s :: S)
  = PMatcherContextData
      ( Term
          s
          ( PDataRecord
              '[ "mcdAssetPrices" ':= PMap 'Sorted PAsset PPrice
               , "mcdAssets" ':= PMap 'Sorted PAsset PAssetInformation
               , "mcdRiskParameters" ':= PMap 'Sorted PAsset PRiskParameters
               , "mcdMinAdaUtxo" ':= PInteger
               , "mcdAccountAuthToken" ':= PAssetClassData
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PMatcherContextData where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PMatcherContextData where type PLifted PMatcherContextData = MatcherContextData
deriving via (DerivePConstantViaData MatcherContextData PMatcherContextData) instance PConstantDecl MatcherContextData
type instance PlyArgOf PMatcherContextData = MatcherContextData

newtype PStateChangeData (s :: S)
  = PStateChangeData
      ( Term
          s
          ( PDataRecord
              '[ "scdSupply" ':= PMap 'Sorted PAsset PReceipt
               , "scdBorrow" ':= PMap 'Sorted PAsset PReceipt
               , "scdPoolReceivable" ':= PValue 'Sorted 'Positive
               , "scdPoolPayable" ':= PValue 'Sorted 'Positive
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PStateChangeData where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PStateChangeData where type PLifted PStateChangeData = StateChangeData
deriving via (DerivePConstantViaData StateChangeData PStateChangeData) instance PConstantDecl StateChangeData
type instance PlyArgOf PStateChangeData = StateChangeData

newtype PRequestResultData (s :: S)
  = PRequestResultData
      ( Term
          s
          ( PDataRecord
              '[ "rrdOutputs" ':= PBuiltinList PTxOut
               , "rrdAccountStateChange" ':= PStateChangeData
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PRequestResultData where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PRequestResultData where type PLifted PRequestResultData = RequestResultData
deriving via (DerivePConstantViaData RequestResultData PRequestResultData) instance PConstantDecl RequestResultData
type instance PlyArgOf PRequestResultData = RequestResultData

toMatcherContext :: Term s (PMatcherContextData :--> MatcherContext)
toMatcherContext = phoistAcyclic $ plam $ \ctxData' -> P.do
  ctxData <- pletAll ctxData'
  pcon $
    MatcherContext
      { mcPriceMap = ctxData.mcdAssetPrices
      , mcAssetInfoMap = ctxData.mcdAssets
      , mcRiskParameters = ctxData.mcdRiskParameters
      , mcMinAdaUtxo = ctxData.mcdMinAdaUtxo
      , mcAccountAuthToken = ptoScottEncoding # ctxData.mcdAccountAuthToken
      }

toStateChange :: Term s (PStateChangeData :--> StateChange)
toStateChange = phoistAcyclic $ plam $ \stateChangeData' -> P.do
  stateChangeData <- pletAll stateChangeData'
  pcon $
    StateChange
      { scSupply = stateChangeData.scdSupply
      , scBorrow = stateChangeData.scdBorrow
      , scPoolReceivable = stateChangeData.scdPoolReceivable
      , scPoolPayable = stateChangeData.scdPoolPayable
      }

fromStateChange :: Term s (StateChange :--> PStateChangeData)
fromStateChange = phoistAcyclic $ plam $ \stateChange -> P.do
  StateChange {scSupply, scBorrow, scPoolReceivable, scPoolPayable} <- pmatch stateChange
  mkRecordConstr PStateChangeData $
    #scdSupply .= pdata scSupply
      .& #scdBorrow .= pdata scBorrow
      .& #scdPoolReceivable .= pdata scPoolReceivable
      .& #scdPoolPayable .= pdata scPoolPayable

fromRequestResult :: Term s (RequestResult :--> PRequestResultData)
fromRequestResult = phoistAcyclic $ plam $ \requestResult -> P.do
  RequestResult {rrOutputs, rrAccountStateChange} <- pmatch requestResult
  mkRecordConstr PRequestResultData $
    #rrdOutputs .= pdata rrOutputs
      .& #rrdAccountStateChange .= pdata (fromStateChange # rrAccountStateChange)

processAccountOffChain :: Term s (PMatcherContextData :--> PTxOut :--> PRequestResultData)
processAccountOffChain = phoistAcyclic $ plam $ \ctxData output ->
  fromRequestResult #$ processAccount # (toMatcherContext # ctxData) # output

updatePoolOffChain
  :: Term
      s
      ( PMap 'Sorted PAsset PRiskParameters
          :--> PStateChangeData
          :--> PInteger
          :--> PMap 'Sorted PAsset PAssetInformation
          :--> PMap 'Sorted PAsset PAssetInformation
      )
updatePoolOffChain = phoistAcyclic $ plam $ \riskMap stateChange -> updatePool # riskMap # (toStateChange # stateChange)
