{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lending.Contracts.Pool.OnChain
  ( validatePoolTerm
  , updatePool
  )
where

import Plutarch.Api.V1.Time (PPOSIXTime (PPOSIXTime))
import Plutarch.Api.V2
  ( AmountGuarantees (Positive)
  , KeyGuarantees (Sorted)
  , PAddress
  , PDatum (PDatum)
  , PExtended (PFinite)
  , PInterval (PInterval)
  , PLowerBound (PLowerBound)
  , PMap (PMap)
  , POutputDatum (POutputDatum)
  , PPOSIXTimeRange
  , PUpperBound (PUpperBound)
  , PValidator
  , PValue
  )

import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Api.V2.Tx (PTxOut)
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Extra.AssetClass (AssetClass, PAssetClassData, ptoScottEncoding)
import Plutarch.Extra.Bool (passert)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.Function (pconst)
import Plutarch.Extra.List (pmapMaybe)
import Plutarch.Extra.Map qualified as AssocMap
import Plutarch.Extra.Maybe (pjust, pnothing)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.ScriptContext (ptryOwnInput)
import Plutarch.Extra.Value (phasOneTokenOfAssetClass)
import Plutarch.Monadic qualified as P

import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Common
  ( findAccountTxIns
  , getOracleAssetPrices
  , getState
  , pmapWithKey
  , ptryFromData
  , uniqueAssetFromTxInInfo
  )
import Lending.Contracts.Exchange (PActual, PCumulativeRate, PPrice, PReceipt, convertFrom)
import Lending.Contracts.Manager.Types (PManagerDatum, PRiskParameters)
import Lending.Contracts.Migrate (validateMigrate)
import Lending.Contracts.Pool.Account
  ( MatcherContext
      ( MatcherContext
      , mcAccountAuthToken
      , mcAssetInfoMap
      , mcMinAdaUtxo
      , mcPriceMap
      , mcRiskParameters
      )
  , StateChange (StateChange, scBorrow, scPoolPayable, scPoolReceivable, scSupply)
  , getPoolChange
  )
import Lending.Contracts.Pool.InterestRate (updateCumulatedRate)
import Lending.Contracts.Pool.Types
  ( PAssetInformation (PAssetInformation)
  , PPoolDatum
  , PPoolRedeemer (PMigratePoolRedeemer, PUpdatePoolRedeemer, PUpdateTreasuryPoolRedeemer)
  )
import Lending.Contracts.Pool.Withdraw (validateUpdateTreasuryPool)

validatePool
  :: Term
      s
      ( PAssetClassData
          :--> PAssetClassData
          :--> PPoolDatum
          :--> PPoolRedeemer
          :--> PScriptContext
          :--> PBool
      )
validatePool = phoistAcyclic $ plam $ \managerAuthToken migrationNft datum redeemer ctx' -> P.do
  pmatch redeemer $ \case
    PUpdatePoolRedeemer _ -> validateUpdatePool # managerAuthToken # datum # ctx'
    PMigratePoolRedeemer _ -> validateMigrate # migrationNft # ctx'
    PUpdateTreasuryPoolRedeemer _ -> validateUpdateTreasuryPool # managerAuthToken # datum # ctx'

validateUpdatePool
  :: Term
      s
      ( PAssetClassData
          :--> PPoolDatum
          :--> PScriptContext
          :--> PBool
      )
validateUpdatePool = phoistAcyclic $ plam $ \managerAuthToken poolInputDatum ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  txinfo <- pletFields @["inputs", "referenceInputs", "outputs", "validRange", "redeemers"] ctx.txInfo
  refInputs <- plet $ txinfo.referenceInputs
  inputs <- plet $ txinfo.inputs
  outputs <- plet $ txinfo.outputs
  validRange <- plet $ txinfo.validRange
  manager' <- plet $ getState @PManagerDatum # managerAuthToken # refInputs -- ###### 39M
  manager <- pletAll manager'
  oracleAssetPrice <- plet $ getOracleAssetPrices # txinfo.redeemers # manager.mdOracleCheckerToken
  accountAuthAsset <- plet $ manager.mdAccountAuthToken
  poolNft <- plet $ manager.mdPoolNft
  accountTxIns <- plet $ findAccountTxIns # inputs # accountAuthAsset -- ###### 25M
  -- TODO: Choosing supply requests before withdraw requests
  unresolvedPoolInput <- plet $ ptryOwnInput # ctx'
  resolvedPoolInput <- plet $ pfield @"resolved" # unresolvedPoolInput
  poolInput <- pletFields @["address", "value"] resolvedPoolInput
  poolInputAddress <- plet $ poolInput.address
  poolInputValue <- plet $ poolInput.value
  findPoolOutputResult <- plet $ findPoolOutput # poolNft # poolInputAddress # outputs -- ###### 25M
  PPair poolOutput remainingOutputs <- pmatch findPoolOutputResult
  -- Force transaction to place pool output on top of outputs list to support calculating of each account
  PPair poolOutputDatum poolOutputValue <- pmatch poolOutput
  -- Consume only one pool
  ptraceIfFalse
    "Must consume only one pool"
    (plength # (pfilter # (uniqueAssetFromTxInInfo # poolNft) # inputs) #== pconstant 1) -- ###### 20M
    -- Check pool and account outputs
    #&& ptraceIfFalse
      "All outputs must match as expected"
      ( checkAllOutputs
          # oracleAssetPrice
          # manager'
          # poolInputDatum
          # poolOutputDatum
          # poolInputValue
          # poolOutputValue
          # remainingOutputs
          # accountTxIns
          # validRange
      )

findPoolOutput
  :: Term
      s
      ( PAssetClassData
          :--> PAddress
          :--> PBuiltinList PTxOut
          :--> PPair (PPair PPoolDatum (PValue 'Sorted 'Positive)) (PBuiltinList PTxOut)
      )
findPoolOutput = phoistAcyclic $ plam $ \authTokenData address -> P.do
  authToken <- plet $ ptoScottEncoding # authTokenData
  precList
    ( \self txOut remaining -> P.do
        output <- pletFields @["address", "value", "datum"] txOut
        outputValue <- plet $ output.value
        outputAddress <- plet $ output.address
        outputTxOutDatum <- plet $ output.datum
        pif
          (phasOneTokenOfAssetClass # authToken # outputValue)
          ( P.do
              POutputDatum outputDatum <- pmatch outputTxOutDatum
              PDatum datum <- pmatch $ pfield @"outputDatum" # outputDatum
              _ <-
                plet $
                  pif
                    (outputAddress #== address)
                    (pconstant ())
                    ( ptraceError
                        ( "Pool output address doesn't match: \nExpect: "
                            <> pshow address
                            <> "\nActual: "
                            <> pshow outputAddress
                        )
                    )
              poolOutput <- plet $ pcon $ PPair (pfromData (ptryFromData @PPoolDatum datum)) outputValue
              pcon (PPair poolOutput remaining)
          )
          (self # remaining)
    )
    (const (ptraceError "Cannot find pool output"))

defaultAssetInfo :: Term s (PAsData PAssetInformation)
defaultAssetInfo =
  phoistAcyclic $
    pdata $
      mkRecordConstr PAssetInformation $
        #aiSupplyAmount .= pconstantData 0
          .& #aiBorrowAmount .= pconstantData 0
          .& #aiCumulatedInterestRateSupplying .= pconstantData 1
          .& #aiCumulatedInterestRateBorrowing .= pconstantData 1

createDefaultOnNew
  :: Term
      s
      ( PMap 'Sorted PAsset PAssetInformation
          :--> PBuiltinPair (PAsData PAsset) (PAsData PReceipt)
          :--> PMaybe (PBuiltinPair (PAsData PAsset) (PAsData PAssetInformation))
      )
createDefaultOnNew = phoistAcyclic $ plam $ \oldAssetInfo assetPair -> P.do
  assetData <- plet $ pfstBuiltin # assetPair
  asset <- plet $ pfromData assetData
  AssocMap.pfoldAt
    # asset
    # (pjust #$ ppairDataBuiltin # assetData #$ defaultAssetInfo)
    # (pconst # pnothing)
    # oldAssetInfo

initNewAssets
  :: Term s (StateChange :--> PMap 'Sorted PAsset PAssetInformation :--> PMap 'Sorted PAsset PAssetInformation)
initNewAssets = phoistAcyclic $ plam $ \stateChange oldAssetInfo -> P.do
  StateChange {scSupply, scBorrow} <- pmatch stateChange
  changedAssets <- plet $ AssocMap.punionWith # plam (+) # scSupply # scBorrow
  pcon $ PMap $ pmapMaybe # (createDefaultOnNew # oldAssetInfo) # pto changedAssets

matchBothOldAndNew :: Term s (PAssetInformation :--> PAssetInformation :--> PAssetInformation)
matchBothOldAndNew = phoistAcyclic $ plam $ \_ _ -> ptraceError "Impossible to match both"

checkAndUpdateReceipt
  :: ClosedTerm PString -> Term s (PReceipt :--> PReceipt :--> PActual :--> PCumulativeRate :--> PReceipt)
checkAndUpdateReceipt msg = phoistAcyclic $ plam $ \oldAmount change cap rate -> P.do
  newAmount <- plet $ oldAmount + change
  pif
    (0 #< change)
    (passert msg (convertFrom # rate # newAmount #<= cap) newAmount)
    newAmount

updatePool
  :: Term
      s
      ( PMap 'Sorted PAsset PRiskParameters
          :--> StateChange
          :--> PInteger
          :--> PMap 'Sorted PAsset PAssetInformation
          :--> PMap 'Sorted PAsset PAssetInformation
      )
updatePool = phoistAcyclic $ plam $ \riskParamMap stateChange deltaTime oldAssetInfoMap -> P.do
  StateChange {scSupply, scBorrow} <- pmatch stateChange
  newAssets <- plet $ initNewAssets # stateChange # oldAssetInfoMap
  allAssets <- plet $ AssocMap.punionWith # matchBothOldAndNew # oldAssetInfoMap # newAssets
  pmapWithKey
    # plam
      ( \asset oldAssetInfo' -> P.do
          supplyChange <- plet $ AssocMap.pfindWithDefault # 0 # asset # scSupply
          borrowChange <- plet $ AssocMap.pfindWithDefault # 0 # asset # scBorrow
          riskParams' <- plet $ AssocMap.ptryLookup # asset # riskParamMap
          riskParams <- pletFields @["rpSupplyCap", "rpBorrowCap"] riskParams'
          oldAssetInfo <- pletAll (updateCumulatedRate # riskParams' # deltaTime # oldAssetInfo')
          supplyRate <- plet $ oldAssetInfo.aiCumulatedInterestRateSupplying
          borrowRate <- plet $ oldAssetInfo.aiCumulatedInterestRateBorrowing
          ptrace "Check @Supply cap exceeded@"
          newLendAmount <-
            plet $
              checkAndUpdateReceipt "Supply cap exceeded"
                # oldAssetInfo.aiSupplyAmount
                # supplyChange
                # riskParams.rpSupplyCap
                # supplyRate
          ptrace "Check @Borrow cap exceeded@"
          newBorrowAmount <-
            plet $
              checkAndUpdateReceipt "Borrow cap exceeded"
                # oldAssetInfo.aiBorrowAmount
                # borrowChange
                # riskParams.rpBorrowCap
                # borrowRate
          mkRecordConstr PAssetInformation $
            #aiSupplyAmount .= pdata newLendAmount
              .& #aiBorrowAmount .= pdata newBorrowAmount
              .& #aiCumulatedInterestRateSupplying .= pdata supplyRate
              .& #aiCumulatedInterestRateBorrowing .= pdata borrowRate
      )
    # allAssets

checkAllOutputs
  :: Term
      s
      ( PMap 'Sorted PAsset PPrice
          :--> PManagerDatum
          :--> PPoolDatum
          :--> PPoolDatum
          :--> PValue 'Sorted 'Positive
          :--> PValue 'Sorted 'Positive
          :--> PBuiltinList PTxOut
          :--> PBuiltinList PTxOut
          :--> PPOSIXTimeRange
          :--> PBool
      )
checkAllOutputs =
  plam $
    \oracleAssetPrice
     managerDatum
     oldPoolDatum
     newPoolDatum
     oldPoolValue
     newPoolValue
     outputs
     accounts
     validRange -> P.do
        oldPoolDatumF <- pletFields @["pdAssets", "pdLastUpdatedTime"] oldPoolDatum
        newPoolDatumF <- pletFields @["pdAssets", "pdLastUpdatedTime"] newPoolDatum
        managerDatumF <-
          pletFields
            @[ "mdRiskParameters"
             , "mdGlobalRiskParameters"
             , "mdMaxValidityDuration"
             , "mdAccountAuthToken"
             ]
            managerDatum
        oldMapAssetSupply <- plet $ oldPoolDatumF.pdAssets
        newMapAssetSupply <- plet $ newPoolDatumF.pdAssets
        oldLastUpdatedTime <- plet $ oldPoolDatumF.pdLastUpdatedTime
        newLastUpdatedTime <- plet $ newPoolDatumF.pdLastUpdatedTime
        riskParameters <- plet $ managerDatumF.mdRiskParameters
        minAdaUtxo <- plet $ pfield @"grpMinAdaUtxo" # managerDatumF.mdGlobalRiskParameters
        maxValidityDuration <- plet $ managerDatumF.mdMaxValidityDuration
        accountAuthToken <- plet $ ptoScottEncoding # managerDatumF.mdAccountAuthToken
        deltaTime <- plet $ calculateDeltaTime # oldLastUpdatedTime # newLastUpdatedTime
        matcherContext <-
          plet $
            pcon $
              MatcherContext
                { mcPriceMap = oracleAssetPrice
                , mcAssetInfoMap = newMapAssetSupply
                , mcRiskParameters = managerDatumF.mdRiskParameters
                , mcMinAdaUtxo = minAdaUtxo
                , mcAccountAuthToken = accountAuthToken
                }
        poolChange <- plet $ getPoolChange # matcherContext # accounts # outputs
        StateChange {scPoolReceivable, scPoolPayable} <- pmatch poolChange
        expectedNewMapAssetSupply <- plet $ updatePool # riskParameters # poolChange # deltaTime # oldMapAssetSupply

        checkValidityTime # validRange # newLastUpdatedTime # maxValidityDuration
          #&& ptraceIfFalse
            ("Pool output datum not valid: " <> pshow expectedNewMapAssetSupply)
            (newMapAssetSupply #== expectedNewMapAssetSupply)
          #&& ptraceIfFalse
            "Pool output value not valid"
            (newPoolValue <> scPoolPayable #== oldPoolValue <> scPoolReceivable)

calculateDeltaTime :: Term s (PPOSIXTime :--> PPOSIXTime :--> PInteger)
calculateDeltaTime = phoistAcyclic $ plam $ \oldLastUpdatedTime newLastUpdatedTime -> P.do
  delta <- plet $ newLastUpdatedTime - oldLastUpdatedTime
  PPOSIXTime iDenta <- pmatch delta
  pdiv # iDenta # 1000

getExtended :: Term s (PPOSIXTime :--> PExtended PPOSIXTime)
getExtended = phoistAcyclic $ plam $ \timestamp ->
  mkRecordConstr PFinite $
    #_0 .= pdata timestamp

checkValidityTime :: Term s (PPOSIXTimeRange :--> PPOSIXTime :--> PPOSIXTime :--> PBool)
checkValidityTime = phoistAcyclic $ plam $ \validRange newLastUpdatedTime maxValidityDuration -> P.do
  submittedRangeLowerBound <-
    plet $
      mkRecordConstr PLowerBound $
        #_0 .= pdata (getExtended # newLastUpdatedTime)
          .& #_1 .= pconstantData True
  submittedRangeUpperBound <-
    plet $
      mkRecordConstr PUpperBound $
        #_0 .= pdata (getExtended # (newLastUpdatedTime + maxValidityDuration))
          .& #_1 .= pconstantData False
  submittedRange <-
    plet $
      mkRecordConstr PInterval $
        #from .= pdata submittedRangeLowerBound
          .& #to .= pdata submittedRangeUpperBound
  ptraceIfFalse ("Invalid transaction validity range: " <> pshow submittedRange) (submittedRange #== validRange)

validatePoolTerm :: AssetClass -> AssetClass -> ClosedTerm PValidator
validatePoolTerm managerAuthToken migrationNft =
  plam $ \datum redeemer ctx ->
    pif
      ( validatePool
          # pconstant @PAssetClassData managerAuthToken
          # pconstant @PAssetClassData migrationNft
          # pfromData (ptryFromData @PPoolDatum datum)
          # pfromData (ptryFromData @PPoolRedeemer redeemer)
          # ctx
      )
      (popaque $ pconstant ())
      (popaque perror)
