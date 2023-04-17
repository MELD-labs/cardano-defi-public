{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Contracts.Common
  ( ptryFromData
  , passetClassValuePositive
  , slotsPerYear
  , getState
  , hasUniqueAsset
  , uniqueAssetFromTxInInfo
  , getCollateralAssets
  , findAccountTxIns
  , checkValueInMapIsZero
  , phasOneTokenOfAssetClassBurned
  , getInlineDatum
  , expectNextOutput
  , expectInlineDatum
  , isBorrowableToken
  , isSupplyableToken
  , getAssetClassData
  , pfilterKey
  , pmapWithKey
  , getOracleAssetPrices
  , pmin
  , calculatePercent
  , percentOf
  , ptraceAssertEqual
  )
where

import Plutarch.Api.V1 (PAddress)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Scripts (PRedeemer (PRedeemer))
import Plutarch.Api.V1.Value
  ( AmountGuarantees (Positive)
  , KeyGuarantees (Sorted, Unsorted)
  , PValue
  , pvalueOf
  )
import Plutarch.Api.V1.Value qualified as V
import Plutarch.Api.V2 (PDatum (PDatum), PMap (PMap), POutputDatum (POutputDatum), PScriptPurpose (PMinting))
import Plutarch.Api.V2.Tx (PTxInInfo, PTxOut)
import Plutarch.Builtin (PIsData (pdataImpl), ppairDataBuiltin)
import Plutarch.Extra.Applicative (PApplicative (ppure))
import Plutarch.Extra.AssetClass (PAssetClass (PAssetClass, pname, psymbol), PAssetClassData, ptoScottEncoding)
import Plutarch.Extra.Bool (passert)
import Plutarch.Extra.Comonad (pextract)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.Map qualified as AssocMap
import Plutarch.Extra.Maybe (pisDJust, pmaybe)
import Plutarch.Extra.Record (mkRecordConstr, (.=))
import Plutarch.Extra.ScriptContext (ptryFromInlineDatum)
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Extra.Value (passetClassValueOf, phasOneTokenOfAssetClass)
import Plutarch.List (ptryUncons)
import Plutarch.List qualified as List
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe qualified as Unsafe

import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Exchange (PActual, PDecimal, PPrice, PReceipt)
import Lending.Contracts.Manager.Types (PRiskParameters)
import Lending.Contracts.OracleCheckerToken.Types (POracleCheckerTokenRedeemer)
import Lending.Contracts.Orphans ()
import Plutarch.Extra.FixedDecimal (PFixedDecimal, pediv, pemul, pfromFixedDecimal, ptoFixedDecimal, ptoFixedZero)
import Plutarch.Extra.Map (pkvPairKey)

slotsPerYear :: Term s (PFixedDecimal 0)
slotsPerYear = 31_536_000

ptryFromData :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> Term s (PAsData a)
ptryFromData x = unTermCont $ fst <$> tcont (ptryFrom @(PAsData a) x)

passetClassValuePositive :: Term s (PAssetClass :--> PInteger :--> PValue 'Sorted 'Positive)
passetClassValuePositive = phoistAcyclic $ plam $ \ac amount -> P.do
  PAssetClass {psymbol, pname} <- pmatch ac
  passert "Amount must be non-negative" (0 #<= amount) $
    Unsafe.punsafeDowncast (pto (V.psingletonData # psymbol # pname # pdata amount))

pGetTxOutFromTxInInfo :: Term s (PTxInInfo :--> PTxOut)
pGetTxOutFromTxInInfo = phoistAcyclic $
  plam $
    \txInInfo -> pfield @"resolved" # txInInfo

uniqueAssetFromTxInInfo :: Term s (PAssetClassData :--> PTxInInfo :--> PBool)
uniqueAssetFromTxInInfo = phoistAcyclic $ plam $ \asset txininfo -> P.do
  let txout = pfield @"resolved" # txininfo
  hasUniqueAsset # asset # txout

hasUniqueAsset :: Term s (PAssetClassData :--> PTxOut :--> PBool)
hasUniqueAsset = phoistAcyclic $ plam $ \asset' txout -> P.do
  value <- plet $ pfield @"value" # txout
  asset <- pletFields @["symbol", "name"] asset'
  amount <- plet $ pvalueOf # value # asset.symbol # asset.name
  pif
    (1 #< amount)
    (ptraceError ("More than one authentic token per utxo" <> pshow amount))
    (1 #== amount)

getInlineDatum :: (PTryFrom PData (PAsData a), PIsData a) => Term s (POutputDatum :--> a)
getInlineDatum = phoistAcyclic $ plam $ \datum -> P.do
  inlineDatum <- plet $ ptryFromInlineDatum # datum
  pfromData (ptryFromData (pto inlineDatum))

getState :: (PTryFrom PData (PAsData a), PIsData a) => Term s (PAssetClassData :--> PBuiltinList PTxInInfo :--> a)
getState = phoistAcyclic $ plam $ \authTokenData -> P.do
  authToken <- plet $ ptoScottEncoding # authTokenData
  precList
    (\self x xs -> whenCons # authToken # self # x # xs)
    (const $ ptraceError "Cannot find state")
  where
    whenCons
      :: (PTryFrom PData (PAsData a), PIsData a)
      => Term s (PAssetClass :--> (PBuiltinList PTxInInfo :--> a) :--> PTxInInfo :--> PBuiltinList PTxInInfo :--> a)
    whenCons = phoistAcyclic $ plam $ \authToken self x xs -> P.do
      resolved' <- plet $ pfield @"resolved" # x
      resolved <- pletFields @["value", "datum"] resolved'
      pif
        (phasOneTokenOfAssetClass # authToken # resolved.value)
        (getInlineDatum # resolved.datum)
        (self # xs)

ptraceAssertEqual :: (PEq a, PShow a) => Term s PString -> Term s a -> Term s a -> Term s PBool
ptraceAssertEqual msg expected actual =
  ptraceIfFalse (msg <> "\nExpected: " <> pshow expected <> "\nActual: " <> pshow actual) (expected #== actual)

getCollateralAssets
  :: Term
      s
      ( AssocMap.PMap 'Sorted PAsset PReceipt
          :--> AssocMap.PMap 'Sorted PAsset PBool
          :--> AssocMap.PMap 'Sorted PAsset PReceipt
      )
getCollateralAssets = phoistAcyclic $ plam $ \supplyAssets collaterals ->
  pfilterKey # plam (\k -> AssocMap.pfindWithDefault # pconstant False # k # collaterals) # supplyAssets

checkBorrowCap
  :: Term s (AssocMap.PMap 'Sorted PAsset PRiskParameters :--> PAsset :--> PActual :--> PBool)
checkBorrowCap = phoistAcyclic $ plam $ \riskParams assetId borrowAmount ->
  pmaybe
    # pconstant False
    # plam ((borrowAmount #<=) . pfromData . (pfield @"rpBorrowCap" #))
    # (AssocMap.plookup # assetId # riskParams)

checkSupplyCap
  :: Term s (AssocMap.PMap 'Sorted PAsset PRiskParameters :--> PAsset :--> PActual :--> PBool)
checkSupplyCap = phoistAcyclic $ plam $ \riskParams assetId supplyAmount ->
  pmaybe
    # pconstant False
    # plam ((supplyAmount #<=) . pfromData . (pfield @"rpSupplyCap" #))
    # (AssocMap.plookup # assetId # riskParams)

isBorrowableToken :: Term s (AssocMap.PMap 'Sorted PAsset PRiskParameters :--> PAsset :--> PBool)
isBorrowableToken = phoistAcyclic $ plam $ \riskParams assetId -> checkBorrowCap # riskParams # assetId # 0

isSupplyableToken :: Term s (AssocMap.PMap 'Sorted PAsset PRiskParameters :--> PAsset :--> PBool)
isSupplyableToken = phoistAcyclic $ plam $ \riskParams assetId -> checkSupplyCap # riskParams # assetId # 0

findAccountTxIns :: Term s (PBuiltinList PTxInInfo :--> PAssetClassData :--> PBuiltinList PTxOut)
findAccountTxIns = phoistAcyclic $ plam $ \inputs authAsset -> P.do
  listAccoutTxIn <- plet $ pfilter # (uniqueAssetFromTxInInfo # authAsset) # inputs
  pmap # pGetTxOutFromTxInInfo # listAccoutTxIn

checkValueInMapIsZero :: Term s (AssocMap.PMap 'Sorted PAsset PReceipt :--> PBool)
checkValueInMapIsZero = phoistAcyclic $ plam $ \mapData ->
  AssocMap.pall
    # phoistAcyclic (plam $ \value -> value #== 0)
    # mapData

-- Checker for burning one token
phasOneTokenOfAssetClassBurned
  :: forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S)
   . Term s (PAssetClass :--> PValue keys amounts :--> PBool)
phasOneTokenOfAssetClassBurned = phoistAcyclic $
  plam $ \cls v ->
    passetClassValueOf # cls # v #== -1

expectNextOutput
  :: Term
      s
      ( PBuiltinList PTxOut
          :--> PAddress
          :--> PValue 'Sorted 'Positive
          :--> POutputDatum
          :--> PBuiltinList PTxOut
      )
expectNextOutput = phoistAcyclic $ plam $ \txOuts address value datum -> P.do
  PPair txOut' remaining <- pmatch (ptryUncons # txOuts)
  txOut <- pletAll txOut'
  checkAddress <- plet $ ptraceAssertEqual "Address doesn't match" address txOut.address
  checkValue <- plet $ ptraceAssertEqual "Value doesn't match" value txOut.value
  checkDatum <- plet $ ptraceAssertEqual "Datum doesn't match" datum txOut.datum
  checkRefScript <- plet $ ptraceIfFalse "Ref script is non-empty" $ pnot # (pisDJust # txOut.referenceScript)
  passert
    "Next output doesn't match"
    (checkAddress #&& checkValue #&& checkDatum #&& checkRefScript)
    remaining

expectInlineDatum :: PIsData d => Term s (d :--> POutputDatum)
expectInlineDatum = phoistAcyclic $ plam $ \expectedDatum ->
  mkRecordConstr POutputDatum (#outputDatum .= pdata (pcon (PDatum (pdataImpl expectedDatum))))

getAssetClassData :: Term s (PAsset :--> PMap 'Sorted PAsset PRiskParameters :--> PAssetClassData)
getAssetClassData = phoistAcyclic $ plam $ \asset riskParams ->
  pfield @"rpAssetClassData" #$ AssocMap.ptryLookup # asset # riskParams

pmapWithKey
  :: (PIsData k, PIsData a, PIsData b)
  => Term s ((k :--> a :--> b) :--> PMap 'Sorted k a :--> PMap 'Sorted k b)
pmapWithKey = phoistAcyclic $ plam $ \f ma ->
  pcon . PMap $
    List.pmap
      # plam
        ( \pair -> P.do
            key <- plet $ pfromData $ pfstBuiltin # pair
            val <- plet $ pfromData $ psndBuiltin # pair
            ppairDataBuiltin # pdata key # pdata (f # key # val)
        )
      # pto ma

pfilterKey :: (PIsData k) => Term s ((k :--> PBool) :--> PMap 'Sorted k a :--> PMap 'Sorted k a)
pfilterKey = phoistAcyclic $ plam $ \f ma ->
  pcon . PMap $ List.pfilter # plam (\pair -> f #$ pkvPairKey # pair) # pto ma

getOracleAssetPrices
  :: Term
      s
      ( PMap 'Unsorted PScriptPurpose PRedeemer
          :--> PAsData PAssetClassData
          :--> PMap 'Sorted PAsset PPrice
      )
getOracleAssetPrices = phoistAcyclic $ plam $ \txRedeemers assetClassData -> P.do
  psymbol <- plet $ pfield @"symbol" # assetClassData
  redeemer <- plet $ AssocMap.ptryLookup # pcon (PMinting (pdcons # psymbol # pdnil)) # txRedeemers
  PRedeemer mintRedeemer <- pmatch redeemer
  octRedeemer <- plet $ ptryFromData @POracleCheckerTokenRedeemer mintRedeemer
  pfield @"odAssetPrices" # octRedeemer

pmin :: POrd a => Term s (a :--> a :--> a)
pmin = phoistAcyclic $ plam $ \a b -> pif (a #<= b) a b

calculatePercent :: Term s (PDecimal :--> PReceipt :--> PReceipt)
calculatePercent = phoistAcyclic $ plam $ \percent old ->
  ppure #$ pfromFixedDecimal # (ptoFixedZero (pextract # old) `pemul` percent)

percentOf :: Term s (PTagged a PInteger :--> PTagged a PInteger :--> PDecimal)
percentOf = phoistAcyclic $ plam $ \dividend divisor ->
  (ptoFixedDecimal @18 # pto dividend) `pediv` ptoFixedZero (pto divisor)
