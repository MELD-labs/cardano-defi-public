{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Contracts.Pool.InterestRate
  ( updateCumulatedRates
  , updateCumulatedRate
  , calculateUtilizationRatio
  , calculateBorrowingRate
  , calculateLendingRate
  )
where

import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Extra.FixedDecimal (PFixedDecimal, pconvertExp, pediv, pemul, ptoFixedDecimal, ptoFixedZero)
import Plutarch.Extra.Map qualified as AssocMap
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Monadic qualified as P

import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Common (pmapWithKey, slotsPerYear)
import Lending.Contracts.Exchange (PActual, PCumulativeRate, PDecimal, convertFrom)
import Lending.Contracts.Manager.Types (PRiskParameters)
import Lending.Contracts.Orphans ()
import Lending.Contracts.Pool.Types (PAssetInformation (PAssetInformation))
import Plutarch.Extra.Applicative (ppure)

calculateUtilizationRatio :: Term s (PActual :--> PActual :--> PDecimal)
calculateUtilizationRatio = phoistAcyclic $ plam $ \actualDebt actualSupply ->
  pif
    (actualSupply #== 0)
    0
    ((ptoFixedDecimal # pto actualDebt) `pediv` ptoFixedZero (pto actualSupply))

-- if u < u_optimal: rate = baseRate + (u/u_optimal)*rate_slope_1
-- if u >= u_optimal: rate = baseRate + rate_slope_1 + (u-u_optimal)/(1-u_optimal) * rate_slope_2
calculateBorrowingRate :: Term s (PRiskParameters :--> PDecimal :--> PCumulativeRate)
calculateBorrowingRate = phoistAcyclic $ plam $ \riskParameters' utilization -> P.do
  riskParameters <-
    pletFields
      @["rpTargetUtilizationRate", "rpBaseBorrowingRate", "rpInterestRateSlope1", "rpInterestRateSlope2"]
      riskParameters'
  baseVariableBorrowRate <- plet $ riskParameters.rpBaseBorrowingRate
  targetUtilizationRate <- plet $ riskParameters.rpTargetUtilizationRate
  interestRateSlope1 <- plet $ riskParameters.rpInterestRateSlope1
  interestRateSlope2 <- plet $ riskParameters.rpInterestRateSlope2
  rate <-
    plet $
      pif
        (utilization #< targetUtilizationRate)
        (utilization `pemul` interestRateSlope1 `pediv` targetUtilizationRate)
        ( interestRateSlope1
            + (utilization - targetUtilizationRate)
            `pemul` interestRateSlope2
            `pediv` (1 - targetUtilizationRate)
        )
  baseVariableBorrowRate + ppure # rate

-- lendingRate = borrowingRate * u * (1-reserveFactor)
calculateLendingRate :: Term s (PRiskParameters :--> PCumulativeRate :--> PDecimal :--> PCumulativeRate)
calculateLendingRate = phoistAcyclic $ plam $ \riskParameters bRate utilization -> P.do
  reserveFactor <- plet $ pfield @"rpReserveFactor" # riskParameters
  ppure #$ pconvertExp #$ pto bRate `pemul` utilization `pemul` (1 - reserveFactor)

positiveOrZero :: Term s PInteger -> Term s (PFixedDecimal 0)
positiveOrZero x = pif (x #<= 0) 0 (ptoFixedZero x)

-- i_t = (1 + rate_t * e/slotsPerYear) * i_(t-1)
calculateLinearInterest :: Term s (PCumulativeRate :--> PInteger :--> PCumulativeRate :--> PCumulativeRate)
calculateLinearInterest = phoistAcyclic $ plam $ \annualRate deltaTime oldRate -> P.do
  result <- plet $ pto annualRate `pemul` positiveOrZero deltaTime `pediv` slotsPerYear
  ppure #$ pconvertExp #$ pto oldRate `pemul` (1 + result)

-- i_t = (1 + e * rate_t/slotsPerYear + e(e-1)/2 * (rate_t/slots)^2 + e(e-1)(e-2)/6 * (rate_t/slots)^3) * i_(t-1)
-- calculateCumulatedInterestRate :: Term s (PCumulativeRate :--> PInteger :--> PCumulativeRate :--> PCumulativeRate)
-- calculateCumulatedInterestRate = phoistAcyclic $ plam $ \annualRate deltaTime oldRate -> P.do
--   -- slotRate * deltaTime
--   firstTerm <- plet $ pto annualRate `pemul` positiveOrZero deltaTime `pediv` slotsPerYear
--   -- (deltaTime * (deltaTime - 1) / 2) * slotRate^2
--   -- = firstTerm * slotRate * (deltaTime - 1) / 2
--   secondTerm <-
--     plet $
--       pconvertExp #$
--         firstTerm
--           `pemul` pto annualRate
--           `pemul` positiveOrZero (deltaTime - 1)
--           `pediv` ptoFixedZero 2
--           `pediv` slotsPerYear
--   -- (deltaTime * (deltaTime - 1) * (deltaTime - 2) / 6) * slotRate^3
--   --  = secondTerm * slotRate * (deltaTime - 2) / 3
--   thirdTerm <-
--     plet $
--       pconvertExp #$
--         secondTerm
--           `pemul` pto annualRate
--           `pemul` positiveOrZero (deltaTime - 2)
--           `pediv` ptoFixedZero 3
--           `pediv` slotsPerYear
--   ppure #$ pconvertExp #$ pto oldRate `pemul` (1 + firstTerm + secondTerm + thirdTerm)

updateCumulatedRates
  :: Term
      s
      ( AssocMap.PMap 'AssocMap.Sorted PAsset PRiskParameters
          :--> AssocMap.PMap 'AssocMap.Sorted PAsset PAssetInformation
          :--> PInteger
          :--> AssocMap.PMap 'AssocMap.Sorted PAsset PAssetInformation
      )
updateCumulatedRates = phoistAcyclic $ plam $ \riskParameters mapAssets deltaTime -> P.do
  pmapWithKey # (onEntry # riskParameters # deltaTime) # mapAssets
  where
    onEntry
      :: Term
          s
          ( AssocMap.PMap 'AssocMap.Sorted PAsset PRiskParameters
              :--> PInteger
              :--> PAsset
              :--> PAssetInformation
              :--> PAssetInformation
          )
    onEntry = phoistAcyclic $ plam $ \riskParameters' deltaTime asset ai -> P.do
      riskParams <- plet $ AssocMap.ptryLookup # asset # riskParameters'
      updateCumulatedRate # riskParams # deltaTime # ai

updateCumulatedRate :: Term s (PRiskParameters :--> PInteger :--> PAssetInformation :--> PAssetInformation)
updateCumulatedRate = phoistAcyclic $ plam $ \riskParams deltaTime ai -> P.do
  assetInfo <-
    pletFields
      @[ "aiSupplyAmount"
       , "aiBorrowAmount"
       , "aiCumulatedInterestRateSupplying"
       , "aiCumulatedInterestRateBorrowing"
       ]
      ai
  lendAmount <- plet $ assetInfo.aiSupplyAmount
  borrowAmount <- plet $ assetInfo.aiBorrowAmount
  cumulatedInterestRateLending <- plet $ assetInfo.aiCumulatedInterestRateSupplying
  cumulatedInterestRateBorrowing <- plet $ assetInfo.aiCumulatedInterestRateBorrowing
  actualSupply <- plet $ convertFrom # cumulatedInterestRateLending # lendAmount
  actualDebt <- plet $ convertFrom # cumulatedInterestRateBorrowing # borrowAmount
  utilization <- plet $ calculateUtilizationRatio # actualDebt # actualSupply
  bRate <- plet $ calculateBorrowingRate # riskParams # utilization
  lRate <- plet $ calculateLendingRate # riskParams # bRate # utilization
  cumulatedLRate <-
    plet $
      calculateLinearInterest # lRate # deltaTime # cumulatedInterestRateLending
  cumulatedBRate <-
    plet $
      calculateLinearInterest # bRate # deltaTime # cumulatedInterestRateBorrowing

  mkRecordConstr PAssetInformation $
    #aiSupplyAmount .= pdata lendAmount
      .& #aiBorrowAmount .= pdata borrowAmount
      .& #aiCumulatedInterestRateSupplying .= pdata cumulatedLRate
      .& #aiCumulatedInterestRateBorrowing .= pdata cumulatedBRate
