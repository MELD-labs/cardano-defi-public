{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Contracts.LoanToValue.OnChain
  ( InterestData (..)
  , checkMaxLoanToValue
  , getInterestData
  )
where

import Plutarch.Extra.Applicative (PApplicative (ppure))
import Plutarch.Extra.Comonad (pextract)
import Plutarch.Extra.FixedDecimal (pediv, pemul, pfromFixedDecimal, ptoFixedDecimal, ptoFixedZero)
import Plutarch.Extra.Function (pconst)
import Plutarch.Extra.Map (pfoldMapWithKey, ptryLookup)
import Plutarch.Extra.Sum (PSum (PSum))
import Plutarch.Monadic qualified as P

import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Exchange (PDecimal, PFiat, PLtvRatio, PPrice, PReceipt, convertFrom)
import Lending.Contracts.Manager.Types (PRiskParameters)
import Lending.Contracts.Pool.Types (PAssetInformation)
import Plutarch.Api.V2 (KeyGuarantees (Sorted), PMap)

data InterestData (s :: S) = InterestData
  { idUnderLiquidationThreshold :: Term s PBool
  , idSafeHealthFactorThreshold :: Term s PBool
  , idLoanToValue :: Term s PDecimal
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType InterestData where
  type DPTStrat _ = PlutusTypeScott

checkMaxLoanToValue
  :: Term
      s
      ( PMap 'Sorted PAsset PRiskParameters
          :--> PMap 'Sorted PAsset PPrice
          :--> PMap 'Sorted PAsset PAssetInformation
          :--> PMap 'Sorted PAsset PReceipt
          :--> PMap 'Sorted PAsset PReceipt
          :--> PBool
      )
checkMaxLoanToValue = phoistAcyclic (validate (pfield @"rpMaxLoanToValue"))

getInterestData
  :: Term
      s
      ( PMap 'Sorted PAsset PRiskParameters
          :--> PMap 'Sorted PAsset PPrice
          :--> PMap 'Sorted PAsset PAssetInformation
          :--> PMap 'Sorted PAsset PReceipt
          :--> PMap 'Sorted PAsset PReceipt
          :--> PDecimal
          :--> PFiat
          :--> PBool
          :--> InterestData
      )
getInterestData = phoistAcyclic (validateHealthFactor (pfield @"rpLiquidationThreshold"))

validate
  :: Term s (PRiskParameters :--> PLtvRatio)
  -> Term
      s
      ( PMap 'Sorted PAsset PRiskParameters
          :--> PMap 'Sorted PAsset PPrice
          :--> PMap 'Sorted PAsset PAssetInformation
          :--> PMap 'Sorted PAsset PReceipt
          :--> PMap 'Sorted PAsset PReceipt
          :--> PBool
      )
validate getLtvRatio = plam $ \riskMap priceMap oldPool collateral borrow -> P.do
  collateralValue <- plet $ weightedCollateralValue getLtvRatio
  totalCollateral <- plet $ pfoldMapWithKey # (collateralValue # riskMap # priceMap # oldPool) # collateral
  totalBorrow <- plet $ pfoldMapWithKey # (borrowValue # priceMap # oldPool) # borrow
  totalBorrow #<= totalCollateral

-- Safe health factor:
-- `totalCollateral` > `minSafeCollateral`
-- `totalBorrow` * `healthFactorThreshold` <= `totalCollateral` * `liquidationThreshold`
validateHealthFactor
  :: Term s (PRiskParameters :--> PLtvRatio)
  -> Term
      s
      ( PMap 'Sorted PAsset PRiskParameters
          :--> PMap 'Sorted PAsset PPrice
          :--> PMap 'Sorted PAsset PAssetInformation
          :--> PMap 'Sorted PAsset PReceipt
          :--> PMap 'Sorted PAsset PReceipt
          :--> PDecimal
          :--> PFiat
          :--> PBool
          :--> InterestData
      )
validateHealthFactor getLtvRatio =
  plam $ \riskMap priceMap oldPool collateral borrow healthFactorThreshold minCollateralThreshold needCheckHF -> P.do
    collateralValue <- plet $ weightedCollateralValue getLtvRatio
    fullyCollateralValue <- plet $ weightedCollateralValue (pconst # 1)
    totalCollateral <- plet $ pfoldMapWithKey # (collateralValue # riskMap # priceMap # oldPool) # collateral
    fullyTotalCollateral <- plet $ pfoldMapWithKey # (fullyCollateralValue # riskMap # priceMap # oldPool) # collateral
    totalBorrow <- plet $ pfoldMapWithKey # (borrowValue # priceMap # oldPool) # borrow
    totalBorrowWithHFThreshold <-
      plet $ ppure #$ pfromFixedDecimal # (ptoFixedZero (pto $ pto totalBorrow) `pemul` healthFactorThreshold)
    pcon $
      InterestData
        { idUnderLiquidationThreshold = totalBorrow #<= totalCollateral
        , idSafeHealthFactorThreshold =
            pif
              needCheckHF
              ( (minCollateralThreshold #< pextract # fullyTotalCollateral)
                  #&& (totalBorrowWithHFThreshold #<= pto totalCollateral)
              )
              (pconstant True)
        , idLoanToValue =
            pif
              (fullyTotalCollateral #== 0)
              0
              ( (ptoFixedDecimal #$ pextract #$ pextract # totalBorrow)
                  `pediv` ptoFixedZero (pextract #$ pextract # fullyTotalCollateral)
              )
        }

weightedCollateralValue
  :: Term s (PRiskParameters :--> PLtvRatio)
  -> Term
      s
      ( PMap 'Sorted PAsset PRiskParameters
          :--> PMap 'Sorted PAsset PPrice
          :--> PMap 'Sorted PAsset PAssetInformation
          :--> PAsset
          :--> PReceipt
          :--> PSum PFiat
      )
weightedCollateralValue getLtvRatio = plam $ \riskMap priceMap oldPool asset receiptAmount -> P.do
  risks <- plet $ ptryLookup # asset # riskMap
  price <- plet $ ptryLookup # asset # priceMap
  assetInfo <- plet $ ptryLookup # asset # oldPool
  lendingRate <- plet $ pfield @"aiCumulatedInterestRateSupplying" # assetInfo
  actualAmount <- plet $ convertFrom # lendingRate # receiptAmount
  fiat <- plet $ convertFrom # price # actualAmount
  ltvRatio <- plet $ getLtvRatio # risks
  power <- plet $ convertFrom # ltvRatio # fiat
  pcon (PSum power)

borrowValue
  :: Term
      s
      ( PMap 'Sorted PAsset PPrice
          :--> PMap 'Sorted PAsset PAssetInformation
          :--> PAsset
          :--> PReceipt
          :--> PSum PFiat
      )
borrowValue = plam $ \priceMap oldPool asset receiptAmount -> P.do
  price <- plet $ ptryLookup # asset # priceMap
  assetInfo <- plet $ ptryLookup # asset # oldPool
  lendingRate <- plet $ pfield @"aiCumulatedInterestRateBorrowing" # assetInfo
  actualAmount <- plet $ convertFrom # lendingRate # receiptAmount
  power <- plet $ convertFrom # price # actualAmount
  pcon (PSum power)
