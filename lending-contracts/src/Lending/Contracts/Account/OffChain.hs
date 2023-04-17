{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Contracts.Account.OffChain (calculateLiquidationResultOffChain) where

import Plutarch.Api.V1.Value (AmountGuarantees (Positive), PValue)
import Plutarch.Api.V2 (KeyGuarantees (Sorted), PMap)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Lift (PConstantDecl, PLifted, PUnsafeLiftDecl)
import Plutarch.Monadic qualified as P
import Ply.Plutarch (PlyArgOf)

import Lending.Contracts.Account.OnChain.Liquidate
  ( LiquidationResult
      ( LiquidationResult
      , lrContinuingAccountDatum
      , lrNewAccountDatum
      , lrNewAccountValue
      )
  , calculateLiquidationResult
  )
import Lending.Contracts.Account.Types (PAccountDatum, PAccountLiquidateRedeemerData)
import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Exchange (PPrice)
import Lending.Contracts.Manager.Types (PManagerDatum)
import Lending.Contracts.Pool.Types (PAssetInformation)
import Lending.Types.Account.OffChain (LiquidationResultData)

newtype PLiquidationResultData (s :: S)
  = PLiquidationResultData
      ( Term
          s
          ( PDataRecord
              '[ "lrContinuingAccountDatum" ':= PAccountDatum
               , "lrNewAccountDatum" ':= PAccountDatum
               , "lrNewAccountValue" ':= PValue 'Sorted 'Positive
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PLiquidationResultData where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PLiquidationResultData where type PLifted PLiquidationResultData = LiquidationResultData
deriving via
  (DerivePConstantViaData LiquidationResultData PLiquidationResultData)
  instance
    PConstantDecl LiquidationResultData
type instance PlyArgOf PLiquidationResultData = LiquidationResultData

fromLiquidationResult :: Term s (LiquidationResult :--> PLiquidationResultData)
fromLiquidationResult = phoistAcyclic $ plam $ \liquidationResult -> P.do
  LiquidationResult
    { lrContinuingAccountDatum
    , lrNewAccountDatum
    , lrNewAccountValue
    } <-
    pmatch liquidationResult
  mkRecordConstr PLiquidationResultData $
    #lrContinuingAccountDatum .= pdata lrContinuingAccountDatum
      .& #lrNewAccountDatum .= pdata lrNewAccountDatum
      .& #lrNewAccountValue .= pdata lrNewAccountValue

calculateLiquidationResultOffChain
  :: Term
      s
      ( PManagerDatum
          :--> PMap 'Sorted PAsset PPrice
          :--> PMap 'Sorted PAsset PAssetInformation
          :--> PAccountDatum
          :--> PAccountLiquidateRedeemerData
          :--> PLiquidationResultData
      )
calculateLiquidationResultOffChain =
  phoistAcyclic $
    plam $
      \managerDatum
       assetPrices
       poolDatum
       accountDatum
       liquidateRedeemerData ->
          fromLiquidationResult #$
            calculateLiquidationResult
              # managerDatum
              # assetPrices
              # poolDatum
              # accountDatum
              # liquidateRedeemerData
