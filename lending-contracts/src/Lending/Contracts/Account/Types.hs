{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lending.Contracts.Account.Types
  ( PAccountRedeemer (..)
  , PRequest (..)
  , PAccountDatum (..)
  , PClearRequest (..)
  , PAccountLiquidateRedeemerData
  )
where

import Plutarch.Api.V2 (KeyGuarantees (Sorted), PAddress, PMap, PMaybeData)
import Plutarch.DataRepr
  ( DerivePConstantViaData (DerivePConstantViaData)
  , PDataFields
  )
import Plutarch.Extra.AssetClass (PAssetClassData)
import Plutarch.Lift
  ( PConstantDecl
  , PLifted
  , PUnsafeLiftDecl
  )
import Ply.Plutarch (PlyArgOf)

import Lending.Contracts.Asset (PAsset)
import Lending.Contracts.Exchange (PActual, PReceipt)
import Lending.Contracts.Orphans ()
import Lending.Types.Account
  ( AccountDatum
  , AccountLiquidateRedeemerData
  , AccountRedeemer
  , ClearRequest
  , Request
  )

data PClearRequest (s :: S)
  = PClearSupplying
      ( Term
          s
          ( PDataRecord
              '[ "cdReceiver" ':= PAddress
               ]
          )
      )
  | PClearBorrowing
      ( Term
          s
          ( PDataRecord
              '[ "cbLimited" ':= PActual
               , "cbReceiver" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PClearRequest where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PClearRequest)
instance PUnsafeLiftDecl PClearRequest where type PLifted PClearRequest = ClearRequest
deriving via (DerivePConstantViaData ClearRequest PClearRequest) instance PConstantDecl ClearRequest
type instance PlyArgOf PClearRequest = ClearRequest

newtype PAccountDatum (s :: S)
  = PAccountDatum
      ( Term
          s
          ( PDataRecord
              '[ "adSupplies" ':= PMap 'Sorted PAsset PReceipt
               , "adBorrowings" ':= PMap 'Sorted PAsset PReceipt
               , "adCollateralAssets" ':= PMap 'Sorted PAsset PBool
               , "adUserNft" ':= PAssetClassData
               , "adNormalRequests" ':= PBuiltinList PRequest
               , "adCollateralUpdate" ':= PMaybeData (PAsData (PMap 'Sorted PAsset PBool))
               , "adProtocolIncentive" ':= PMaybeData (PAsData (PMap 'Sorted PAsset PReceipt))
               , "adClearRequests" ':= PMap 'Sorted PAsset PClearRequest
               , "adExtraLovelace" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields, PShow)

instance DerivePlutusType PAccountDatum where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PAccountDatum)
instance PUnsafeLiftDecl PAccountDatum where type PLifted PAccountDatum = AccountDatum
deriving via (DerivePConstantViaData AccountDatum PAccountDatum) instance PConstantDecl AccountDatum
type instance PlyArgOf PAccountDatum = AccountDatum
newtype PAccountLiquidateRedeemerData (s :: S)
  = PAccountLiquidateRedeemerData
      ( Term
          s
          ( PDataRecord
              '[ "alrBorrowings" ':= PMap 'Sorted PAsset PActual
               , "alrCollaterals" ':= PMap 'Sorted PAsset PActual
               , "alrClearRequests" ':= PMap 'Sorted PAsset PClearRequest
               , "alrExtraLovelace" ':= PInteger
               , "alrUserNft" ':= PAssetClassData
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields, PShow)
instance DerivePlutusType PAccountLiquidateRedeemerData where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData PAccountLiquidateRedeemerData
instance PUnsafeLiftDecl PAccountLiquidateRedeemerData where
  type PLifted PAccountLiquidateRedeemerData = AccountLiquidateRedeemerData
deriving via
  (DerivePConstantViaData AccountLiquidateRedeemerData PAccountLiquidateRedeemerData)
  instance
    PConstantDecl AccountLiquidateRedeemerData
type instance PlyArgOf PAccountLiquidateRedeemerData = AccountLiquidateRedeemerData

data PAccountRedeemer (s :: S)
  = PAccountUpdateRedeemer (Term s (PDataRecord '[]))
  | PAccountApplyRedeemer (Term s (PDataRecord '[]))
  | PAccountCloseRedeemer (Term s (PDataRecord '[]))
  | PAccountLiquidateRedeemer (Term s (PDataRecord '["alrData" ':= PAccountLiquidateRedeemerData]))
  | PAccountMigrateRedeemer (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)
instance DerivePlutusType PAccountRedeemer where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PAccountRedeemer)
instance PUnsafeLiftDecl PAccountRedeemer where type PLifted PAccountRedeemer = AccountRedeemer
deriving via (DerivePConstantViaData AccountRedeemer PAccountRedeemer) instance PConstantDecl AccountRedeemer

data PRequest (s :: S)
  = PSupplyRequest
      ( Term
          s
          ( PDataRecord
              '[ "srAsset" ':= PAsset
               , "srAmount" ':= PActual
               ]
          )
      )
  | PWithdrawRequest
      ( Term
          s
          ( PDataRecord
              '[ "wrAsset" ':= PAsset
               , "wrAmount" ':= PActual
               , "wrReceiver" ':= PAddress
               ]
          )
      )
  | PBorrowRequest
      ( Term
          s
          ( PDataRecord
              '[ "brAsset" ':= PAsset
               , "brAmount" ':= PActual
               , "brReceiver" ':= PAddress
               ]
          )
      )
  | PRepayRequest
      ( Term
          s
          ( PDataRecord
              '[ "rrAsset" ':= PAsset
               , "rrAmount" ':= PActual
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PRequest where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PRequest where type PLifted PRequest = Request
deriving via (DerivePConstantViaData Request PRequest) instance PConstantDecl Request
instance PTryFrom PData (PAsData (PBuiltinList PRequest))
type instance PlyArgOf PRequest = Request
