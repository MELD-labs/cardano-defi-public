{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Contracts.Account.OnChain.Update
  ( validateUpdate
  )
where

import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Extra.AssetClass (PAssetClassData)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.List (ptryFromSingleton)
import Plutarch.Extra.Maybe (pisDJust)
import Plutarch.Monadic qualified as P

import Lending.Contracts.Account.Types
  ( PAccountDatum
  )
import Lending.Contracts.Common
  ( getState
  , uniqueAssetFromTxInInfo
  )
import Lending.Contracts.Manager.Types (PManagerDatum)
import Lending.Contracts.Request
  ( UnchangableAccountFields
      ( UnchangableAccountFields
      , uafAddress
      , uafBeingLiquidated
      , uafBorrowTokens
      , uafCollateralAsset
      , uafSupplyAssets
      , uafUserNft
      )
  , checkAccountWithRequests
  )

-- TODO: Allow users to remove pending requests
validateUpdate
  :: Term
      s
      ( PAssetClassData
          :--> PAccountDatum
          :--> PScriptContext
          :--> PBool
      )
validateUpdate = phoistAcyclic $ plam $ \managerAuthToken oldDatum' ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  txinfo <- pletFields @["inputs", "referenceInputs", "outputs"] ctx.txInfo
  refInputs <- plet txinfo.referenceInputs
  inputs <- plet txinfo.inputs
  outputs <- plet txinfo.outputs
  manager' <- plet $ getState @PManagerDatum # managerAuthToken # refInputs
  accountAuthAsset <- plet $ pfield @"mdAccountAuthToken" # manager'

  -- Extract account input
  inputsHavingAuthToken <- plet $ pfilter # (uniqueAssetFromTxInInfo # accountAuthAsset) # inputs
  inputTxInInfo <- plet $ ptryFromSingleton # inputsHavingAuthToken

  oldAddress <- plet $ pfield @"address" #$ pfield @"resolved" # inputTxInInfo

  oldDatum <- pletAll oldDatum'

  unchangableFields <-
    plet $
      pcon $
        UnchangableAccountFields
          { uafAddress = oldAddress
          , uafSupplyAssets = oldDatum.adSupplies
          , uafBorrowTokens = oldDatum.adBorrowings
          , uafCollateralAsset = oldDatum.adCollateralAssets
          , uafUserNft = oldDatum.adUserNft
          , uafBeingLiquidated = oldDatum.adProtocolIncentive
          }
  -- Include user's nft
  (pany # (uniqueAssetFromTxInInfo # oldDatum.adUserNft) # inputs)
    -- Liquidate Request does not exist
    #&& pnot # (pisDJust # oldDatum.adProtocolIncentive)
    -- Check account output
    #&& checkAccountWithRequests # outputs # manager' # unchangableFields
