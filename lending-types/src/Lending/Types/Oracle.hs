{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lending.Types.Oracle
  ( OracleScriptParams (..)
  , OracleDatum (..)
  )
where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Plutarch.Extra.AssetClass (AssetClass)
import Plutarch.Extra.Tagged ()
import PlutusTx qualified

import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)
import Lending.Types.Orphans ()

-- TODO: Consider to move this parameter to Manager or a more suitable place
newtype OracleScriptParams = OracleScriptParams {ospOperatorNFT :: AssetClass}

newtype OracleDatum = OracleDatum
  { odAssetPrices :: Map Asset Price
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''OracleDatum [('OracleDatum, 0)]
