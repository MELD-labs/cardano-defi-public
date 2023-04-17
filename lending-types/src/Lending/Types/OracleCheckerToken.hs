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
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lending.Types.OracleCheckerToken (OracleCheckerTokenRedeemer (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import GHC.Generics (Generic)
import Plutarch.Extra.Tagged ()
import PlutusTx qualified

import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Price)
import Lending.Types.Orphans ()

newtype OracleCheckerTokenRedeemer = OracleCheckerTokenRedeemer {odAssetPrices :: Map Asset Price}
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''OracleCheckerTokenRedeemer [('OracleCheckerTokenRedeemer, 0)]
