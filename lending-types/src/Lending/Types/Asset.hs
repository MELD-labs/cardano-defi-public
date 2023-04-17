{-# LANGUAGE DataKinds #-}

module Lending.Types.Asset (Asset) where

import Data.Tagged (Tagged)

type Asset = Tagged "Asset" Integer
