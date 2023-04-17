{-# LANGUAGE DataKinds #-}

module Lending.Contracts.Asset
  ( PAsset
  )
where

import Plutarch.Extra.Tagged (PTagged)

type PAsset = PTagged "Asset" PInteger
