{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Index.Orphans () where

import Database.Persist.TH qualified as Persist
import Lending.Scripts (LendingScript)

Persist.derivePersistField "LendingScript"
