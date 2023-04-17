module Lending.Index.Query.ChainPoint (getLatestChainPoint, queryLatestChainPoint) where

import Control.Monad.Catch (throwM)
import Control.Monad.Extra (maybeM)
import Database.Persist qualified as Persist

import Cardano.Index.ChainPoint.Model (ChainPoint, EntityField (ChainPointSlotNo))
import Lending.Index.Exception (IndexException (IndexChainPointNotFound))
import Lending.Index.Query.Types (SqlM)

queryLatestChainPoint :: SqlM (Maybe (Persist.Entity ChainPoint))
queryLatestChainPoint =
  Persist.selectFirst
    []
    [Persist.Desc ChainPointSlotNo]

getLatestChainPoint :: SqlM ChainPoint
getLatestChainPoint =
  maybeM (throwM IndexChainPointNotFound) (pure . Persist.entityVal) queryLatestChainPoint
