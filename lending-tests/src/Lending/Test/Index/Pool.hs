{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Test.Index.Pool (indexPoolSpec) where

import Control.Monad qualified as Monad

import Lending.Index.Pool (Pool)
import Lending.Test.Common (getLatestState)
import Lending.Test.Env (IntegTest, TestEnv (TestEnv, dbConnection))

indexPoolSpec :: IntegTest
indexPoolSpec TestEnv {dbConnection} = Monad.void (getLatestState @Pool dbConnection)
