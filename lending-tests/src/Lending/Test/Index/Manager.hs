{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Test.Index.Manager (indexManagerSpec) where

import Control.Monad qualified as Monad

import Lending.Index.Manager (Manager)
import Lending.Test.Common (getLatestState)
import Lending.Test.Env (IntegTest, TestEnv (TestEnv, dbConnection))

indexManagerSpec :: IntegTest
indexManagerSpec TestEnv {dbConnection} = Monad.void (getLatestState @Manager dbConnection)
