{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Test.Index.Oracle (indexOracleSpec) where

import Control.Monad qualified as Monad

import Lending.Index.Oracle qualified as IO
import Lending.Test.Common (getLatestState)
import Lending.Test.Env (IntegTest, TestEnv (TestEnv, dbConnection))

indexOracleSpec :: IntegTest
indexOracleSpec TestEnv {dbConnection} = Monad.void (getLatestState @IO.Oracle dbConnection)
