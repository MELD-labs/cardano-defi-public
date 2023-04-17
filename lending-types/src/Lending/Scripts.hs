{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Lending.Scripts (LendingScript (..), scriptFileName, scriptHashMapFileName) where

import Data.Aeson (FromJSON, FromJSONKey (fromJSONKey), ToJSON, ToJSONKey (toJSONKey))
import Data.Aeson qualified as Aeson
import GHC.Generics (Generic)

data LendingScript
  = Manager
  | ManagerAuthToken
  | Oracle
  | OracleAuthToken
  | Account
  | AccountAuthToken
  | Pool
  | PoolAuthToken
  | AlwaysFalse
  | AccountOwnerNft
  | OperatorManagerNft
  | OperatorOracleNft
  | OperatorMigrationNft
  | OperatorPoolNft
  | OracleCheckerToken
  | AlwaysFalseToken
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Ord LendingScript where
  a <= b = show a <= show b

instance FromJSONKey LendingScript where
  fromJSONKey = Aeson.genericFromJSONKey Aeson.defaultJSONKeyOptions

instance ToJSONKey LendingScript where
  toJSONKey = Aeson.genericToJSONKey Aeson.defaultJSONKeyOptions

scriptFileName :: LendingScript -> FilePath
scriptFileName script = show script <> ".plutus"

scriptHashMapFileName :: FilePath
scriptHashMapFileName = "script-hash-map.yaml"
