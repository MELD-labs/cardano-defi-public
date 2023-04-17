{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lending.Test.Functional.Types
  ( TestUserName
  , TestUserMap
  , TestAssetAmount (..)
  , TestRequestInstruction (..)
  , TestStep (..)
  , TestScenario (..)
  , FunctionalTestM
  )
where

import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Accum (AccumT)
import Data.Aeson
  ( FromJSON
  , SumEncoding (ObjectWithSingleField)
  , constructorTagModifier
  , fieldLabelModifier
  , sumEncoding
  )
import Data.Aeson qualified as Aeson
import Data.Aeson.TH qualified as Aeson
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Map (Map)
import Test.Tasty (TestName)
import Text.Casing qualified as Casing

import Lending.Test.Env (TestUserAccount)
import Lending.Test.Functional.Asset (TestAsset, toTestAsset)
import Lending.Types.Exchange (Price)
import Lending.Types.Orphans ()
import Plutarch.Extra.FixedDecimal (FixedDecimal)

type TestUserName = String

type TestUserMap = Map TestUserName TestUserAccount

type FunctionalTestM a = AccumT TestUserMap (LoggingT IO) a

data TestAssetAmount = TestAssetAmount
  { taaAmount :: FixedDecimal 6
  , taaAssetName :: TestAsset
  }
  deriving stock (Show)

data TestRequestInstruction
  = TestSupply TestAssetAmount
  | TestWithdraw TestAssetAmount
  | TestBorrow TestAssetAmount
  | TestRepay TestAssetAmount
  | TestCollateral [TestAsset]
  deriving stock (Show)

data TestStep
  = TestStepUpdate (Map TestUserName [TestRequestInstruction])
  | TestStepOracle (Map TestAsset Price)
  | TestStepWait Int
  deriving stock (Show)

data TestScenario = TestScenario
  { tsName :: TestName
  , tsDisable :: Maybe Bool
  , tsSteps :: [TestStep]
  }
  deriving stock (Show)

assetAmountParser :: Parser TestAssetAmount
assetAmountParser =
  TestAssetAmount
    <$> (fromRational . toRational <$> Attoparsec.scientific <* Attoparsec.skipSpace)
    <*> (toTestAsset <$> Attoparsec.takeText)

instance FromJSON TestAssetAmount where
  parseJSON = Aeson.withText "TestAssetAmount" $ either fail pure . Attoparsec.parseOnly assetAmountParser

$( Aeson.deriveFromJSON
    Aeson.defaultOptions
      { constructorTagModifier = Casing.camel . drop 4
      , fieldLabelModifier = Casing.camel . drop 3
      , sumEncoding = ObjectWithSingleField
      }
    ''TestRequestInstruction
 )
$( Aeson.deriveFromJSON
    Aeson.defaultOptions {constructorTagModifier = Casing.camel . drop 8, sumEncoding = ObjectWithSingleField}
    ''TestStep
 )
$( Aeson.deriveFromJSON
    Aeson.defaultOptions {fieldLabelModifier = Casing.camel . drop 2}
    ''TestScenario
 )
