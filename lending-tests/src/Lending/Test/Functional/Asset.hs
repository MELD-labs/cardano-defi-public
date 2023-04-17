{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lending.Test.Functional.Asset
  ( TestAsset (..)
  , toAssetId
  , toTestAsset
  , getAssetMap
  )
where

import Cardano.Api qualified as CA
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson
  ( FromJSON
  , FromJSONKey
  , FromJSONKeyFunction (FromJSONKeyText)
  )
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String qualified as String
import Data.Text (Text)
import Data.Text qualified as Text
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB))
import Database.Persist.Sql (ConnectionPool)

import Lending.Index.Manager (Manager (Manager, managerDatum))
import Lending.Test.Common (getLatestStateExtract)
import Lending.Test.Env (TestUserCredential (tucSigningKey))
import Lending.Types.Asset (Asset)
import Lending.Types.Manager
  ( ManagerDatum (ManagerDatum, mdRiskParameters)
  , RiskParameters (RiskParameters, rpAssetClassData)
  )
import Plutarch.Extra.AssetClass (AssetClass (AssetClass, name, symbol), fromPlutusTokenName)

data TestAsset = TestAda | TestToken CA.AssetName
  deriving stock (Eq, Ord, Show)

toTestAsset :: Text -> TestAsset
toTestAsset text = if Text.toLower text == "ada" then TestAda else TestToken (String.fromString (Text.unpack text))

instance FromJSON TestAsset where
  parseJSON = Aeson.withText "TestAsset" $ pure . toTestAsset

instance FromJSONKey TestAsset where
  fromJSONKey = FromJSONKeyText toTestAsset

toAssetId :: TestUserCredential -> TestAsset -> CA.AssetId
toAssetId _ TestAda = CA.AdaAssetId
toAssetId oracleOperator (TestToken assetName) = CA.AssetId policyId assetName
  where
    policyId :: CA.PolicyId
    policyId =
      CA.scriptPolicyId
        . CA.SimpleScript CA.SimpleScriptV2
        . CA.RequireSignature
        . CA.verificationKeyHash
        . CA.castVerificationKey
        . CA.getVerificationKey
        . tucSigningKey
        $ oracleOperator

extractRiskParams :: Asset -> RiskParameters -> Map TestAsset Asset
extractRiskParams asset RiskParameters {rpAssetClassData = AssetClass {symbol, name}} =
  Map.singleton
    (if symbol == "" then TestAda else TestToken (fromPlutusTokenName name))
    asset

getAssetMap :: ConnectionPool -> ReaderT a (LoggingT IO) (Map TestAsset Asset)
getAssetMap dbConnection = do
  getLatestStateExtract dbConnection [] $ \Manager {managerDatum = JSONB ManagerDatum {mdRiskParameters}} ->
    pure (Map.foldMapWithKey extractRiskParams mdRiskParameters)
