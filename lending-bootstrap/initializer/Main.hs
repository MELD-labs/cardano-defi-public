{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Api qualified as CA
import Cardano.Api.Extra.Node (NodeIO)
import Cardano.Api.Extra.Node qualified as CAE
import Cardano.Api.Shelley qualified as CA
import Control.Exception (Exception)
import Control.Monad ((>=>))
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Trans.Reader qualified as Reader
import Data.Coerce qualified as Coerce
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Yaml (FromJSON)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Plutus.V2.Ledger.Api (POSIXTime, ToData)
import Plutus.V2.Ledger.Api qualified as Plutus
import Ply qualified
import Ply.Core.Deserialize qualified as Ply
import Ply.Core.Serialize.Script qualified as Ply
import System.Environment qualified as Environment
import System.FilePath ((</>))

import Cardano.Api.Extra.Key (readPaymentSigningKey)
import Cardano.Api.Extra.NetworkId (NetworkIdText (NetworkIdText))
import Cardano.Api.Extra.NetworkParams (getNetworkParams)
import Cardano.Index.Data.RawBytesHex (RawBytesHex (RawBytesHex))
import Lending.Core.Utils (buildSignAndSubmitTx, queryInputsAndCollaterals)
import Lending.Core.Utils qualified as Utils
import Lending.Scripts
  ( LendingScript
      ( Account
      , AccountAuthToken
      , Manager
      , ManagerAuthToken
      , OperatorManagerNft
      , OperatorMigrationNft
      , OperatorOracleNft
      , OperatorPoolNft
      , Oracle
      , OracleAuthToken
      , OracleCheckerToken
      , Pool
      , PoolAuthToken
      )
  , scriptFileName
  , scriptHashMapFileName
  )
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Decimal, Fiat)
import Lending.Types.Manager
  ( GlobalRiskParameters
      ( GlobalRiskParameters
      , grpBatcherFee
      , grpCloseFactor
      , grpCloseFactorHealthFactorThreshold
      , grpCloseFactorMinCollateralThreshold
      , grpLiquidatorIncentive
      , grpMaxLiquidationCloseFactor
      , grpMinAdaUtxo
      , grpProtocolIncentive
      )
  , ManagerDatum
    ( ManagerDatum
    , mdAccountAddress
    , mdAccountAuthToken
    , mdGlobalRiskParameters
    , mdMaxValidityDuration
    , mdOracleCheckerToken
    , mdPoolNft
    , mdRiskParameters
    , mdRiskParamsOperatorNft
    , mdTreasuryOperatorNft
    )
  , RiskParameters
  )
import Lending.Types.Oracle (OracleDatum (OracleDatum, odAssetPrices))
import Lending.Types.Pool (PoolDatum (PoolDatum, pdAssets, pdLastUpdatedTime))
import Plutarch.Extra.AssetClass (AssetClass (AssetClass))
import Plutarch.Extra.FixedDecimal (FixedDecimal, fixedNumerator)
import Plutus.Extra.Base16Plutus (Base16Plutus (unBase16Plutus))
import Service.Runner (runApp)
import TxBuilder.Api
  ( BuildConstraints
  , NetworkParams (pparams)
  , WitnessScript (AttachedScript)
  , mustMintValue
  , mustPayTo
  , mustSpendFromWallet
  , setChangeAddr
  , setCollateral
  )
import TxBuilder.Api.Types (UtxoInput (uiTxOut))

data InitializerConfig = InitializerConfig
  { networkId :: NetworkIdText
  , collateralUtxo :: CA.TxIn
  , riskParameters :: Map Asset RiskParameters
  , batcherFee :: FixedDecimal 6
  , minAdaUtxo :: FixedDecimal 6
  , maxValidityDuration :: POSIXTime
  , initialPoolLovelace :: CA.Lovelace
  , closeFactor :: Decimal
  , maxLiquidationCloseFactor :: Decimal
  , closeFactorHealthFactorThreshold :: Decimal
  , closeFactorMinCollateralThreshold :: Fiat
  , liquidatorIncentive :: Decimal
  , protocolIncentive :: Decimal
  , initialManagerOperatorAddress :: CA.AddressInEra CA.BabbageEra
  , initialOracleOperatorAddress :: CA.AddressInEra CA.BabbageEra
  , initialMigrationOperatorAddress :: CA.AddressInEra CA.BabbageEra
  , initialPoolOperatorAddress :: CA.AddressInEra CA.BabbageEra
  , initialStakeKeyHash :: RawBytesHex (CA.Hash CA.StakeKey)
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data ContractExporterConfig = ContractExporterConfig
  { rootOperatorManagerNftUtxo :: CA.TxIn
  , rootOperatorOracleNftUtxo :: CA.TxIn
  , rootOperatorMigrationNftUtxo :: CA.TxIn
  , rootOperatorPoolNftUtxo :: CA.TxIn
  , rootManagerAuthTokenUtxo :: CA.TxIn
  , rootOracleAuthTokenUtxo :: CA.TxIn
  , rootPoolAuthTokenUtxo :: CA.TxIn
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data LoadedConfig = LoadedConfig
  { scriptDir :: FilePath
  , initializerConfig :: InitializerConfig
  , contractExporterConfig :: ContractExporterConfig
  , plutusScriptHashMap :: Map LendingScript Plutus.ScriptHash
  , caScriptHashMap :: Map LendingScript CA.ScriptHash
  , deployerSigningKey :: CA.SigningKey CA.PaymentExtendedKey
  }

data DeployException
  = UtxoNotFound CA.TxIn
  | ScriptNotFound LendingScript
  | UnableToDecodeScript
  | NoUtxosProvided
  deriving stock (Show)
  deriving anyclass (Exception)

getScript :: FilePath -> LendingScript -> NodeIO (CA.Script CA.PlutusScriptV2)
getScript scriptDir =
  MonadIO.liftIO . Ply.readEnvelope . getScriptPath
    >=> toScript . getRawScript . Ply.serializeScript . Ply.tsScript
  where
    getScriptPath scriptName = scriptDir </> scriptFileName scriptName
    getRawScript = CA.deserialiseFromRawBytes (CA.AsPlutusScript CA.AsPlutusScriptV2)
    toScript = maybe (Catch.throwM UnableToDecodeScript) (pure . CA.PlutusScript CA.PlutusScriptV2)

getScriptHash :: LendingScript -> Map LendingScript a -> NodeIO a
getScriptHash script scriptHashMap =
  maybe
    (Catch.throwM (ScriptNotFound script))
    pure
    (Map.lookup script scriptHashMap)

getAuthTokenAssetClass :: Plutus.ScriptHash -> AssetClass
getAuthTokenAssetClass scriptHash = AssetClass (Coerce.coerce scriptHash) ""

getScriptAddress :: Plutus.ScriptHash -> Plutus.Address
getScriptAddress hash = Plutus.Address (Plutus.ScriptCredential (Coerce.coerce hash)) Nothing

getAddress :: Maybe (CA.Hash CA.StakeKey) -> CA.ScriptHash -> NodeIO (CA.AddressInEra CA.BabbageEra)
getAddress stakeKey scriptHash = do
  networkId <- Reader.asks CA.localNodeNetworkId
  pure
    ( CA.makeShelleyAddressInEra
        networkId
        (CA.PaymentCredentialByScript scriptHash)
        (maybe CA.NoStakeAddress (CA.StakeAddressByValue . CA.StakeCredentialByKey) stakeKey)
    )

mintNftAndPayToGlobalState
  :: ToData datum
  => LoadedConfig
  -> Maybe CA.Lovelace
  -> LendingScript
  -> LendingScript
  -> Maybe (CA.Hash CA.StakeKey)
  -> datum
  -> NodeIO BuildConstraints
mintNftAndPayToGlobalState
  loadedConfig@LoadedConfig {caScriptHashMap}
  maybeAda
  authTokenType
  scriptType
  stakeKey
  datum = do
    scriptAddress <- getScriptHash scriptType caScriptHashMap >>= getAddress stakeKey
    let txOutDatum =
          CA.TxOutDatumInline
            CA.ReferenceTxInsScriptsInlineDatumsInBabbageEra
            (CA.fromPlutusData (Plutus.toData datum))
    mintNftAndPayToAddress loadedConfig maybeAda authTokenType scriptAddress txOutDatum

mintNftAndPayToAddress
  :: LoadedConfig
  -> Maybe CA.Lovelace
  -> LendingScript
  -> CA.AddressInEra CA.BabbageEra
  -> CA.TxOutDatum CA.CtxTx CA.BabbageEra
  -> NodeIO BuildConstraints
mintNftAndPayToAddress LoadedConfig {scriptDir} maybeAda authTokenType address datum = do
  authTokenScript@(CA.PlutusScript _ plutusScript) <- getScript scriptDir authTokenType
  networkParams <- getNetworkParams @CA.BabbageEra
  let nftAssetId = CA.AssetId (CA.scriptPolicyId authTokenScript) ""
      mintNftConstraint =
        mustMintValue
          nftAssetId
          1
          (AttachedScript CA.PlutusScriptV2 plutusScript)
          ()
      value =
        maybe
          ( Utils.calculateUtxoValueSatisfyMinAda
              (pparams networkParams)
              address
              (CA.valueFromList [(nftAssetId, 1)])
              datum
          )
          ((CA.valueFromList [(nftAssetId, 1)] <>) . CA.lovelaceToValue)
          maybeAda
      outputConstraint = mustPayTo address value datum
  pure (mintNftConstraint <> outputConstraint)

deployGlobalStateUtxos :: LoadedConfig -> NodeIO ()
deployGlobalStateUtxos
  loadedConfig@LoadedConfig
    { initializerConfig =
      InitializerConfig
        { collateralUtxo
        , riskParameters
        , batcherFee
        , minAdaUtxo
        , maxValidityDuration
        , initialPoolLovelace
        , closeFactor
        , maxLiquidationCloseFactor
        , closeFactorHealthFactorThreshold
        , closeFactorMinCollateralThreshold
        , liquidatorIncentive
        , protocolIncentive
        , initialManagerOperatorAddress
        , initialOracleOperatorAddress
        , initialMigrationOperatorAddress
        , initialPoolOperatorAddress
        , initialStakeKeyHash = RawBytesHex stakeKeyHash
        }
    , contractExporterConfig =
      ContractExporterConfig
        { rootOperatorManagerNftUtxo
        , rootOperatorOracleNftUtxo
        , rootOperatorMigrationNftUtxo
        , rootOperatorPoolNftUtxo
        , rootManagerAuthTokenUtxo
        , rootPoolAuthTokenUtxo
        , rootOracleAuthTokenUtxo
        }
    , plutusScriptHashMap
    , deployerSigningKey
    } =
    do
      networkParams <- getNetworkParams @CA.BabbageEra
      -- inputConstraint and collateralConstraint
      let spentUtxos =
            [ rootOperatorManagerNftUtxo
            , rootManagerAuthTokenUtxo
            , rootPoolAuthTokenUtxo
            , rootOracleAuthTokenUtxo
            , rootOperatorOracleNftUtxo
            , rootOperatorMigrationNftUtxo
            , rootOperatorPoolNftUtxo
            ]
      (inputUtxos, collateralUtxos) <- queryInputsAndCollaterals spentUtxos [collateralUtxo] $ pparams networkParams
      let inputConstraint = foldMap mustSpendFromWallet inputUtxos
          collateralConstraint = setCollateral collateralUtxos

          -- changeAddressConstraint
          addressFromTxOut (CA.TxOut address _ _ _) = address

      changeAddressConstraint <-
        maybe
          (Catch.throwM NoUtxosProvided)
          (pure . setChangeAddr . addressFromTxOut)
          (Maybe.listToMaybe (uiTxOut <$> inputUtxos))

      -- managerConstraint
      let globalRiskParameters =
            GlobalRiskParameters
              { grpCloseFactor = closeFactor
              , grpMaxLiquidationCloseFactor = maxLiquidationCloseFactor
              , grpCloseFactorHealthFactorThreshold = closeFactorHealthFactorThreshold
              , grpCloseFactorMinCollateralThreshold = closeFactorMinCollateralThreshold
              , grpLiquidatorIncentive = liquidatorIncentive
              , grpProtocolIncentive = protocolIncentive
              , grpMinAdaUtxo = fixedNumerator minAdaUtxo
              , grpBatcherFee = fixedNumerator batcherFee
              }
      accountAuthTokenAsset <- getAuthTokenAssetClass <$> getScriptHash AccountAuthToken plutusScriptHashMap
      poolAuthTokenAsset <- getAuthTokenAssetClass <$> getScriptHash PoolAuthToken plutusScriptHashMap
      oracleCheckerTokenAsset <- getAuthTokenAssetClass <$> getScriptHash OracleCheckerToken plutusScriptHashMap
      operatorPoolNft <- getAuthTokenAssetClass <$> getScriptHash OperatorPoolNft plutusScriptHashMap
      operatorManagerNft <- getAuthTokenAssetClass <$> getScriptHash OperatorManagerNft plutusScriptHashMap
      accountAddress <- getScriptAddress <$> getScriptHash Account plutusScriptHashMap
      managerConstraint <-
        mintNftAndPayToGlobalState
          loadedConfig
          Nothing
          ManagerAuthToken
          Manager
          Nothing
          ManagerDatum
            { mdAccountAuthToken = accountAuthTokenAsset
            , mdRiskParameters = riskParameters
            , mdPoolNft = poolAuthTokenAsset
            , mdTreasuryOperatorNft = operatorPoolNft
            , mdRiskParamsOperatorNft = operatorManagerNft
            , mdAccountAddress = accountAddress
            , mdMaxValidityDuration = maxValidityDuration
            , mdOracleCheckerToken = oracleCheckerTokenAsset
            , mdGlobalRiskParameters = globalRiskParameters
            }

      -- poolParamsConstraint
      poolParamsConstraint <-
        mintNftAndPayToGlobalState
          loadedConfig
          (Just initialPoolLovelace)
          PoolAuthToken
          Pool
          (Just stakeKeyHash)
          PoolDatum
            { pdAssets = mempty
            , pdLastUpdatedTime = 0
            }

      -- oracle
      oracleConstraint <-
        mintNftAndPayToGlobalState
          loadedConfig
          Nothing
          OracleAuthToken
          Oracle
          Nothing
          OracleDatum
            { odAssetPrices = mempty
            }

      -- Operator NFTs
      operatorManagerNftConstraint <-
        mintNftAndPayToAddress
          loadedConfig
          Nothing
          OperatorManagerNft
          initialManagerOperatorAddress
          CA.TxOutDatumNone

      operatorOracleNftConstraint <-
        mintNftAndPayToAddress
          loadedConfig
          Nothing
          OperatorOracleNft
          initialOracleOperatorAddress
          CA.TxOutDatumNone

      operatorMigrationNftConstraint <-
        mintNftAndPayToAddress
          loadedConfig
          Nothing
          OperatorMigrationNft
          initialMigrationOperatorAddress
          CA.TxOutDatumNone

      operatorPoolNftConstraint <-
        mintNftAndPayToAddress
          loadedConfig
          Nothing
          OperatorPoolNft
          initialPoolOperatorAddress
          CA.TxOutDatumNone

      let constraints =
            inputConstraint
              <> collateralConstraint
              <> changeAddressConstraint
              <> managerConstraint
              <> poolParamsConstraint
              <> oracleConstraint
              <> operatorManagerNftConstraint
              <> operatorOracleNftConstraint
              <> operatorMigrationNftConstraint
              <> operatorPoolNftConstraint
      txId <- buildSignAndSubmitTx deployerSigningKey constraints
      MonadIO.liftIO $ putStrLn ("Initialize global state UTXOs with transaction id: " <> show txId)

run :: IO ()
run = do
  initializerConfigFile <- Environment.getEnv "INITIALIZER_CONFIG_FILE"
  contractExporterConfigFile <- Environment.getEnv "CONTRACT_EXPORTER_CONFIG_FILE"
  scriptDir <- Environment.getEnv "SCRIPT_DIR"
  cardanoNodeSocketPath <- Environment.getEnv "CARDANO_NODE_SOCKET_PATH"
  initializerConfig@InitializerConfig {networkId = NetworkIdText networkId} <-
    Yaml.decodeFileThrow initializerConfigFile
  let scriptHashMapFile = scriptDir </> scriptHashMapFileName
  loadedConfig <-
    LoadedConfig scriptDir initializerConfig
      <$> Yaml.decodeFileThrow contractExporterConfigFile
      <*> ((unBase16Plutus <$>) <$> Yaml.decodeFileThrow scriptHashMapFile)
      <*> Yaml.decodeFileThrow scriptHashMapFile
      <*> readPaymentSigningKey "DEPLOYER_SIGNING_KEY"
  Reader.runReaderT
    (deployGlobalStateUtxos loadedConfig)
    (CAE.mkNodeConnectInfo networkId cardanoNodeSocketPath)

main :: IO ()
main = runApp run
