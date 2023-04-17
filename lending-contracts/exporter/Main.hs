{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Aeson (FromJSON)
import Data.Coerce qualified as Coerce
import Data.Foldable qualified as Foldable
import Data.Yaml qualified as Yaml
import Options.Applicative qualified as Opt
import PlutusLedgerApi.V2 qualified as Plutus

import Lending.Contracts.Account.OffChain (calculateLiquidationResultOffChain)
import Lending.Contracts.Account.OnChain (accountValidatorTerm)
import Lending.Contracts.AccountAuthToken.OnChain (accountPolicyTerm)
import Lending.Contracts.AlwaysFalse (alwaysFalseValidator)
import Lending.Contracts.AlwaysFalseToken (alwaysFalsePolicyTerm)
import Lending.Contracts.Manager.OnChain (managerValidator)
import Lending.Contracts.Nft (nftPolicyTerm, unappliedNftPolicyTerm)
import Lending.Contracts.OffChain (calculateAccountValueOffChain)
import Lending.Contracts.Oracle.OnChain (oracleValidatorTerm)
import Lending.Contracts.OracleCheckerToken.OnChain (oracleCheckerPolicyTerm)
import Lending.Contracts.Orphans ()
import Lending.Contracts.Pool.InterestRate
  ( calculateBorrowingRate
  , calculateLendingRate
  , calculateUtilizationRatio
  , updateCumulatedRates
  )
import Lending.Contracts.Pool.OffChain (processAccountOffChain, updatePoolOffChain)
import Lending.Contracts.Pool.OnChain (validatePoolTerm)
import Lending.Functions
  ( LendingFunction
      ( LendingFunctionBorrowApy
      , LendingFunctionCalculateAccountValue
      , LendingFunctionCalculateLiquidationResult
      , LendingFunctionInterestRate
      , LendingFunctionProcessAccount
      , LendingFunctionSupplyApy
      , LendingFunctionUpdatePool
      , LendingFunctionUtilization
      )
  )
import Lending.Scripts
  ( LendingScript
      ( Account
      , AccountAuthToken
      , AccountOwnerNft
      , AlwaysFalse
      , AlwaysFalseToken
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
  )
import Lending.Types.Manager (ManagerScriptParams (ManagerScriptParams))
import Lending.Types.Oracle (OracleScriptParams (OracleScriptParams))
import Plutarch.Export.Scripts
  ( Compiled (Compiled, compiledScriptHash)
  , buildFunction
  , buildScript
  , writeCompiled
  , writeScriptHashMap
  )
import Plutarch.Export.TxOutRefJson (TxOutRefJson (TxOutRefJson))
import Plutarch.Extra.AssetClass (AssetClass (AssetClass))

data Arguments = Arguments
  { configFile :: FilePath
  , outputDir :: FilePath
  }

data Config = Config
  { rootOperatorManagerNftUtxo :: TxOutRefJson
  , rootOperatorOracleNftUtxo :: TxOutRefJson
  , rootOperatorMigrationNftUtxo :: TxOutRefJson
  , rootOperatorPoolNftUtxo :: TxOutRefJson
  , rootManagerAuthTokenUtxo :: TxOutRefJson
  , rootOracleAuthTokenUtxo :: TxOutRefJson
  , rootPoolAuthTokenUtxo :: TxOutRefJson
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

pConfigFile :: Opt.Parser FilePath
pConfigFile =
  Opt.strOption
    ( Opt.long "config"
        <> Opt.short 'c'
        <> Opt.help "Config file."
    )

pOutputDir :: Opt.Parser FilePath
pOutputDir =
  Opt.strOption
    ( Opt.long "output"
        <> Opt.short 'o'
        <> Opt.help "Output directory."
    )

loadArguments :: IO Arguments
loadArguments = do
  Opt.execParser $
    Opt.info
      (Arguments <$> pConfigFile <*> pOutputDir)
      (Opt.progDesc "export-contracts" <> Opt.fullDesc)

emptyTokenName :: Compiled -> AssetClass
emptyTokenName Compiled {compiledScriptHash} = AssetClass (Coerce.coerce compiledScriptHash) ""

main :: IO ()
main = do
  Arguments {configFile, outputDir} <- loadArguments
  Config
    { rootOperatorManagerNftUtxo = TxOutRefJson operatorManagerNftTxOutRef
    , rootOperatorOracleNftUtxo = TxOutRefJson operatorOracleNftTxOutRef
    , rootOperatorMigrationNftUtxo = TxOutRefJson operatorMigrationNftTxOutRef
    , rootOperatorPoolNftUtxo = TxOutRefJson operatorPoolNftTxOutRef
    , rootManagerAuthTokenUtxo = TxOutRefJson rootManagerAuthTokenTxOutRef
    , rootOracleAuthTokenUtxo = TxOutRefJson rootOracleAuthTokenTxOutRef
    , rootPoolAuthTokenUtxo = TxOutRefJson rootPoolAuthTokenTxOutRef
    } <-
    Yaml.decodeFileThrow configFile
  migOperator <- buildScript OperatorMigrationNft $ nftPolicyTerm operatorMigrationNftTxOutRef

  mOperator <- buildScript OperatorManagerNft $ nftPolicyTerm operatorManagerNftTxOutRef
  mAuthToken <- buildScript ManagerAuthToken $ nftPolicyTerm rootManagerAuthTokenTxOutRef
  mValidator <-
    buildScript Manager $
      managerValidator (ManagerScriptParams (emptyTokenName migOperator) (emptyTokenName mAuthToken))

  oOperator <- buildScript OperatorOracleNft $ nftPolicyTerm operatorOracleNftTxOutRef
  oAuthToken <- buildScript OracleAuthToken $ nftPolicyTerm rootOracleAuthTokenTxOutRef
  oValidator <- buildScript Oracle $ oracleValidatorTerm (OracleScriptParams (emptyTokenName oOperator))
  oracleCheckerAuthToken <- buildScript OracleCheckerToken $ oracleCheckerPolicyTerm (emptyTokenName oAuthToken)

  aValidator <- buildScript Account $ accountValidatorTerm (emptyTokenName mAuthToken) (emptyTokenName migOperator)
  aAuthToken <-
    buildScript AccountAuthToken $ accountPolicyTerm (emptyTokenName mAuthToken) (emptyTokenName migOperator)

  pOperator <- buildScript OperatorPoolNft $ nftPolicyTerm operatorPoolNftTxOutRef
  pAuthToken <- buildScript PoolAuthToken $ nftPolicyTerm rootPoolAuthTokenTxOutRef
  pValidator <- buildScript Pool $ validatePoolTerm (emptyTokenName mAuthToken) (emptyTokenName migOperator)

  alwaysFalse <- buildScript AlwaysFalse alwaysFalseValidator
  alwaysFalseToken <- buildScript AlwaysFalseToken alwaysFalsePolicyTerm

  let scripts =
        [ mValidator
        , mAuthToken
        , oValidator
        , oAuthToken
        , aValidator
        , aAuthToken
        , pValidator
        , pAuthToken
        , alwaysFalse
        , alwaysFalseToken
        , mOperator
        , oOperator
        , migOperator
        , pOperator
        , oracleCheckerAuthToken
        ]
  functions <-
    sequence
      [ buildFunction LendingFunctionInterestRate updateCumulatedRates
      , buildFunction LendingFunctionProcessAccount processAccountOffChain
      , buildFunction LendingFunctionUpdatePool updatePoolOffChain
      , buildFunction LendingFunctionUtilization calculateUtilizationRatio
      , buildFunction LendingFunctionBorrowApy calculateBorrowingRate
      , buildFunction LendingFunctionSupplyApy calculateLendingRate
      , buildFunction LendingFunctionCalculateLiquidationResult calculateLiquidationResultOffChain
      , buildFunction LendingFunctionCalculateAccountValue calculateAccountValueOffChain
      ]
  writeScriptHashMap outputDir scripts
  Foldable.traverse_ (writeCompiled outputDir) (scripts <> functions)
  buildScript AccountOwnerNft unappliedNftPolicyTerm >>= writeCompiled outputDir
