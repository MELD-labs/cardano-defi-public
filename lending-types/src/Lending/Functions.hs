{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Lending.Functions (LendingFunction (..)) where

import PlutusLedgerApi.V2 (TxOut, Value)
import PlutusTx.AssocMap qualified as AssocMap

import Lending.Types.Account (AccountDatum, AccountLiquidateRedeemerData)
import Lending.Types.Account.OffChain (LiquidationResultData)
import Lending.Types.Asset (Asset)
import Lending.Types.Exchange (Actual, CumulativeRate, Decimal, Price)
import Lending.Types.Manager (ManagerDatum, RiskParameters)
import Lending.Types.Orphans ()
import Lending.Types.Pool (AssetInformation)
import Lending.Types.Pool.OffChain (MatcherContextData, RequestResultData, StateChangeData)
import Plutarch.Converter.Functions (IsFunction (Args, Res))

data LendingFunction a b where
  LendingFunctionInterestRate
    :: LendingFunction
        [ AssocMap.Map Asset RiskParameters
        , AssocMap.Map Asset AssetInformation
        , Integer
        ]
        (AssocMap.Map Asset AssetInformation)
  LendingFunctionProcessAccount :: LendingFunction [MatcherContextData, TxOut] RequestResultData
  LendingFunctionUpdatePool
    :: LendingFunction
        [ AssocMap.Map Asset RiskParameters
        , StateChangeData
        , Integer
        , AssocMap.Map Asset AssetInformation
        ]
        (AssocMap.Map Asset AssetInformation)
  LendingFunctionUtilization :: LendingFunction [Actual, Actual] Decimal
  LendingFunctionBorrowApy
    :: LendingFunction
        [ RiskParameters
        , Decimal
        ]
        CumulativeRate
  LendingFunctionSupplyApy
    :: LendingFunction
        [ RiskParameters
        , CumulativeRate
        , Decimal
        ]
        CumulativeRate
  LendingFunctionCalculateLiquidationResult
    :: LendingFunction
        [ ManagerDatum
        , AssocMap.Map Asset Price
        , AssocMap.Map Asset AssetInformation
        , AccountDatum
        , AccountLiquidateRedeemerData
        ]
        LiquidationResultData
  LendingFunctionCalculateAccountValue
    :: LendingFunction
        [ ManagerDatum
        , AccountDatum
        ]
        Value

instance Show (LendingFunction a b) where
  show LendingFunctionInterestRate = "LendingFunctionInterestRate"
  show LendingFunctionProcessAccount = "LendingFunctionProcessAccount"
  show LendingFunctionUpdatePool = "LendingFunctionUpdatePool"
  show LendingFunctionUtilization = "LendingFunctionUtilization"
  show LendingFunctionBorrowApy = "LendingFunctionBorrowApy"
  show LendingFunctionSupplyApy = "LendingFunctionSupplyApy"
  show LendingFunctionCalculateLiquidationResult = "LendingFunctionCalculateLiquidationResult"
  show LendingFunctionCalculateAccountValue = "LendingFunctionCalculateAccountValue"

instance IsFunction (k :: LendingFunction a b) where
  type Args (k :: LendingFunction a b) = a
  type Res (k :: LendingFunction a b) = b
