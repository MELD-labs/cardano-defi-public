{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

module Lending.Contracts.AlwaysFalseToken
  ( alwaysFalseTokenName
  , alwaysFalsePolicyTerm
  )
where

import Plutarch.Api.V2 (PMintingPolicy)
import PlutusLedgerApi.V2 qualified as Plutus

alwaysFalseTokenName :: Plutus.TokenName
alwaysFalseTokenName = ""

alwaysFalsePolicyTerm :: ClosedTerm PMintingPolicy
alwaysFalsePolicyTerm =
  plam $ \_ _ -> popaque perror
