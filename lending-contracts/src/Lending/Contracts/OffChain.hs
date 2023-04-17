{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Contracts.OffChain
  ( calculateAccountValueOffChain
  )
where

import Plutarch.Api.V1.Value (pforgetPositive)
import Plutarch.Api.V2 (AmountGuarantees (NonZero), KeyGuarantees (Sorted), PValue)

import Lending.Contracts.Account.Types (PAccountDatum)
import Lending.Contracts.Manager.Types (PManagerDatum)
import Lending.Contracts.Request (calculateAccountValue)

calculateAccountValueOffChain
  :: Term
      s
      ( PManagerDatum :--> PAccountDatum :--> PValue 'Sorted 'NonZero
      )
calculateAccountValueOffChain =
  phoistAcyclic $ plam $ \managerDatum accountDatum -> P.do
    pforgetPositive $
      calculateAccountValue
        # managerDatum
        # accountDatum
