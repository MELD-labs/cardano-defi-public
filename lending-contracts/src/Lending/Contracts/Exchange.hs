{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lending.Contracts.Exchange
  ( PActual
  , PReceipt
  , PFiat
  , PDecimal
  , PCumulativeRate
  , PPrice
  , PLtvRatio
  , convertFrom
  , convertTo
  )
where

import GHC.TypeLits (KnownNat)
import Plutarch.Extra.ExchangeRate (exchangeFromTruncate, exchangeToTruncate, (:>))
import Plutarch.Extra.FixedDecimal (PFixedDecimal, ptoRational)
import Plutarch.Extra.Functor ((#<$>))
import Plutarch.Extra.Tagged (PTagged)

type PActual = PTagged "Actual" PInteger

type PReceipt = PTagged "Receipt" PInteger

type PFiat = PTagged "Fiat" PInteger

type PDecimal = PFixedDecimal 18

type PCumulativeRate = PTagged ("Receipt" :> "Actual") PDecimal

type PPrice = PTagged ("Actual" :> "Fiat") (PFixedDecimal 18)

type PLtvRatio = PTagged ("Fiat" :> "Fiat") (PFixedDecimal 18)

convertFrom
  :: KnownNat u
  => Term s (PTagged (a :> b) (PFixedDecimal u) :--> PTagged a PInteger :--> PTagged b PInteger)
convertFrom = phoistAcyclic $ plam \rate receipt ->
  exchangeFromTruncate # (ptoRational #<$> rate) # receipt

convertTo
  :: KnownNat u
  => Term s (PTagged (a :> b) (PFixedDecimal u) :--> PTagged b PInteger :--> PTagged a PInteger)
convertTo = phoistAcyclic $ plam \rate receipt ->
  exchangeToTruncate # (ptoRational #<$> rate) # receipt
