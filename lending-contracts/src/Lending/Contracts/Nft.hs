{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

module Lending.Contracts.Nft
  ( nftPolicyTerm
  , unappliedNftPolicyTerm
  )
where

import Plutarch.Api.V2 (PMintingPolicy, PTxOutRef)
import Plutarch.Extra.StateThread qualified as StateThread
import PlutusLedgerApi.V2 qualified as Plutus

validateNftMinting :: Term s PMintingPolicy
validateNftMinting = phoistAcyclic $ plam $ \_ _ -> popaque $ pconstant ()

nftPolicyTerm :: Plutus.TxOutRef -> ClosedTerm PMintingPolicy
nftPolicyTerm txOutRef = StateThread.withStateThread validateNftMinting (pconstant txOutRef)

unappliedNftPolicyTerm :: ClosedTerm (PTxOutRef :--> PMintingPolicy)
unappliedNftPolicyTerm = StateThread.pwithStateThread # validateNftMinting
