{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module Lending.Contracts.Map (punionWithThese) where

import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (KeyGuarantees (Sorted), PMap)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.These (PDThese (PDThat, PDThese, PDThis))
import Plutarch.Monadic qualified as P

buildThese :: Term s (PAsData a) -> Term s (PAsData b) -> Term s (PDThese a b)
buildThese x y =
  mkRecordConstr PDThese $
    #_0 .= x
      .& #_1 .= y

combineThese :: Term s (PDThese a b :--> PDThese a b :--> PDThese a b)
combineThese = phoistAcyclic $ plam $ \left' right' -> P.do
  left <- pmatch left'
  right <- pmatch right'
  case (left, right) of
    (PDThis x, PDThat y) -> buildThese (pfield @"_0" # x) (pfield @"_0" # y)
    (PDThat y, PDThis x) -> buildThese (pfield @"_0" # x) (pfield @"_0" # y)
    _ -> ptraceError "Unexpected behaviour of punionWithThese"

buildLeftRight
  :: PIsData c
  => (forall s'. Term s' (PDataRecord '["_0" ':= c]) -> PDThese a b s')
  -> Term s (c :--> PDThese a b)
buildLeftRight constr = phoistAcyclic $ plam $ \c ->
  mkRecordConstr constr $
    #_0 .= pdata c

punionWithThese
  :: (POrd k, PIsData k, PIsData a, PIsData b, PIsData c)
  => Term s ((PDThese a b :--> c) :--> PMap 'Sorted k a :--> PMap 'Sorted k b :--> PMap 'Sorted k c)
punionWithThese = phoistAcyclic $ plam $ \f left right -> P.do
  leftThis <- plet $ AssocMap.pmap # buildLeftRight PDThis # left
  rightThat <- plet $ AssocMap.pmap # buildLeftRight PDThat # right
  allThese <- plet $ AssocMap.punionWith # combineThese # leftThis # rightThat
  AssocMap.pmap # f # allThese
