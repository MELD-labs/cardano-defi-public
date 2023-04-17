{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Map (specs) where

import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (KeyGuarantees (Sorted), PMap)
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Extra.Function (pidentity)
import Plutarch.Extra.These (pdthese)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

import Lending.Contracts.Map (punionWithThese)

specs :: TestTree
specs =
  Tasty.testGroup
    "Extra function for AssocMap"
    [ Tasty.testCase "punionWithThese" $ do
        let left :: Term s (PMap 'Sorted PInteger PByteString)
            left =
              AssocMap.pfromAscList #$
                pcons # (ppairDataBuiltin # pdata 1 # pdata (pconstant "abc")) #$
                  pcons
                    # (ppairDataBuiltin # pdata 2 # pdata (pconstant "xyztuv"))
                    # pnil
            right :: Term s (PMap 'Sorted PInteger PInteger)
            right =
              AssocMap.pfromAscList #$
                pcons # (ppairDataBuiltin # pdata 2 # pdata 10) #$
                  pcons # (ppairDataBuiltin # pdata 3 # pdata 20) #$
                    pcons
                      # (ppairDataBuiltin # pdata 4 # pdata 30)
                      # pnil
            combine = pdthese # plengthBS # pidentity #$ plam $ \bs int -> plengthBS # bs + int
            actual = punionWithThese # combine # left # right
            expected :: Term s (PMap 'Sorted PInteger PInteger)
            expected =
              AssocMap.pfromAscList #$
                pcons # (ppairDataBuiltin # pdata 1 # pdata 3) #$
                  pcons # (ppairDataBuiltin # pdata 2 # pdata 16) #$
                    pcons # (ppairDataBuiltin # pdata 3 # pdata 20) #$
                      pcons
                        # (ppairDataBuiltin # pdata 4 # pdata 30)
                        # pnil
        Tasty.assertBool "should return expected union" (plift (actual #== expected))
    ]
