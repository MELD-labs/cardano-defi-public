module Lending.Contracts.AlwaysFalse (alwaysFalseValidator) where

import Plutarch.Api.V2 (PValidator)

alwaysFalseValidator :: ClosedTerm PValidator
alwaysFalseValidator = phoistAcyclic $ plam $ \_ _ -> perror
