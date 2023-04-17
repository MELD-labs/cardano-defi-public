module Lending.Index.Query.Types where

import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist.Sql (SqlBackend)

-- | `SqlM` resembles @persistent@'s `SqlPersistM` but swaps out `NoLoggingT` for `LoggingT`.
type SqlM = ReaderT SqlBackend (ResourceT (LoggingT IO))
