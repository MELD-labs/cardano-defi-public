{-# LANGUAGE DerivingStrategies #-}

module Lending.Core.Errors
  ( ScriptError (ScriptError)
  , throwE
  , eitherE
  , maybeE
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)

throwE :: (MonadThrow m, Exception e) => (a -> e) -> a -> m b
throwE handle = throwM . handle

eitherE :: (MonadThrow m, Exception e) => (l -> e) -> Either l a -> m a
eitherE handle = either (throwE handle) pure

maybeE :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeE err = maybe (throwE id err) pure

newtype ScriptError = ScriptError String
  deriving stock (Eq, Show)
