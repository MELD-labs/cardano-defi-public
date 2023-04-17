{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lending.Core.Api where

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Control.Exception.Safe (handleAny)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.List.Extra (drop1)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Servant qualified as S

-- | Error that is returned from api handlers
data Err = Err
  { errTitle :: Text
  -- ^ error title
  , errMsg :: Text
  -- ^ actual error message
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AnyServerError = forall e. (Exception e) => AnyServerError e
  deriving anyclass (Exception)

instance Show AnyServerError where
  show (AnyServerError e) = show e

-- | alter 'ServerError' with user provided error body
appToErr :: S.ServerError -> Text -> S.ServerError
appToErr x msg =
  x
    { S.errBody = A.encode $ Err (T.pack $ S.errReasonPhrase x) msg
    , S.errHeaders = [("Content-Type", "application/json")]
    }

extractUserResponse :: String -> String
extractUserResponse logs =
  if '@' `elem` logs
    then do
      let rLogs = reverse logs
          rest = drop1 $ dropWhile (/= '@') rLogs
          result = takeWhile (/= '@') rest
       in reverse result
    else logs

handleError :: S.Handler a -> S.Handler a
handleError = handleAny throwEx
  where
    throwEx e = do
      liftIO $ print ("Error:" <> show e)
      let baseError = maybe S.err400 (const S.err500) (Exception.fromException @AnyServerError e)
      S.throwError $ appToErr baseError (T.pack . extractUserResponse $ show e)

utxoCountLimit :: Int
utxoCountLimit = 200
