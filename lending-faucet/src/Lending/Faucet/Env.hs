{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | General module that contains type definitions
module Lending.Faucet.Env
  ( FaucetConfig (..)
  , AppEnv (..)
  , AppM
  , MintingToken
  )
where

import Cardano.Api qualified as CA
import Control.Concurrent (MVar)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import GHC.Generics (Generic)
import Servant (Handler)

import Cardano.Api.Extra.AssetId (AssetIdText)
import Cardano.Api.Extra.NetworkId (NetworkIdText)
import Cardano.Api.Extra.Nft (AssetNameReadable)
import Cardano.Index.Orphans ()
import Data.Aeson.Api (DefaultApiJson (DefaultApiJson), HasAesonOptions)
import Lending.Types.Orphans ()
import Plutarch.Extra.FixedDecimal (FixedDecimal)
import TxBuilder.Api qualified as TB

type MintingRange = (FixedDecimal 6, FixedDecimal 6)

type MintingToken = Map CA.AssetName MintingRange

-- | Faucet configuration, read from a local yaml file
data FaucetConfig = FaucetConfig
  { fcNetworkId :: NetworkIdText
  , fcNetworkParamsFetchDelay :: Int
  , fcMint :: Map AssetNameReadable MintingRange
  , fcSpent :: Map AssetIdText (FixedDecimal 6)
  , fcNumberOfTokensPerUser :: Int
  , fcCollateral :: FixedDecimal 6
  , fcFeeAndChange :: FixedDecimal 6
  , fcNft :: Map AssetNameReadable Aeson.Value
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (HasAesonOptions)
  deriving (FromJSON) via DefaultApiJson FaucetConfig

-- | The full environment type of the server.
-- contains all the data we need to run the faucet server successfully
data AppEnv = AppEnv
  { aeNodeConnection :: CA.LocalNodeConnectInfo CA.CardanoMode
  , aeNetworkParams :: MVar TB.NetworkParams
  , aeOperatorSigningKey :: CA.SigningKey CA.PaymentExtendedKey
  , aeFaucetUtxo :: MVar [CA.TxIn]
  , aeFaucetConfig :: FaucetConfig
  }

-- | Main type synonym used in the faucet code
type AppM = ReaderT AppEnv (LoggingT Handler)
