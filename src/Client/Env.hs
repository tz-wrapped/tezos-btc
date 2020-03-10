{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Env
  ( AppEnv (..)
  , AppM
  , emptyEnv
  , runAppM
  ) where

import Tezos.Common.Json (TezosInt64)

import Client.Error
import Client.Types

data AppEnv = AppEnv
  { aeConfigOverride :: ConfigOverride
  , aeTezosClientPath :: Either TzbtcClientError FilePath
  , aeFees :: Maybe TezosInt64
  }

emptyEnv :: AppEnv
emptyEnv = AppEnv
  { aeConfigOverride = emptyConfigOverride
  , aeTezosClientPath = Left $ TzbtcTezosClientError "Tezos client path not available"
  , aeFees = Nothing
  }

type AppM = ReaderT AppEnv IO

runAppM :: AppEnv -> AppM a -> IO a
runAppM = flip runReaderT

emptyConfigOverride :: ConfigOverride
emptyConfigOverride  = ConfigOverride Nothing Nothing Nothing
