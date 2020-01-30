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

import Client.Error
import Client.Types

data AppEnv = AppEnv
  { aeConfigOverride :: ConfigOverride
  , aeTezosClientPath :: Either TzbtcClientError FilePath
  }

emptyEnv :: AppEnv
emptyEnv = AppEnv
  { aeConfigOverride = emptyConfigOverride
  , aeTezosClientPath = Left $ TzbtcTezosClientError "Tezos client path not available"
  }

type AppM = ReaderT AppEnv IO

runAppM :: AppEnv -> AppM a -> IO a
runAppM = flip runReaderT

emptyConfigOverride :: ConfigOverride
emptyConfigOverride  = ConfigOverride Nothing
