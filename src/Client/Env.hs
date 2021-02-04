{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Client.Env
  ( AppEnv (..)
  , AppM (..)
  , TzbtcClientEnv' (..)
  , emptyConfigOverride
  , emptyEnv
  , runAppM
  ) where

import Colog (HasLog(..), LogAction(..), Message)

import Morley.Client.App (MorleyClientEnv, MorleyClientM, runMorleyClientM)
import Morley.Client.Env (MorleyClientEnv'(..))
import Morley.Client.RPC.Class
import Morley.Client.TezosClient.Class
import Tezos.Core (Mutez)

import Client.Types

data AppEnv = AppEnv
  { aeConfigOverride :: ConfigOverride
  , aeFees :: Maybe Mutez
  }

data TzbtcClientEnv' m = TzbtcClientEnv
  { tzbtcEnv :: AppEnv
  , mcEnv :: (MorleyClientEnv' m)
  }

type TzbtcClientEnv = TzbtcClientEnv' AppM

emptyEnv :: AppEnv
emptyEnv = AppEnv
  { aeConfigOverride = emptyConfigOverride
  , aeFees = Nothing
  }

newtype AppM a = AppM { unAppM :: ReaderT TzbtcClientEnv IO a }
  deriving newtype
    ( Functor, Applicative, Monad, MonadReader TzbtcClientEnv
    , MonadIO, MonadThrow, MonadCatch, MonadMask
    )

instance HasLog TzbtcClientEnv Message AppM where
  getLogAction (TzbtcClientEnv _ mcEnv) = mceLogAction mcEnv
  setLogAction action (TzbtcClientEnv tzbtcEnv mcEnv) =
    TzbtcClientEnv tzbtcEnv $ mcEnv {mceLogAction = action}

runAppM :: TzbtcClientEnv -> AppM a -> IO a
runAppM tzbtcClientEnv action =
  runReaderT (unAppM action) tzbtcClientEnv

morleyClientMToAppM :: MorleyClientM a -> AppM a
morleyClientMToAppM action =
  AppM $ ReaderT $ \(TzbtcClientEnv _ mcEnv) -> runMorleyClientM (convertEnv mcEnv) action
  where
    convertEnv :: MorleyClientEnv' AppM -> MorleyClientEnv
    convertEnv env = env
      { mceLogAction = LogAction $ \msg -> appMToMorleyClientM $
        unLogAction (mceLogAction env) msg
      }
    convertEnv' :: MorleyClientEnv -> MorleyClientEnv' AppM
    convertEnv' env = env
      { mceLogAction = LogAction $ \msg -> morleyClientMToAppM $
        unLogAction (mceLogAction env) msg
      }
    appMToMorleyClientM :: AppM a -> MorleyClientM a
    appMToMorleyClientM action' = do
      mcEnv <- ask
      liftIO $ runAppM (TzbtcClientEnv emptyEnv (convertEnv' mcEnv)) action'

emptyConfigOverride :: ConfigOverride
emptyConfigOverride  = ConfigOverride Nothing Nothing Nothing

instance HasTezosRpc AppM where
  getHeadBlock = morleyClientMToAppM getHeadBlock
  getCounter = morleyClientMToAppM ... getCounter
  getBlockConstants = morleyClientMToAppM ... getBlockConstants
  getProtocolParameters = morleyClientMToAppM getProtocolParameters
  runOperation = morleyClientMToAppM ... runOperation
  preApplyOperations = morleyClientMToAppM ... preApplyOperations
  forgeOperation = morleyClientMToAppM ... forgeOperation
  injectOperation = morleyClientMToAppM ... injectOperation
  getContractScript = morleyClientMToAppM ... getContractScript
  getContractStorage = morleyClientMToAppM ... getContractStorage
  getContractBigMap = morleyClientMToAppM ... getContractBigMap
  getBigMapValue = morleyClientMToAppM ... getBigMapValue
  getBalance = morleyClientMToAppM ... getBalance
  runCode = morleyClientMToAppM ... runCode
  getChainId = morleyClientMToAppM getChainId

instance HasTezosClient AppM where
  signBytes = morleyClientMToAppM ... signBytes
  genKey = morleyClientMToAppM ... genKey
  revealKey = morleyClientMToAppM ... revealKey
  waitForOperation = morleyClientMToAppM ... waitForOperation
  rememberContract = morleyClientMToAppM ... rememberContract
  genFreshKey = morleyClientMToAppM ... genFreshKey
  importKey = morleyClientMToAppM ... importKey
  resolveAddressMaybe = morleyClientMToAppM ... resolveAddressMaybe
  getAlias = morleyClientMToAppM ... getAlias
  getPublicKey = morleyClientMToAppM ... getPublicKey
  getTezosClientConfig = morleyClientMToAppM ... getTezosClientConfig
  calcTransferFee = morleyClientMToAppM ... calcTransferFee
  calcOriginationFee = morleyClientMToAppM ... calcOriginationFee
