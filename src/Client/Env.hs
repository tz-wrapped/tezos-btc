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

import Morley.Client (MorleyClientEnv, MorleyClientM, runMorleyClientM)
import Morley.Client.Env (MorleyClientEnv'(..))
import Morley.Client.RPC.Class
import Morley.Client.TezosClient.Class
import Morley.Tezos.Core (Mutez)

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
  getBlockHash = morleyClientMToAppM ... getBlockHash
  getCounterAtBlock = morleyClientMToAppM ... getCounterAtBlock
  getBlockHeader = morleyClientMToAppM ... getBlockHeader
  getBlockConstants = morleyClientMToAppM ... getBlockConstants
  getBlockOperations = morleyClientMToAppM ... getBlockOperations
  getProtocolParametersAtBlock = morleyClientMToAppM ... getProtocolParametersAtBlock
  runOperationAtBlock = morleyClientMToAppM ... runOperationAtBlock
  preApplyOperationsAtBlock = morleyClientMToAppM ... preApplyOperationsAtBlock
  forgeOperationAtBlock = morleyClientMToAppM ... forgeOperationAtBlock
  injectOperation = morleyClientMToAppM ... injectOperation
  getContractScriptAtBlock = morleyClientMToAppM ... getContractScriptAtBlock
  getContractStorageAtBlock = morleyClientMToAppM ... getContractStorageAtBlock
  getContractBigMapAtBlock = morleyClientMToAppM ... getContractBigMapAtBlock
  getBigMapValueAtBlock = morleyClientMToAppM ... getBigMapValueAtBlock
  getBigMapValuesAtBlock = morleyClientMToAppM ... getBigMapValuesAtBlock
  getBalanceAtBlock = morleyClientMToAppM ... getBalanceAtBlock
  getDelegateAtBlock = morleyClientMToAppM ... getDelegateAtBlock
  runCodeAtBlock = morleyClientMToAppM ... runCodeAtBlock
  getChainId = morleyClientMToAppM ... getChainId
  getManagerKeyAtBlock = morleyClientMToAppM ... getManagerKeyAtBlock
  getScriptSizeAtBlock = morleyClientMToAppM ... getScriptSizeAtBlock
  getBlockOperationHashes = morleyClientMToAppM ... getBlockOperationHashes
  waitForOperation = \action -> do
    env <- ask
    morleyClientMToAppM $ waitForOperation $ liftIO $ runAppM env action

instance HasTezosClient AppM where
  signBytes = morleyClientMToAppM ... signBytes
  genKey = morleyClientMToAppM ... genKey
  revealKey = morleyClientMToAppM ... revealKey
  rememberContract = morleyClientMToAppM ... rememberContract
  genFreshKey = morleyClientMToAppM ... genFreshKey
  importKey = morleyClientMToAppM ... importKey
  resolveAddressMaybe = morleyClientMToAppM ... resolveAddressMaybe
  getAlias = morleyClientMToAppM ... getAlias
  getPublicKey = morleyClientMToAppM ... getPublicKey
  getTezosClientConfig = morleyClientMToAppM ... getTezosClientConfig
  calcTransferFee = morleyClientMToAppM ... calcTransferFee
  calcOriginationFee = morleyClientMToAppM ... calcOriginationFee
  getKeyPassword = morleyClientMToAppM ... getKeyPassword
  registerDelegate = morleyClientMToAppM ... registerDelegate
  getSecretKey = morleyClientMToAppM ... getSecretKey
