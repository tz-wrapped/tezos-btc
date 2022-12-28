{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Client.Env
  ( AppEnv (..)
  , AppM (..)
  , TzbtcClientEnv (..)
  , ExtTezosClient (..)
  , emptyConfigOverride
  , emptyEnv
  , runAppM
  ) where

import Colog (HasLog(..), Message, hoistLogAction)

import Morley.Client (MorleyClientEnv, MorleyClientM, runMorleyClientM)
import Morley.Client.Full (MorleyClientEnv(..))
import Morley.Client.RPC.Class
import Morley.Client.TezosClient.Class
import Morley.Client.TezosClient.Impl qualified as Impl
import Morley.Client.TezosClient.Types
import Morley.Tezos.Address.Alias
import Morley.Tezos.Core (Mutez)
import Morley.Tezos.Crypto

import Client.Types

data AppEnv = AppEnv
  { aeConfigOverride :: ConfigOverride
  , aeFees :: Maybe Mutez
  }

data TzbtcClientEnv = TzbtcClientEnv
  { tzbtcEnv :: AppEnv
  , mcEnv :: MorleyClientEnv
  }

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
  getLogAction (TzbtcClientEnv _ mcEnv) =
    hoistLogAction (liftToAppM mcEnv) $ mceLogAction mcEnv
  setLogAction action env@(TzbtcClientEnv tzbtcEnv mcEnv) =
    TzbtcClientEnv tzbtcEnv $ mcEnv {mceLogAction = hoistLogAction (unliftFromAppM env) action}

liftToAppM :: MorleyClientEnv -> MorleyClientM a -> AppM a
liftToAppM mcEnv action = AppM $ lift $ runMorleyClientM mcEnv action

unliftFromAppM :: MonadIO m => TzbtcClientEnv -> AppM a -> m a
unliftFromAppM env action = liftIO $ runAppM env action

runAppM :: TzbtcClientEnv -> AppM a -> IO a
runAppM tzbtcClientEnv action =
  runReaderT (unAppM action) tzbtcClientEnv

morleyClientMToAppM :: MorleyClientM a -> AppM a
morleyClientMToAppM action =
  AppM $ ReaderT $ \(TzbtcClientEnv _ mcEnv) -> runMorleyClientM mcEnv action

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

class ExtTezosClient m where
  getPublicKey :: ImplicitAddressOrAlias -> m PublicKey
  -- ^ Get public key for given address. Public keys are often used when interacting
  -- with the multising contracts
  getTezosClientConfig :: m TezosClientConfig
  -- ^ Retrieve the current @tezos-client@ config.

instance HasTezosClient AppM where
  signBytes = morleyClientMToAppM ... signBytes
  genKey = morleyClientMToAppM ... genKey
  revealKey = morleyClientMToAppM ... revealKey
  rememberContract = morleyClientMToAppM ... rememberContract
  genFreshKey = morleyClientMToAppM ... genFreshKey
  getAliasesAndAddresses = morleyClientMToAppM ... getAliasesAndAddresses
  getKeyPassword = morleyClientMToAppM ... getKeyPassword

instance ExtTezosClient AppM where
  getPublicKey = morleyClientMToAppM ... Impl.getPublicKey
  getTezosClientConfig = do
    tce <- view tezosClientEnvL <$> asks mcEnv
    let path = tceTezosClientPath tce
        mbDataDir = tceMbTezosClientDataDir tce
    liftIO $ Impl.getTezosClientConfig path mbDataDir
