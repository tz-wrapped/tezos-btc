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
import Morley.Tezos.Core (Mutez)

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
  getTicketBalanceAtBlock = morleyClientMToAppM ... getTicketBalanceAtBlock
  getAllTicketBalancesAtBlock = morleyClientMToAppM ... getAllTicketBalancesAtBlock

class ExtTezosClient m where
  getTezosClientConfig :: m TezosClientConfig
  -- ^ Retrieve the current @octez-client@ config.

instance HasTezosClient AppM where
  signBytes = morleyClientMToAppM ... signBytes
  genKey = morleyClientMToAppM ... genKey
  rememberContract = morleyClientMToAppM ... rememberContract
  genFreshKey = morleyClientMToAppM ... genFreshKey
  getAliasesAndAddresses = morleyClientMToAppM ... getAliasesAndAddresses
  getKeyPassword = morleyClientMToAppM ... getKeyPassword
  getPublicKey = morleyClientMToAppM ... getPublicKey

instance ExtTezosClient AppM where
  getTezosClientConfig = do
    tce <- view tezosClientEnvL <$> asks mcEnv
    let path = tceTezosClientPath tce
        mbDataDir = tceMbTezosClientDataDir tce
    liftIO $ Impl.getTezosClientConfig path mbDataDir
