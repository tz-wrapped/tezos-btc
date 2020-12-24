{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{- | Some capability typeclasses that we use to abstract IO operations.

This enables us to isolate the rest of the logic and possibly write tests
for it.

-}
module Util.AbstractIO
  ( HasCmdLine(..)
  , HasConfig(..)
  , HasFilesystem(..)
  , HasTezosClient(..)
  , HasTezosRpc(..)
  , HasEnv(..)
  ) where

import Options.Applicative as Opt

import Morley.Micheline (Expression, TezosInt64)
import Tezos.Address
import Tezos.Core (ChainId)
import Tezos.Crypto

import Lorentz.Contracts.Multisig

import Client.Env
import Client.Error
import Client.Types
import Lorentz.Constraints
import Lorentz.Contracts.TZBTC (V1DeployParameters)
import Lorentz.Contracts.TZBTC.V2 (V2DeployParameters)

-- File system operations
class (Monad m) => HasFilesystem m  where
  writeFileUtf8 :: ToLText text => FilePath -> text -> m ()
  writeFile :: FilePath -> ByteString -> m ()
  readFile :: FilePath -> m ByteString
  doesFileExist :: FilePath -> m Bool

-- Config paths, reads and writes
class (Monad m) => HasConfig m  where
  readConfig :: m (Either TzbtcClientError ClientConfig)

-- Read arguments from cli, print messages out via cli
class (Monad m) => HasCmdLine m where
  parseCmdLine :: Opt.ParserInfo a -> m a
  printTextLn :: (Print text) => text -> m ()
  printStringLn :: String -> m ()
  printByteString :: ByteString -> m ()
  confirmAction :: Text -> m ConfirmationResult

-- Interaction with tezos node via RPC
class (HasTezosClient m, HasConfig m, Monad m, MonadThrow m) => HasTezosRpc m where
  runTransaction
    :: (NicePackedValue param)
    => Address
    -> EntrypointParam param
    -> TezosInt64 -- ^ Amount to transfer
    -> Maybe TezosInt64 -- ^ Optional baker fee to pay.
    -> m ()
  getStorage :: Text -> m Expression
  getCounter :: Text -> m TezosInt64
  getChainId :: Text -> m ChainId
  getFromBigMap :: Natural -> Text -> m (Either TzbtcClientError Expression)
  deployTzbtcContractV1 :: Maybe TezosInt64 -> V1DeployParameters -> m ()
  deployTzbtcContractV2 :: Maybe TezosInt64 -> V2DeployParameters -> m ()
  deployMultisigContract :: Maybe TezosInt64 -> MSigStorage -> Bool -> m ()

-- Interaction with tezos client binary
class (HasConfig m, HasEnv m, Monad m) => HasTezosClient m where
  getAddressAndPKForAlias :: Text -> m (Either TzbtcClientError (Address, PublicKey))
  getAddressForContract :: Text -> m (Either TzbtcClientError Address)
  signWithTezosClient :: Either ByteString Text -> Text -> m (Either Text Signature)
  waitForOperation :: Text -> m ()
  getTezosClientConfig ::
    m (Either Text
          (FilePath, TezosClientConfig)) -- File path is path to tezos-client executable
  rememberContractAs :: Address -> Text -> m ()

-- Interaction with environment variables
class (Monad m) => HasEnv m where
  lookupEnv :: m AppEnv
  withLocal :: (AppEnv -> AppEnv) -> m a -> m a
