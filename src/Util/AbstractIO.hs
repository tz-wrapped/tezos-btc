{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{- | Some capability typeclasses that we use to abstract IO operations.

This enables us to isolate the rest of the logic and possibly write tests
for it.

-}
module Util.AbstractIO
  ( HasCmdLine(..)
  , HasConfig(..)
  , HasEditor(..)
  , HasFilesystem(..)
  , HasTezosClient(..)
  , HasTezosRpc(..)
  ) where

import Options.Applicative as Opt

import Tezos.Address
import Tezos.Common.Json (TezosInt64)
import Tezos.Crypto
import Tezos.V005.Micheline (Expression)

import Client.Error
import Client.Types
import Lorentz.Constraints
import Lorentz.Contracts.TZBTC (OriginationParameters)

-- File system operations
class (Monad m) => HasFilesystem m  where
  writeFileUtf8 :: Print text => FilePath -> text -> m ()
  writeFile :: FilePath -> ByteString -> m ()
  readFile :: FilePath -> m ByteString
  doesFileExist :: FilePath -> m Bool
  createDirectoryIfMissing :: Bool -> DirPath -> m ()
  getConfigPaths :: m (DirPath, FilePath)

-- Config paths, reads and writes
class (Monad m, HasFilesystem m) => HasConfig m  where
  readConfig :: m (Either TzbtcClientError ClientConfig)
  readConfigText :: m (Either TzbtcClientError ClientConfigText)
  writeConfigFull :: ClientConfig -> m ()
  writeConfigPartial :: ClientConfigPartial -> m ()
  writeConfigText :: ClientConfigText -> m ()

-- Read arguments from cli, print messages out via cli
class (Monad m) => HasCmdLine m where
  parseCmdLine :: Opt.ParserInfo a -> m a
  printTextLn :: (Print text) => text -> m ()
  printStringLn :: String -> m ()
  printByteString :: ByteString -> m ()
  confirmAction :: Text -> m ConfirmationResult

-- Interaction with tezos node via RPC
class (HasTezosClient m, HasConfig m, Monad m, MonadThrow m) => HasTezosRpc m where
  runTransactions :: (NicePackedValue param) => Address -> [EntrypointParam param] -> m ()
  getStorage :: Text -> m Expression
  getCounter :: Text -> m TezosInt64
  getFromBigMap :: Natural -> Text -> m (Either TzbtcClientError Expression)
  deployTzbtcContract :: OriginationParameters -> m ()

-- Interaction with tezos client binary
class (HasConfig m, Monad m) => HasTezosClient m where
  getAddressAndPKForAlias :: Text -> m (Either TzbtcClientError (Address, PublicKey))
  signWithTezosClient :: Either ByteString Text -> m (Either Text Signature)
  waitForOperation :: Text -> m ()

-- Interaction with a text editor
class (Monad m) => HasEditor m where
  openEditor :: FilePath -> (ByteString -> m ()) -> m ()
