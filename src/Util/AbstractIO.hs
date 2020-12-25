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
  , HasFilesystem(..)
  , HasEnv(..)
  ) where

import Client.Env
import Client.Types

-- File system operations
class (Monad m) => HasFilesystem m  where
  writeFileUtf8 :: ToLText text => FilePath -> text -> m ()
  writeFile :: FilePath -> ByteString -> m ()
  readFile :: FilePath -> m ByteString
  doesFileExist :: FilePath -> m Bool

-- Read arguments from cli, print messages out via cli
class (Monad m) => HasCmdLine m where
  printTextLn :: (Print text) => text -> m ()
  printStringLn :: String -> m ()
  printByteString :: ByteString -> m ()
  confirmAction :: Text -> m ConfirmationResult

-- Interaction with environment variables
class (Monad m) => HasEnv m where
  lookupEnv :: m AppEnv
  withLocal :: (AppEnv -> AppEnv) -> m a -> m a
