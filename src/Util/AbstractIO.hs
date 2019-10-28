{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Util.AbstractIO
  ( HasFilesystem(..)
  , HasTezosRpc(..)
  , HasConfig(..)
  , HasTezosClient(..)
  , HasCmdLine(..)
  , HasEditor(..)
  , DirPath(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (cons)
import qualified Data.Aeson as Aeson (decodeFileStrict)
import Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC (putStrLn)
import qualified System.Directory as Directory (doesFileExist, createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserConfigFile)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Michelson.Untyped (InternalByteString(..))
import Servant.Client
  (BaseUrl(..), ClientError(..), ClientEnv, ResponseF(..), Scheme(..), mkClientEnv,
  runClientM)
import Network.HTTP.Client
  (ManagerSettings(..), Request(..), newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (notFound404)
import Michelson.Runtime.GState (genesisAddress1, genesisAddress2)
import Michelson.Interpret.Unpack (UnpackError)


import Tezos.Address
import Tezos.Micheline (Expression)
import Tezos.Crypto
import Tezos.Json (TezosWord64(..))

import Client.Error
import Client.Types
import Client.Util
import Client.Crypto
import Client.Parser
import qualified Client.API as API
import Util.MultiSig
import Lorentz.Contracts.TZBTC (fromFlatParameter, Interface, FlatParameter(..), SafeParameter, Parameter)
import qualified Util.IO as UIO (writeFileUtf8)
import Util.Editor

newtype DirPath = DirPath { unDirPath :: FilePath }

-- Some capability typeclasses that we use to abstract IO operations
-- so that we can isolate the rest of the logic and possibly test it.

-- File system operations
class (Monad m) => HasFilesystem m  where
  writeFileUtf8 :: Print text => FilePath -> text -> m ()
  writeFile :: FilePath -> ByteString -> m ()
  readFile :: FilePath -> m ByteString
  doesFileExist :: FilePath -> m Bool
  decodeFileStrict :: (FromJSON a) => FilePath -> m (Maybe a)

-- Config paths, reads and writes
class (Monad m, HasFilesystem m) => HasConfig m  where
  getConfigPaths :: m (DirPath, FilePath)
  readConfig :: (FromJSON a) => m (Either TzbtcClientError a)
  writeConfig :: (ToJSON a) => a -> m ()
  createDirectoryIfMissing :: Bool -> DirPath -> m ()

-- Read arguments from cli, print messages out via cli
class (Monad m) => HasCmdLine m where
  parseCmdLine :: Opt.ParserInfo a -> m a
  printStringLn :: String -> m ()
  printLTextLn :: LText -> m ()
  printTextLn :: Text -> m ()
  printByteString :: ByteString -> m ()
  getLineFromUser :: m Text

-- Interaction with tezos node via RPC
class (HasTezosClient m, HasConfig m, Monad m, MonadThrow m) => HasTezosRpc m where
  runTransaction :: (ParamConstraints param) => Address -> param -> m ()
  getStorage :: Text -> m Expression
  getCounter :: Text -> m TezosWord64
  getCosts :: RunOperation -> m (TezosWord64, TezosWord64)
  getFromBigMap :: Natural -> Text -> m (Either TzbtcClientError Expression)
  waitForOperation :: Text -> m ()

-- Interaction with tezos client binary
class (HasConfig m, Monad m) => HasTezosClient m where
  getAddressAndPKForAlias :: Text -> m (Either TzbtcClientError (Address, PublicKey))
  signWithTezosClient :: Either InternalByteString Text -> m (Either Text Signature)

-- Interaction with a text editor
class (Monad m) => HasEditor m where
  openEditor :: FilePath -> (ByteString -> m ()) -> m ()
