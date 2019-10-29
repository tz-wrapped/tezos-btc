{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{- | Some capability typeclasses that we use to abstract IO operations.

This enables us to isolate the rest of the logic and possibly write tests
for it.

-}
module Util.AbstractIO
  ( HasFilesystem(..)
  , HasTezosRpc(..)
  , HasConfig(..)
  , HasTezosClient(..)
  , HasCmdLine(..)
  , HasEditor(..)
  , Handlers(..)
  , DirPath(..)
  , TestM
  , ST
  , Expectation(..)
  , ExpectationCount(..)
  , ExpectationStatus(..)
  , MyHandlers(..)
  ) where

import Data.Aeson (FromJSON)
import qualified Data.Map as Map
import Options.Applicative as Opt

import Tezos.Address
import Tezos.Micheline (Expression)
import Tezos.Crypto
import Tezos.Json (TezosWord64(..))

import Client.Error
import Client.Types
import Lorentz.Constraints

newtype DirPath = DirPath { unDirPath :: FilePath } deriving (Show ,Eq)

-- File system operations
class (Monad m) => HasFilesystem m  where
  writeFileUtf8 :: Print text => FilePath -> text -> m ()
  writeFile :: FilePath -> ByteString -> m ()
  readFile :: FilePath -> m ByteString
  doesFileExist :: FilePath -> m Bool
  decodeFileStrict :: (FromJSON a) => FilePath -> m (Maybe a)
  createDirectoryIfMissing :: Bool -> DirPath -> m ()

-- Config paths, reads and writes
class (Monad m, HasFilesystem m) => HasConfig m  where
  getConfigPaths :: m (DirPath, FilePath)
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
  getLineFromUser :: m Text

-- Interaction with tezos node via RPC
class (HasTezosClient m, HasConfig m, Monad m, MonadThrow m) => HasTezosRpc m where
  runTransactions :: (NicePackedValue param) => Address -> [EntrypointParam param] -> m ()
  getStorage :: Text -> m Expression
  getCounter :: Text -> m TezosWord64
  getFromBigMap :: Natural -> Text -> m (Either TzbtcClientError Expression)
  deployTzbtcContract :: Address -> Address -> m ()

-- Interaction with tezos client binary
class (HasConfig m, Monad m) => HasTezosClient m where
  getAddressAndPKForAlias :: Text -> m (Either TzbtcClientError (Address, PublicKey))
  signWithTezosClient :: Either ByteString Text -> m (Either Text Signature)
  waitForOperation :: Text -> m ()

-- Interaction with a text editor
class (Monad m) => HasEditor m where
  openEditor :: FilePath -> (ByteString -> m ()) -> m ()

-- | Expectations that the tests can setup, and check at the end of the test.
data Expectation
  = PrintsMessage
  | GetConfigPaths
  | ChecksFileExist
  | CreateDirectory
  | ReadConfig
  | WritesConfig
  | WritesFile
  | ReadsFile
  | ParseCmdLine
  | GetsLineFromUser
  | CustomExpectation String
  | RunsTransaction
  deriving (Eq, Show, Ord)

-- | Specifiy how may time we expect an event to happen.
data ExpectationCount = Multiple | Once | Exact Int deriving (Show)

data ExpectationStatus = ExpectationStatus
  { exExpectCount :: ExpectationCount
  , exOccurCount :: Int
  } deriving (Show)

type ST = Map.Map Expectation ExpectationStatus

data MyHandlers = MyHandlers { unMyHandlers ::  Handlers TestM }

type TestM = StateT ST (ReaderT MyHandlers (Either SomeException))

-- | A reader environment for mock so that we can implement some default
-- instances reading the data from the environment.
data Handlers m = Handlers
  { hWriteFileUtf8 :: forall text. Print text => FilePath -> text -> m ()
  , hWriteFile :: FilePath -> ByteString -> m ()
  , hReadFile :: FilePath -> m ByteString
  , hDoesFileExist :: FilePath -> m Bool
  , hDecodeFileStrict :: forall a.(FromJSON a) => FilePath -> m (Maybe a)
  , hCreateDirectoryIfMissing :: Bool -> DirPath -> m ()

  , hGetConfigPaths :: m (DirPath, FilePath)
  , hReadConfig :: m (Either TzbtcClientError ClientConfig)
  , hReadConfigText :: m (Either TzbtcClientError ClientConfigText)
  , hWriteConfigFull :: ClientConfig -> m ()
  , hWriteConfigPartial :: ClientConfigPartial -> m ()
  , hWriteConfigText :: ClientConfigText -> m ()

  , hParseCmdLine :: forall a. Opt.ParserInfo a -> m a
  , hPrintTextLn :: forall text. (Print text) => text -> m ()
  , hPrintStringLn :: String -> m ()
  , hPrintByteString :: ByteString -> m ()
  , hGetLineFromUser :: m Text

  , hRunTransactions :: forall param. (NicePackedValue param) => Address -> [EntrypointParam param] -> m ()
  , hGetStorage :: Text -> m Expression
  , hGetCounter :: Text -> m TezosWord64
  , hGetFromBigMap :: Natural -> Text -> m (Either TzbtcClientError Expression)
  , hWaitForOperation :: Text -> m ()
  , hDeployTzbtcContract :: Address -> Address -> m ()

  , hGetAddressAndPKForAlias :: Text -> m (Either TzbtcClientError (Address, PublicKey))
  , hSignWithTezosClient :: Either ByteString Text -> m (Either Text Signature)

  , hOpenEditor :: FilePath -> (ByteString -> m ()) -> m ()
  }

getHandler :: (Handlers TestM -> fn) -> TestM fn
getHandler fn = (fn . unMyHandlers) <$> ask

instance HasFilesystem TestM where
  writeFileUtf8 fp t = do
    fn <- getHandler hWriteFileUtf8
    fn fp t
  writeFile fp bs = do
    fn <- getHandler hWriteFile
    fn fp bs
  readFile fp = do
    fn <- getHandler hReadFile
    fn fp
  doesFileExist fp = do
    fn <- getHandler hDoesFileExist
    fn fp
  decodeFileStrict fp = do
    fn <- getHandler hDecodeFileStrict
    fn fp
  createDirectoryIfMissing fl fp = do
    fn <- getHandler hCreateDirectoryIfMissing
    fn fl fp

instance HasConfig TestM where
  getConfigPaths = join $ getHandler hGetConfigPaths
  readConfig = join $ getHandler hReadConfig
  readConfigText = join $ getHandler hReadConfigText
  writeConfigFull c = do
    fn <- getHandler hWriteConfigFull
    fn c
  writeConfigPartial c = do
    fn <- getHandler hWriteConfigPartial
    fn c
  writeConfigText c = do
    fn <- getHandler hWriteConfigText
    fn c

instance HasCmdLine TestM where
  parseCmdLine a = do
    fn <- getHandler hParseCmdLine
    fn a
  printTextLn m = do
    fn <- getHandler hPrintTextLn
    fn m
  printStringLn m = do
    fn <- getHandler hPrintStringLn
    fn m
  printByteString m = do
    fn <- getHandler hPrintByteString
    fn m
  getLineFromUser = join $ getHandler hGetLineFromUser

instance HasTezosRpc TestM where
  runTransactions a b = do
    fn <- getHandler hRunTransactions
    fn a b
  getStorage t = do
    fn <- getHandler hGetStorage
    fn t
  getCounter t = do
    fn <- getHandler hGetCounter
    fn t
  getFromBigMap n t = do
    fn <- getHandler hGetFromBigMap
    fn n t
  deployTzbtcContract a b = do
    fn <- getHandler hDeployTzbtcContract
    fn a b

instance HasTezosClient TestM where
  getAddressAndPKForAlias a = do
    fn <- getHandler hGetAddressAndPKForAlias
    fn a
  signWithTezosClient a = do
    fn <- getHandler hSignWithTezosClient
    fn a
  waitForOperation t = do
    fn <- getHandler hWaitForOperation
    fn t

instance HasEditor TestM where
  openEditor fp cb = do
    fn <- getHandler hOpenEditor
    fn fp cb
