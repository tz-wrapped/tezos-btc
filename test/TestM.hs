{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
-- | This module defines some infra to mock the IO procedures
-- and track various expectations during testing.
{-# OPTIONS_GHC -Wno-orphans #-}

module TestM
  ( Expectation(..)
  , ExpectationCount(..)
  , ExpectationStatus(..)
  , Handlers(..)
  , MyHandlers(..)
  , ST
  , TestM
  , TestError(..)
  ) where

import Data.Aeson (FromJSON)
import qualified Data.Map as Map
import Options.Applicative as Opt

import Tezos.Address
import Tezos.Common.Json (TezosInt64)
import Tezos.Crypto
import Tezos.V005.Micheline (Expression)

import Client.Error
import Client.Types
import Lorentz.Constraints
import Lorentz.Contracts.TZBTC (OriginationParameters)

import Util.AbstractIO

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
  | GetsUserConfirmation
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

data TestError
  = TestError String
  deriving Show

instance Exception TestError

instance MonadFail (Either SomeException) where
  fail s = Left $ toException $ TestError s

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
  , hConfirmAction :: Text -> m ConfirmationResult

  , hRunTransactions :: forall param. (NicePackedValue param) => Address -> [EntrypointParam param] -> m ()
  , hGetStorage :: Text -> m Expression
  , hGetCounter :: Text -> m TezosInt64
  , hGetFromBigMap :: Natural -> Text -> m (Either TzbtcClientError Expression)
  , hWaitForOperation :: Text -> m ()
  , hDeployTzbtcContract :: OriginationParameters -> m ()

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
  confirmAction i = do
    fn <- getHandler hConfirmAction
    fn i

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
  deployTzbtcContract op = do
    fn <- getHandler hDeployTzbtcContract
    fn op

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
