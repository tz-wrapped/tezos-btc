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

import qualified Data.Map as Map
import Options.Applicative as Opt

import Tezos.Address
import Tezos.Common.Json (TezosInt64)
import Tezos.Crypto
import Tezos.V005.Micheline (Expression)

import Client.Env
import Client.Error
import Client.IO ()
import Client.Types
import Lorentz.Constraints
import Lorentz.Contracts.TZBTC (OriginationParameters)

import Util.AbstractIO

-- | Expectations that the tests can setup, and check at the end of the test.
data Expectation
  = PrintsMessage
  | WritesFile FilePath (Maybe ByteString)
  | WritesFileUtf8 FilePath
  | ReadsFile
  | ParseCmdLine
  | GetsUserConfirmation
  | RunsTransaction
  | PrintByteString ByteString
  | DeployTzbtcContract
  | RememberContract Address Text
  | LooksupEnv
  deriving (Eq, Show, Ord)

-- | Specifiy how may time we expect an event to happen.
data ExpectationCount = Multiple | Once | Exact Int deriving (Show)

data ExpectationStatus = ExpectationStatus
  { exExpectCount :: ExpectationCount
  , exOccurCount :: Int
  } deriving (Show)

type ST = Map.Map Expectation ExpectationStatus

data MyHandlers = MyHandlers { unMyHandlers ::  Handlers TestM }

type TestM = StateT ST (ReaderT (MyHandlers, AppEnv) (Either SomeException))

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
  , hGetTezosClientConfig :: m (Either Text (FilePath, TezosClientConfig))
  , hDeployTzbtcContract :: OriginationParameters -> m ()

  , hGetAddressAndPKForAlias :: Text -> m (Either TzbtcClientError (Address, PublicKey))
  , hSignWithTezosClient :: Either ByteString Text -> Text -> m (Either Text Signature)
  , hGetAddressForContract :: Text -> m (Either TzbtcClientError Address)
  , hRememberContract :: Address -> Text -> m ()

  , hLookupEnv :: m AppEnv
  , hWithLocal :: forall a. (AppEnv -> AppEnv) -> m a -> m a
  }

getHandler :: (Handlers TestM -> fn) -> TestM fn
getHandler fn = (fn . unMyHandlers . fst) <$> ask

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
  signWithTezosClient a alias = do
    fn <- getHandler hSignWithTezosClient
    fn a alias
  waitForOperation t = do
    fn <- getHandler hWaitForOperation
    fn t
  getTezosClientConfig = join $ getHandler hGetTezosClientConfig
  getAddressForContract c = do
    fn <- getHandler hGetAddressForContract
    fn c
  rememberContractAs c a = do
    fn <- getHandler hRememberContract
    fn c a

instance HasEnv TestM where
  lookupEnv = do
    join $ getHandler hLookupEnv
  withLocal f act = do
    fn <- getHandler hWithLocal
    fn f act

