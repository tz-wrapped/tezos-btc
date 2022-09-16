{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
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

import Colog.Core.Class (HasLog(..))
import Colog.Message (Message)
import Data.ByteArray (ScrubbedBytes)
import Data.Map qualified as Map

import Morley.Client
import Morley.Client.Logging (ClientLogAction)
import Morley.Client.RPC
import Morley.Client.TezosClient (CalcOriginationFeeData, CalcTransferFeeData, TezosClientConfig)
import Morley.Micheline
import Morley.Michelson.Typed.Scope (UntypedValScope)
import Morley.Tezos.Address
import Morley.Tezos.Address.Alias (AddressOrAlias(..), Alias(..))
import Morley.Tezos.Core
import Morley.Tezos.Crypto (KeyHash, PublicKey, SecretKey, Signature)
import Morley.Util.ByteString

import Client.Env
import Client.IO ()
import Client.Types

import Util.AbstractIO

-- | Expectations that the tests can setup, and check at the end of the test.
data Expectation
  = PrintsMessage
  | WritesFile FilePath (Maybe ByteString)
  | WritesFileUtf8 FilePath
  | ReadsFile
  | GetsUserConfirmation
  | PrintByteString ByteString
  | RememberContract Address Alias
  | LooksupEnv
  deriving stock (Eq, Show, Ord)

-- | Specifiy how may time we expect an event to happen.
data ExpectationCount = Multiple | Once | Exact Int
  deriving stock (Show)

data ExpectationStatus = ExpectationStatus
  { exExpectCount :: ExpectationCount
  , exOccurCount :: Int
  } deriving stock (Show)

type ST = Map.Map Expectation ExpectationStatus

data MyHandlers = MyHandlers { unMyHandlers ::  Handlers TestM }

type TestM = StateT ST (ReaderT (MyHandlers, AppEnv) (Either SomeException))

data TestError
  = TestError String
  deriving stock Show

instance Exception TestError

instance MonadFail (Either SomeException) where
  fail s = Left $ toException $ TestError s

-- | A reader environment for mock so that we can implement some default
-- instances reading the data from the environment.
data Handlers m = Handlers
  { hWriteFileUtf8 :: forall text. ToLText text => FilePath -> text -> m ()
  , hWriteFile :: FilePath -> ByteString -> m ()
  , hReadFile :: FilePath -> m ByteString
  , hDoesFileExist :: FilePath -> m Bool

  , hPrintTextLn :: forall text. (Print text) => text -> m ()
  , hPrintStringLn :: String -> m ()
  , hPrintByteString :: ByteString -> m ()
  , hConfirmAction :: Text -> m ConfirmationResult

  , hGetHeadBlock :: m BlockHash
  , hGetCounter :: Address -> m TezosInt64
  , hGetBlockHeader :: BlockId -> m BlockHeader
  , hGetBlockConstants :: BlockId -> m BlockConstants
  , hGetBlockOperations :: BlockId -> m [[BlockOperation]]
  , hGetProtocolParameters :: m ProtocolParameters
  , hRunOperation :: RunOperation -> m RunOperationResult
  , hPreApplyOperations :: [PreApplyOperation] -> m [RunOperationResult]
  , hForgeOperation :: ForgeOperation -> m HexJSONByteString
  , hInjectOperation :: HexJSONByteString -> m OperationHash
  , hGetContractScript :: Address -> m OriginationScript
  , hGetContractStorageAtBlock :: BlockId -> Address -> m Expression
  , hGetContractBigMap :: Address -> GetBigMap -> m GetBigMapResult
  , hGetBigMapValueAtBlock :: BlockId -> Natural -> Text -> m Expression
  , hGetBalance :: Address -> m Mutez
  , hRunCode :: RunCode -> m RunCodeResult
  , hGetChainId :: m ChainId
  , hGetBigMapValuesAtBlock  :: BlockId -> Natural -> Maybe Natural -> Maybe Natural -> m Expression
  , hGetManagerKeyAtBlock :: BlockId -> Address -> m (Maybe PublicKey)
  , hGetDelegateAtBlock :: BlockId -> Address -> m (Maybe KeyHash)

  -- HasTezosClient
  , hSignBytes :: AddressOrAlias -> Maybe ScrubbedBytes -> ByteString -> m Signature
  , hGenKey :: Alias -> m Address
  , hGenFreshKey :: Alias -> m Address
  , hRevealKey :: Alias -> Maybe ScrubbedBytes -> m ()
  , hWaitForOperation :: m OperationHash -> m OperationHash
  , hRememberContract :: Bool -> Address -> Alias -> m ()
  , hImportKey :: Bool -> Alias -> SecretKey -> m Alias
  , hResolveAddressMaybe :: AddressOrAlias -> m (Maybe Address)
  , hGetAlias :: AddressOrAlias -> m Alias
  , hGetPublicKey :: AddressOrAlias -> m PublicKey
  , hGetTezosClientConfig :: m TezosClientConfig
  , hCalcTransferFee
    :: AddressOrAlias -> Maybe ScrubbedBytes -> TezosInt64 -> [CalcTransferFeeData] -> m [TezosMutez]
  , hCalcOriginationFee
    :: forall cp st. UntypedValScope st => CalcOriginationFeeData cp st -> m TezosMutez
  , hGetKeyPassword :: Address -> m (Maybe ScrubbedBytes)
  , hRegisterDelegate :: Alias -> Maybe ScrubbedBytes -> m ()

  , hLookupEnv :: m AppEnv
  , hWithLocal :: forall a. (AppEnv -> AppEnv) -> m a -> m a

  , hLogAction :: ClientLogAction m
  , hGetSecretKey :: AddressOrAlias -> m SecretKey
  , hGetBlockOperationHashes :: BlockId -> m [[OperationHash]]
  , hGetScriptSizeAtBlock :: BlockId -> CalcSize -> m ScriptSize
  }

getHandler :: (Handlers TestM -> fn) -> TestM fn
getHandler fn = (fn . unMyHandlers . fst) <$> ask

instance HasLog (MyHandlers, AppEnv) Message TestM where
  getLogAction = hLogAction . unMyHandlers . fst
  setLogAction action' (MyHandlers handlers, env) =
    (MyHandlers $ handlers { hLogAction = action' }, env)


instance HasFilesystem TestM where
  writeFileUtf8 fp t = do
    handlers <- asks (unMyHandlers . fst)
    hWriteFileUtf8 handlers fp t
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
  printTextLn m = do
    handlers <- asks (unMyHandlers . fst)
    hPrintTextLn handlers m
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
  getBlockHash _blk = do
    join $ getHandler hGetHeadBlock
  getCounterAtBlock _blk addr = do
    h <- getHandler hGetCounter
    h addr
  getBlockHeader block = do
    h <- getHandler hGetBlockHeader
    h block
  getBlockConstants block = do
    h <- getHandler hGetBlockConstants
    h block
  getBlockOperations block = do
    h <- getHandler hGetBlockOperations
    h block
  getProtocolParametersAtBlock _blk = do
    join $ getHandler hGetProtocolParameters
  runOperationAtBlock _blk op = do
    h <- getHandler hRunOperation
    h op
  preApplyOperationsAtBlock _blk ops = do
    h <- getHandler hPreApplyOperations
    h ops
  forgeOperationAtBlock _blk op = do
    h <- getHandler hForgeOperation
    h op
  injectOperation op = do
    h <- getHandler hInjectOperation
    h op
  getContractScriptAtBlock _blk addr = do
    h <- getHandler hGetContractScript
    h addr
  getContractStorageAtBlock block addr = do
    h <- getHandler hGetContractStorageAtBlock
    h block addr
  getContractBigMapAtBlock _blk addr getBigMap = do
    h <- getHandler hGetContractBigMap
    h addr getBigMap
  getBigMapValueAtBlock block bigMapId scriptExpr = do
    h <- getHandler hGetBigMapValueAtBlock
    h block bigMapId scriptExpr
  getBalanceAtBlock _blk addr = do
    h <- getHandler hGetBalance
    h addr
  runCodeAtBlock _blk r = do
    h <- getHandler hRunCode
    h r
  getChainId = join (getHandler hGetChainId)
  getBigMapValuesAtBlock blockId bigMapId mbOffset mbLength = do
    h <- getHandler hGetBigMapValuesAtBlock
    h blockId bigMapId mbOffset mbLength
  getDelegateAtBlock block addr = do
    h <- getHandler hGetDelegateAtBlock
    h block addr
  getManagerKeyAtBlock block addr = do
    h <- getHandler hGetManagerKeyAtBlock
    h block addr
  waitForOperation op = do
    h <- getHandler hWaitForOperation
    h op
  getScriptSizeAtBlock x cs = do
    h <- getHandler hGetScriptSizeAtBlock
    h x cs
  getBlockOperationHashes x = do
    h <- getHandler hGetBlockOperationHashes
    h x

instance HasTezosClient TestM where
  signBytes alias mbPassword op = do
    h <- getHandler hSignBytes
    h alias mbPassword op
  genKey alias = do
    h <- getHandler hGenKey
    h alias
  genFreshKey alias = do
    h <- getHandler hGenFreshKey
    h alias
  revealKey alias mbPassword = do
    h <- getHandler hRevealKey
    h alias mbPassword
  rememberContract replaceExisting addr alias = do
    h <- getHandler hRememberContract
    h replaceExisting addr alias
  importKey replaceExisting alias key = do
    h <- getHandler hImportKey
    h replaceExisting alias key
  resolveAddressMaybe addr = do
    h <- getHandler hResolveAddressMaybe
    h addr
  getAlias originator = do
    h <- getHandler hGetAlias
    h originator
  getPublicKey a = do
    h <- getHandler hGetPublicKey
    h a
  getTezosClientConfig =
    join $ getHandler hGetTezosClientConfig
  calcTransferFee sender mbPassword fee transferData = do
    h <- getHandler hCalcTransferFee
    h sender mbPassword fee transferData
  calcOriginationFee origData = do
    handlers <- asks (unMyHandlers . fst)
    hCalcOriginationFee handlers origData
  getKeyPassword addr = do
    h <- getHandler hGetKeyPassword
    h addr
  registerDelegate kh pw = do
    h <- getHandler hRegisterDelegate
    h kh pw
  getSecretKey a = do
    h <- getHandler hGetSecretKey
    h a

instance HasEnv TestM where
  lookupEnv = do
    join $ getHandler hLookupEnv
  withLocal f act = do
    handlers <- asks (unMyHandlers . fst)
    hWithLocal handlers f act
