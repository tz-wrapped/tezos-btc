{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.IO
  ( createMultisigPackage
  , getPackageFromFile
  , runTzbtcContract
  , runMultisigContract
  , setupClient
  , writePackageToFile
  ) where

import Data.Aeson (decodeFileStrict, encodeFile)
import Data.ByteString (cons)
import Network.HTTP.Client
  (ManagerSettings(..), Request(..), newManager, defaultManagerSettings)
import Servant.Client
  (BaseUrl(..), ClientError, ClientEnv, Scheme(..), mkClientEnv, runClientM)
import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import System.Exit (ExitCode(..))
import System.IO (readLn)
import System.Process (readProcessWithExitCode)
import Text.Hex (encodeHex, decodeHex)
import Tezos.Json (TezosWord64)

import Michelson.Runtime.GState (genesisAddress1, genesisAddress2)
import Michelson.Typed.Haskell.Value (ContractAddr(..))
import Michelson.Untyped (InternalByteString(..))
import Tezos.Address (Address, formatAddress)
import Tezos.Crypto (PublicKey, Signature)
import Util.IO (readFileUtf8, writeFileUtf8)

import Client.API
import Client.Crypto
import Client.Error
import Client.Parser
import Client.Types
import Client.Util
import Lorentz.Contracts.TZBTC (Parameter(..))
import Lorentz.Contracts.TZBTC.Types
import qualified Lorentz.Contracts.TZBTC.MultiSig as MSig
import Util.MultiSig

appName, configFile :: FilePath
appName = "tzbtc"
configFile = "config.json"

getFullAppAndConfigPath :: IO (FilePath, FilePath)
getFullAppAndConfigPath = do
  appPath <- getAppUserDataDirectory appName
  return $ (appPath, appPath <> "/" <> configFile)

stubSignature :: Signature
stubSignature = unsafeParseSignature
  "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"

dumbOp :: TransactionOperation
dumbOp = TransactionOperation
  { toKind = "transaction"
  , toSource = genesisAddress1
  , toFee = 50000
  , toCounter = 0
  , toGasLimit = 800000
  , toStorageLimit = 10000
  , toAmount = 0
  , toDestination = genesisAddress2
  , toParameters = paramToExpression $ Pause ()
  }

-- | Add header, required by the Tezos RPC interface
fixRequest :: Request -> IO Request
fixRequest req = return $
  req { requestHeaders = ("Content-Type", "application/json") :
        filter (("Content-Type" /=) . fst) (requestHeaders req)
      }

getClientEnv :: ClientConfig -> IO ClientEnv
getClientEnv ClientConfig{..} = do
  manager' <- newManager $ defaultManagerSettings { managerModifyRequest = fixRequest }
  let nodeUrl = BaseUrl Http (toString ccNodeAddress) ccNodePort ""
  return $ mkClientEnv manager' nodeUrl

getAddressCounter :: ClientEnv -> Address -> IO (Either ClientError TezosWord64)
getAddressCounter env address = runClientM (getCounter $ formatAddress address) env

readConfigFile :: IO (Either TzbtcClientError ClientConfig)
readConfigFile = do
  (_, configPath) <- getFullAppAndConfigPath
  mbConfig <- decodeFileStrict configPath
  case mbConfig of
    Just config -> return $ Right config
    Nothing -> return $ Left TzbtcClientConfigError

getPackageFromFile :: FilePath -> IO (Either Text Package)
getPackageFromFile packageFilePath = do
  fileContents <- readFileUtf8 packageFilePath
  return $ case decodeHex fileContents of
    Nothing -> Left
      "Failed to decode multisig package: Invalid hex encoding"
    Just bs -> case decodePackage bs of
      Left err -> Left $ "Failed to decode multisig package: " <> fromString err
      Right package -> Right package

writePackageToFile :: Package -> FilePath -> IO ()
writePackageToFile package fileToWrite =
  writeFileUtf8 fileToWrite $ encodeHex $ encodePackage package

createMultisigPackage
  :: (ParamConstraints param, ToUnpackEnv param)
  => FilePath -> param -> IO ()
createMultisigPackage packagePath param = do
  ClientConfig{..} <- throwLeft $ readConfigFile
  putStrLn ("Enter multisig counter:" :: Text)
  -- (counter, _) <- getMultisigStorage ccMultisigAddress config
  counter <- readLn
  let package = mkPackage ccMultisigAddress counter
        (ContractAddr ccContractAddress) param
  writePackageToFile package packagePath


-- Not used for now
_getMultisigStorage :: Address -> ClientConfig -> IO MSig.Storage
_getMultisigStorage addr config@ClientConfig{..} = do
  clientEnv <- getClientEnv config
  mSigStorageRaw <- throwClientError $
    runClientM (getStorage $ formatAddress addr) clientEnv
  throwLeft $ pure $ exprToMultisigStorage mSigStorageRaw

getCosts
  :: ClientEnv -> RunOperation
  -> IO (Either TzbtcClientError (TezosWord64, TezosWord64))
getCosts env runOp = do
  req <- runClientM (runOperation runOp) env
  case req of
    Left err -> return $ Left (TzbtcServantError err)
    Right RunRes{..} -> do
      case rrOperationContents of
        [OperationContent (RunMetadata res internalOps)] ->
          let internalResults = map unInternalOperation internalOps in
          case foldr combineResults res internalResults of
            RunOperationApplied consumedGas storageSize ->
              return $ Right (consumedGas, storageSize)
            RunOperationFailed errors ->
              return $ Left (TzbtcRunFailed errors)
        [] -> return $ Left $ TzbtcUnexpectedRunResult "empty result"
        _ -> return $ Left $
          TzbtcUnexpectedRunResult "expecting only one operation content"

getOperationHex
  :: ClientEnv -> ForgeOperation
  -> IO (Either ClientError InternalByteString)
getOperationHex env forgeOp = runClientM (forgeOperation forgeOp) env

getLastBlockHash :: ClientEnv -> IO (Either ClientError Text)
getLastBlockHash env = runClientM getLastBlock env

injectOp :: Text -> Signature -> ClientConfig -> IO ()
injectOp opToInject sign config = do
  clientEnv <- getClientEnv config
  opHash <- throwClientError $ runClientM
    (injectOperation (Just "main") $ prepareForInjection opToInject sign) clientEnv
  putStrLn $ "Operation hash: " <> opHash

runTransaction
  :: (ParamConstraints param)
  => Address -> param -> ClientConfig -> IO ()
runTransaction to param config@ClientConfig{..} = do
  clientEnv <- getClientEnv config
  lastBlockHash <- throwClientError $ getLastBlockHash clientEnv
  counter <- throwClientError $ getAddressCounter clientEnv ccUserAddress
  let opToRun = dumbOp
        { toCounter = counter + 1
        , toDestination = to
        , toSource = ccUserAddress
        , toParameters = paramToExpression $ param
        }
  let runOp = RunOperation
        { roBranch = lastBlockHash
        , roContents = [opToRun]
        , roSignature = stubSignature
        }
  (consumedGas, storageSize) <- throwLeft $ getCosts clientEnv runOp
  hex <- throwClientError $ getOperationHex clientEnv ForgeOperation
    { foBranch = lastBlockHash
    , foContents = [opToRun { toGasLimit = consumedGas + 200
                            , toStorageLimit = storageSize + 20
                            , toFee = calcFees consumedGas storageSize
                            }]
    }
  signRes <- signWithTezosClient (addOperationPrefix hex) config
  case signRes of
    Left err -> putStrLn err
    Right signature' -> injectOp (formatByteString hex) signature' config

addOperationPrefix :: InternalByteString -> InternalByteString
addOperationPrefix (InternalByteString bs) =
  InternalByteString $ cons 0x03 bs

signWithTezosClient :: InternalByteString -> ClientConfig -> IO (Either Text Signature)
signWithTezosClient bs ClientConfig{..} = do
  (exitCode, stdout', stderr') <- readProcessWithExitCode ccTezosClientExecutable
    [ "-A", toString ccNodeAddress, "-P", show ccNodePort
    , "sign", "bytes", toString $ "0x" <> formatByteString bs
    -- 0x prefix is required for bytestrings in tezos-client
    , "for", toString ccUserAlias
    ] ""
  case exitCode of
    ExitSuccess -> do
      Right <$> (throwLeft $ pure $ parseSignatureFromOutput (fromString stdout'))
    ExitFailure _ -> return . Left . fromString $
      "Operation signing failed: " <> stderr'

setupClient :: ClientConfig -> IO ()
setupClient config = do
  (appPath, configPath) <- getFullAppAndConfigPath
  createDirectoryIfMissing True appPath
  encodeFile configPath config

runTzbtcContract :: Parameter -> IO ()
runTzbtcContract param = do
  config@ClientConfig{..} <- throwLeft $ readConfigFile
  runTransaction ccContractAddress param config

runMultisigContract :: NonEmpty Package -> [PublicKey] -> IO ()
runMultisigContract packages keys' = do
  config <- throwLeft $ readConfigFile
  package <- throwLeft $ pure $ mergePackages packages
  multisigAddr <- throwLeft $ pure (fst <$> getToSign package)
  (_, multisigParam) <- throwLeft $ pure $ mkMultiSigParam keys' packages
  runTransaction multisigAddr multisigParam config
