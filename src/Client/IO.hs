{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.IO
  ( addrOrAliasToAddr
  , createMultisigPackage
  , getPackageFromFile
  , runTzbtcContract
  , runMultisigContract
  , setupClient
  , signPackageForConfiguredUser
  , writePackageToFile
  ) where

import Data.Aeson (decodeFileStrict, encodeFile)
import Data.ByteString (cons, readFile, writeFile)
import Data.Sequence (Seq(..))
import Network.HTTP.Client
  (ManagerSettings(..), Request(..), newManager, defaultManagerSettings)
import Servant.Client
  (BaseUrl(..), ClientError, ClientEnv, Scheme(..), mkClientEnv, runClientM)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserConfigFile)
import Tezos.Json (TezosWord64(..))
import Tezos.Micheline
  (Expression(..), MichelinePrimAp(..), MichelinePrimitive(..))

import Michelson.Runtime.GState (genesisAddress1, genesisAddress2)
import Michelson.Typed.Haskell.Value (ContractAddr(..))
import Michelson.Untyped (InternalByteString(..))
import Tezos.Address (Address, formatAddress, parseAddress)
import Tezos.Crypto (PublicKey, Signature, parsePublicKey)

import Client.API
import Client.Crypto
import Client.Error
import Client.Parser
import Client.Types
import Client.Util
import Lorentz.Contracts.TZBTC (Parameter(..))
import qualified Lorentz.Contracts.TZBTC.MultiSig as MSig
import Util.MultiSig

appName, configFile :: FilePath
appName = "tzbtc"
configFile = "config.json"

getFullAppAndConfigPath :: IO (FilePath, FilePath)
getFullAppAndConfigPath = do
  configDir <- getUserConfigDir appName
  configFile_ <- getUserConfigFile appName configFile

  return $ (configDir, configFile_)

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
  fileContents <- readFile packageFilePath
  return $ case decodePackage fileContents of
    Left err -> Left $ "Failed to decode multisig package: " <> fromString err
    Right package -> Right package

writePackageToFile :: Package -> FilePath -> IO ()
writePackageToFile package fileToWrite =
  writeFile fileToWrite $ encodePackage package

createMultisigPackage :: FilePath -> Parameter -> IO ()
createMultisigPackage packagePath param = do
  config@ClientConfig{..} <- throwLeft $ readConfigFile
  (counter, _) <- throwLeft $ getMultisigStorage ccMultisigAddress config
  let package = mkPackage ccMultisigAddress counter
        (ContractAddr ccContractAddress) param
  writePackageToFile package packagePath


-- Quite ugly patternmatching, but at least such approach
-- works for multisig storage extraction. As soon as https://issues.serokell.io/issue/TM-140
-- is resolved, this function should be changed.
getMultisigStorage
  :: Address -> ClientConfig -> IO (Either TzbtcClientError MSig.Storage)
getMultisigStorage addr config@ClientConfig{..} = do
  clientEnv <- getClientEnv config
  mSigStorageRaw <- throwClientError $
    runClientM (getStorage $ formatAddress addr) clientEnv
  return $ case mSigStorageRaw of
    Expression_Prim
      (MichelinePrimAp
       { _michelinePrimAp_prim = MichelinePrimitive "Pair"
       , _michelinePrimAp_args =
         Expression_Int (TezosWord64 {unTezosWord64 = counter}) :<|
         Expression_Prim
         (MichelinePrimAp
           { _michelinePrimAp_prim = MichelinePrimitive "Pair"
           , _michelinePrimAp_args =
             Expression_Int (TezosWord64 {unTezosWord64 = threshold}) :<|
             Expression_Seq keySequence :<|
             Empty
            }
         ) :<|
         Empty
       }
      ) -> do
      case traverse (extractKey mSigStorageRaw) $ toList keySequence of
        Left err -> Left err
        Right keys' -> Right $ (fromIntegral counter, (fromIntegral threshold, keys'))
    _ -> Left $ TzbtcUnexpectedMultisigStorage $ MichelsonExpression mSigStorageRaw

  where
    extractKey :: Expression -> Expression -> Either TzbtcClientError PublicKey
    extractKey rawStor = \case
      Expression_String pubKeyRaw ->
        either (Left . TzbtcPublicKeyParseError pubKeyRaw) Right $
        parsePublicKey pubKeyRaw
      _ -> Left $ TzbtcUnexpectedMultisigStorage $ MichelsonExpression rawStor

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

injectOp :: Text -> Signature -> ClientConfig -> IO Text
injectOp opToInject sign config = do
  clientEnv <- getClientEnv config
  throwClientError $ runClientM
    (injectOperation (Just "main") $ prepareForInjection opToInject sign) clientEnv

runTransaction
  :: (ParamConstraints param)
  => Address -> param -> ClientConfig -> IO ()
runTransaction to param config@ClientConfig{..} = do
  clientEnv <- getClientEnv config
  lastBlockHash <- throwClientError $ getLastBlockHash clientEnv
  (sourceAddr, _) <- throwLeft $ getAddressAndPKForAlias ccUserAlias config
  counter <- throwClientError $ getAddressCounter clientEnv sourceAddr
  let opToRun = dumbOp
        { toCounter = counter + 1
        , toDestination = to
        , toSource = sourceAddr
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
  signRes <- signWithTezosClient (Left $ addOperationPrefix hex) config
  case signRes of
    Left err -> putStrLn err
    Right signature' -> do
      opHash <- injectOp (formatByteString hex) signature' config
      putStrLn $ "Operation hash: " <> opHash
      waitForOperationInclusion opHash config

addOperationPrefix :: InternalByteString -> InternalByteString
addOperationPrefix (InternalByteString bs) =
  InternalByteString $ cons 0x03 bs

signWithTezosClient
  :: Either InternalByteString Text -> ClientConfig -> IO (Either Text Signature)
signWithTezosClient bs config@ClientConfig{..} = do
  let toSign = case bs of
        Left rawBS -> addTezosBytesPrefix $ formatByteString rawBS
        Right formatedBS -> formatedBS
  (exitCode, stdout', stderr') <- readProcessWithExitCode ccTezosClientExecutable
    (tezosNodeArgs config <>
     [ "sign", "bytes", toString $ toSign
     -- 0x prefix is required for bytestrings in tezos-client
     , "for", toString ccUserAlias
     ]) ""
  case exitCode of
    ExitSuccess -> do
      Right <$> (throwLeft $ pure $ parseSignatureFromOutput (fromString stdout'))
    ExitFailure _ -> return . Left . fromString $
      "Operation signing failed: " <> stderr'

signPackageForConfiguredUser :: Package -> IO (Either String Package)
signPackageForConfiguredUser pkg = do
  config@ClientConfig{..} <- throwLeft $ readConfigFile
  (_, pk) <- throwLeft $ getAddressAndPKForAlias ccUserAlias config
  signRes <- signWithTezosClient (Right $ getBytesToSign pkg) config
  case signRes of
    Left err -> pure $ Left $ toString err
    Right signature' -> pure $ addSignature pkg (pk, signature')

tezosNodeArgs :: ClientConfig -> [String]
tezosNodeArgs ClientConfig{..} = ["-A", toString ccNodeAddress, "-P", show ccNodePort]

waitForOperationInclusion :: Text -> ClientConfig -> IO ()
waitForOperationInclusion op config@ClientConfig{..} = do
  (exitCode, stdout', stderr') <- readProcessWithExitCode ccTezosClientExecutable
    (tezosNodeArgs config <> ["wait", "for", toString op, "to", "be", "included"]) ""
  case exitCode of
    ExitSuccess -> putStr stdout'
    ExitFailure _ -> putStr stderr'

getAddressAndPKForAlias
  :: Text -> ClientConfig -> IO (Either TzbtcClientError (Address, PublicKey))
getAddressAndPKForAlias alias config@ClientConfig{..} = do
  (exitCode, stdout', _) <- readProcessWithExitCode ccTezosClientExecutable
    (tezosNodeArgs config <> ["show", "address", toString alias]) ""
  case exitCode of
    ExitSuccess -> do
      addr <- throwLeft $ pure $ (parseAddressFromOutput $ toText stdout')
      pure $ Right addr
    ExitFailure _ -> pure $ Left $ TzbtcUnknownAliasError alias

addrOrAliasToAddr :: AddrOrAlias -> IO Address
addrOrAliasToAddr addrOrAlias =
  case parseAddress addrOrAlias of
    Right addr -> pure addr
    Left _ -> do
      config <- throwLeft $ readConfigFile
      (addr, _) <- throwLeft $ getAddressAndPKForAlias addrOrAlias config
      pure addr

setupClient :: ClientConfig -> IO ()
setupClient config = do
  (appPath, configPath) <- getFullAppAndConfigPath
  createDirectoryIfMissing True appPath
  putStrLn $ "Config file written to: " ++ configPath
  encodeFile configPath config

runTzbtcContract :: Parameter -> IO ()
runTzbtcContract param = do
  config@ClientConfig{..} <- throwLeft $ readConfigFile
  runTransaction ccContractAddress param config

runMultisigContract :: NonEmpty Package -> IO ()
runMultisigContract packages = do
  config <- throwLeft $ readConfigFile
  package <- throwLeft $ pure $ mergePackages packages
  multisigAddr <- throwLeft $ pure (fst <$> getToSign package)
  (_, (_, keys')) <- throwLeft $ getMultisigStorage multisigAddr config
  (_, multisigParam) <- throwLeft $ pure $ mkMultiSigParam keys' packages
  runTransaction multisigAddr multisigParam config
