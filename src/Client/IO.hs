{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.IO
  ( HasStoreTemplateField
  , addrOrAliasToAddr
  , createMultisigPackage
  , getAllowance
  , getBalance
  , getFieldFromTzbtcUStore
  , getValueFromTzbtcUStoreSubmap
  , getPackageFromFile
  , runConfigEdit
  , runTzbtcContract
  , runMultisigContract
  , setupClient
  , signPackageForConfiguredUser
  , writePackageToFile
  ) where

import Data.Aeson (FromJSON, ToJSON, decodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (cons, readFile, writeFile)
import qualified Data.ByteString.Lazy as BSL hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL (putStrLn)
import qualified Data.Map as Map (lookup)
import Fmt (pretty)
import Named (Name(..), arg)
import Network.HTTP.Client
  (ManagerSettings(..), Request(..), newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (notFound404)
import Servant.Client
  (BaseUrl(..), ClientError(..), ClientEnv, ResponseF(..), Scheme(..), mkClientEnv,
  runClientM)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserConfigFile)
import Tezos.Json (TezosWord64(..))

import Lorentz.Constraints (NicePackedValue, NiceUnpackedValue)
import Lorentz.Contracts.ManagedLedger.Types (LedgerValue)
import Lorentz.Pack (lPackValue, lUnpackValue)
import Lorentz.UStore (HasUField, HasUStore)
import Lorentz.UStore.Common (fieldNameToMText)
import Michelson.Interpret.Unpack (dummyUnpackEnv)
import Michelson.Runtime.GState (genesisAddress1, genesisAddress2)
import Michelson.Untyped (InternalByteString(..))
import Tezos.Address (Address, formatAddress, parseAddress)
import Tezos.Crypto (PublicKey, Signature, encodeBase58Check)

import Client.API
import Client.Crypto
import Client.Error
import Client.Parser
import Client.Types
import Client.Util
import Lorentz.Contracts.TZBTC
  (FlatParameter(..), Interface, SafeParameter, StoreTemplate, Parameter, fromFlatParameter)
import qualified Lorentz.Contracts.TZBTC.MultiSig as MSig
import Util.MultiSig
import Util.Editor

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
  , toParameters = ParametersInternal
    { piEntrypoint = "default"
    , piValue = paramToExpression $ fromFlatParameter $ Pause ()
    }
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

readConfigFile :: (FromJSON a) => IO (Either TzbtcClientError a)
readConfigFile = do
  (_, configPath) <- getFullAppAndConfigPath
  fileExists <- doesFileExist configPath
  if fileExists then do
    mbConfig <- decodeFileStrict configPath
    case mbConfig of
      Just config -> return $ Right config
      Nothing -> return $ Left TzbtcClientConfigError
  else return $ Left $ TzbtcClientConfigFileNotFound configPath

getPackageFromFile :: FilePath -> IO (Either Text Package)
getPackageFromFile packageFilePath = do
  fileContents <- readFile packageFilePath
  return $ case decodePackage fileContents of
    Left err -> Left $ "Failed to decode multisig package: " <> fromString err
    Right package -> Right package

writePackageToFile :: Package -> FilePath -> IO ()
writePackageToFile package fileToWrite =
  writeFile fileToWrite $ encodePackage package

createMultisigPackage :: FilePath -> SafeParameter s -> IO ()
createMultisigPackage packagePath parameter = do
  config@ClientConfig{..} <- throwLeft $ readConfigFile
  case ccMultisigAddress of
    Nothing -> throwLeft $ pure $ Left TzbtcMutlisigConfigUnavailable
    Just msAddr ->  do
      (counter, _) <- getMultisigStorage msAddr config
      let package = mkPackage msAddr counter ccContractAddress parameter
      writePackageToFile package packagePath

getMultisigStorage
  :: Address -> ClientConfig -> IO MSig.Storage
getMultisigStorage addr config@ClientConfig{..} = do
  clientEnv <- getClientEnv config
  mSigStorageRaw <- throwClientError $
    runClientM (getStorage $ formatAddress addr) clientEnv
  throwLeft $ pure $ exprToValue @MSig.Storage mSigStorageRaw

type HasStoreTemplateField t name =
  ( HasUField name t StoreTemplate
  , NiceUnpackedValue t
  )

type HasStoreTemplateSubmap key value name =
  ( HasUStore name key value StoreTemplate
  , NicePackedValue key, NiceUnpackedValue value
  )

-- | Get field with given name from TZBTC contract UStore.
--
getFieldFromTzbtcUStore
  :: forall name t. HasStoreTemplateField t name
  => IO (Maybe t)
getFieldFromTzbtcUStore =
  getFromTzbtcUStore $ fieldNameToMText @name

-- | Get value assosiated with given key from given submap of
-- TZBTC contract UStore.
getValueFromTzbtcUStoreSubmap
  :: forall name key value. (HasStoreTemplateSubmap key value name)
  => key -> IO (Maybe value)
getValueFromTzbtcUStoreSubmap key =
  getFromTzbtcUStore (fieldNameToMText @name , key)

-- | Get value for given keu from UStore.
--
-- UStore is basically `BigMap Bytestring ByteString`.
-- So in order to get value from it we pack the key into
-- `ByteString` and perform getting from BigMap using RPC call.
-- After obtaining value we unpack it to the desired type.
getFromTzbtcUStore
  :: forall key value.
     ( NicePackedValue key
     , NiceUnpackedValue value
     )
  => key -> IO (Maybe value)
getFromTzbtcUStore key = do
  config <- throwLeft $ readConfigFile
  clientEnv <- getClientEnv config
  AlmostStorage{..} <- getTzbtcStorage config clientEnv
  let scriptExpr = encodeBase58Check . valueToScriptExpr . lPackValue $ key
  fieldValueRaw <- runClientM (getFromBigMap asBigMapId scriptExpr) clientEnv
  case fieldValueRaw of
    Left err -> case err of
      FailureResponse _ Response{..} -> do
        if responseStatusCode == notFound404 then pure $ Nothing
        else throwM err
      _ -> throwM err
    Right veryRawVal -> do
      rawVal <- throwLeft $ pure $ exprToValue @ByteString veryRawVal
      fmap Just . throwLeft . pure $ lUnpackValue dummyUnpackEnv rawVal

getTzbtcStorage
  :: ClientConfig -> ClientEnv -> IO (AlmostStorage Interface)
getTzbtcStorage ClientConfig{..} clientEnv = do
  storageRaw <- throwClientError $
    runClientM (getStorage $ formatAddress ccContractAddress) clientEnv
  throwLeft $ pure $ exprToValue storageRaw

getBalance :: Address -> IO Natural
getBalance addr = do
  mbLedgerValue <- getValueFromTzbtcUStoreSubmap @"ledger" @Address @LedgerValue addr
  case mbLedgerValue of
    Nothing -> pure 0
    Just ledgerValue -> pure $ arg (Name @"balance") . fst $ ledgerValue

getAllowance :: Address -> Address -> IO Natural
getAllowance owner spender = do
  mbLedgerValue <- getValueFromTzbtcUStoreSubmap @"ledger" @Address @LedgerValue owner
  case mbLedgerValue of
    Nothing -> pure 0
    Just ledgerValue -> let approvals = arg (Name @"approvals") . snd $ ledgerValue in
      pure $ maybe 0 id $ Map.lookup spender approvals

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

getCurrentChainId :: ClientEnv -> IO (Either ClientError Text)
getCurrentChainId env = runClientM getMainChainId env

injectOp :: Text -> Signature -> ClientConfig -> IO Text
injectOp opToInject sign config = do
  clientEnv <- getClientEnv config
  throwClientError $ runClientM
    (injectOperation (Just "main") $ prepareForInjection opToInject sign) clientEnv

runTransaction
  :: (NicePackedValue param)
  => Address -> param -> ClientConfig -> IO ()
runTransaction to param config@ClientConfig{..} = do
  clientEnv <- getClientEnv config
  lastBlockHash <- throwClientError $ getLastBlockHash clientEnv
  (sourceAddr, _) <- throwLeft $ getAddressAndPKForAlias ccUserAlias config
  chainId <- throwClientError $ getCurrentChainId clientEnv
  counter <- throwClientError $ getAddressCounter clientEnv sourceAddr
  let opToRun = dumbOp
        { toCounter = counter + 1
        , toDestination = to
        , toSource = sourceAddr
        , toParameters = ParametersInternal
          { piEntrypoint = "default"
          , piValue = paramToExpression $ param
          }
        }
  let runOp = RunOperation
        { roOperation =
          RunOperationInternal
          { roiBranch = lastBlockHash
          , roiContents = [opToRun]
          , roiSignature = stubSignature
          }
        , roChainId = chainId
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
     , "for", toString ccUserAlias
     ]) ""
  case exitCode of
    ExitSuccess -> do
      Right <$> (throwLeft $ pure $ parseSignatureFromOutput (fromString stdout'))
    ExitFailure _ -> return . Left . fromString $
      "Operation signing failed: " <> stderr'

data ConfirmationResult = Confirmed | Canceled

confirmAction :: IO ConfirmationResult
confirmAction = do
  putTextLn "Are you sure? [Y/N]"
  res <- getLine
  case res of
    x | x `elem` ["Y", "y", "yes"] -> pure Confirmed
    x | x `elem` ["N", "n", "no"] -> pure Canceled
    _ -> confirmAction

signPackageForConfiguredUser :: Package -> IO (Either String Package)
signPackageForConfiguredUser pkg = do
  config@ClientConfig{..} <- throwLeft $ readConfigFile
  (_, pk) <- throwLeft $ getAddressAndPKForAlias ccUserAlias config
  putTextLn "You are going to sign the following package:\n"
  putTextLn $ pretty pkg
  confirmationResult <- confirmAction
  case confirmationResult of
    Canceled -> pure $ Left $ "Package signing was canceled"
    Confirmed -> do
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

-- | Write the configuration values to file as JSON in the following way.
--
-- 1. If none of the configuration values were provided, write the file
-- with placeholders for missing values, and notify the user.
--
-- 2. If a configuration file exists alredy, do not overwrite it, no matter
-- what.
setupClient :: ClientConfigPartial -> IO ()
setupClient configPartial = do
  (appPath, configFilePath) <- getFullAppAndConfigPath
  fileExists <- doesFileExist configFilePath
  if fileExists  then
    putStrLn $
      "Not overwriting the existing config file at, \n\n" <> configFilePath <>
      "\n\nPlease remove the file and try again"
  else do
    createDirectoryIfMissing True appPath
    case toConfigFilled configPartial of
      Just config -> do
        BSL.writeFile configFilePath $ encodePretty config
        putStrLn $ "Config file has been written to " <> configFilePath
      Nothing -> do
        BSL.writeFile configFilePath $ encodePretty configPartial
        putStrLn $ "Since the required configuration values were missing from the command, \
          \a config file, with placeholders in place of missing values has been \
          \written to, \n\n" <> configFilePath <> "\n\n\
          \Use `tzbtc-client setupClient --help` to see the command arguments."

writeConfig :: (ToJSON a) => a -> IO ()
writeConfig c =  do
  putTextLn "Writing config to"
  (_, configFilePath) <- getFullAppAndConfigPath
  putStrLn  configFilePath
  BSL.writeFile configFilePath $ encodePretty c

runTzbtcContract :: Parameter s -> IO ()
runTzbtcContract param = do
  config@ClientConfig{..} <- throwLeft $ readConfigFile
  runTransaction ccContractAddress param config

runMultisigContract :: NonEmpty Package -> IO ()
runMultisigContract packages = do
  config <- throwLeft $ readConfigFile
  package <- throwLeft $ pure $ mergePackages packages
  multisigAddr <- throwLeft $ pure (fst <$> getToSign package)
  (_, (_, keys')) <- getMultisigStorage multisigAddr config
  (_, multisigParam) <- throwLeft $ pure $ mkMultiSigParam keys' packages
  runTransaction multisigAddr multisigParam config

checkConfig :: IO ()
checkConfig = do
  econfig <- readConfigFile @ClientConfig
  case econfig of
    Left _ -> putTextLn "WARNING! Config file is incomplete."
    Right _ -> pass

runConfigEdit :: Bool -> ClientConfigPartial -> IO ()
runConfigEdit doEdit configPartial = do
  (_, path) <- getFullAppAndConfigPath
  if doEdit then
    if hasDelta configPartial then do
      econfig <- readConfigFile
      case econfig of
        Left err -> putTextLn (pretty err)
        Right config -> writeConfig $ mergeConfig config configPartial
    else editConfigViaEditor path
  else printConfig path
  checkConfig
  where
    -- Is there any change to the config at all?
    hasDelta ccp =
      isAvailable (ccNodeAddress ccp) || isAvailable (ccNodePort ccp) ||
      isAvailable (ccContractAddress ccp) || isAvailable (ccMultisigAddress ccp) ||
      isAvailable (ccUserAlias ccp) || isAvailable (ccTezosClientExecutable ccp)

    mergeConfig :: ClientConfigText -> ClientConfigPartial -> ClientConfigText
    mergeConfig cc ccp = ClientConfig
      (withDefaultConfig (ccNodeAddress cc) (ccNodeAddress ccp))
      (withDefaultConfig (ccNodePort cc) (ccNodePort ccp))
      (withDefaultConfig (ccContractAddress cc) (ccContractAddress ccp))
      (withDefaultConfig (ccMultisigAddress cc) (ccMultisigAddress ccp))
      (withDefaultConfig (ccUserAlias cc) (ccUserAlias ccp))
      (withDefaultConfig (ccTezosClientExecutable cc) (ccTezosClientExecutable ccp))

    withDefaultConfig :: TextConfig a -> Partial s a -> TextConfig a
    withDefaultConfig _ (Available a) = ConfigVal a
    withDefaultConfig a Unavilable = a

printConfig :: FilePath -> IO ()
printConfig path = do
  fileContents <- readFile path
  putTextLn "Config file path:"
  putTextLn "-----------------"
  putStrLn path
  putTextLn ""
  putTextLn "Config file content:"
  putTextLn "--------------------"
  BSL.putStrLn $ BSL.fromStrict $ fileContents
