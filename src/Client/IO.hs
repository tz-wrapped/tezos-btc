{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.IO
  ( runConfigEdit
  , runTzbtcContract
  , getAllowance
  , getBalance
  , getPackageFromFile
  , writePackageToFile
  , setupClient
  , addrOrAliasToAddr
  , signPackageForConfiguredUser
  , runMultisigContract
  , createMultisigPackage
  , getTzbtcStorage
  , getFromTzbtcStorage
  , mainProgram
  ) where

import Fmt (pretty)

import Data.Version (showVersion)
import Fmt (pretty)
import Options.Applicative
  (footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)

import Lorentz.Macro (View(..))
import Michelson.Typed.Haskell.Value (ContractAddr(..))
import Paths_tzbtc (version)
import Util.Named ((.!))

import Client.Parser
import Client.Types
import Lorentz.Contracts.TZBTC
import Util.MultiSig
import Util.AbstractIO


import Tezos.Address
import Options.Applicative.Help.Pretty (Doc, linebreak)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BSL hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL (putStrLn)
import Data.Aeson.Encode.Pretty (encodePretty)
import Util.AbstractIO
import Util.Editor
import Client.Types
import Client.Error
import Client.Util
import Tezos.Crypto
import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.MultiSig as MSig
import Util.MultiSig as MSig
import Michelson.Interpret.Unpack (UnpackError)
import Servant.Client.Core as Servant (ClientError(..))
import Servant.Client as Servant (ResponseF(..))
import Network.HTTP.Types.Status (notFound404)
import Client.Parser

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

appName, configFile :: FilePath -- TODO Move this somewhere common
appName = "tzbtc"
configFile = "config.json"

data ConfirmationResult = Confirmed | Canceled
--
instance HasEditor IO where
  openEditor = editConfigViaEditor

instance HasFilesystem IO where
  writeFile = BS.writeFile
  writeFileUtf8 = UIO.writeFileUtf8
  readFile = BS.readFile
  doesFileExist = Directory.doesFileExist
  decodeFileStrict = Aeson.decodeFileStrict

instance HasCmdLine IO  where
  parseCmdLine = execParser
  printStringLn = putStrLn
  printLTextLn = LT.putStrLn
  printTextLn = T.putStrLn
  printByteString = BSC.putStrLn
  getLineFromUser = getLine

instance HasConfig IO where
  getConfigPaths = do
    configDir <- getUserConfigDir appName
    configPath <- getUserConfigFile appName configFile
    pure (DirPath configDir, configPath)
  readConfig = do
    (_, configPath) <- getConfigPaths
    fileExists <- doesFileExist configPath
    if fileExists then do
      mbConfig <- decodeFileStrict configPath
      case mbConfig of
        Just config -> return $ Right config
        Nothing -> return $ Left TzbtcClientConfigError
    else return $ Left $ TzbtcClientConfigFileNotFound configPath
  writeConfig c = do
    (_, configFilePath) <- getConfigPaths
    putStrLn  configFilePath
    BSL.writeFile configFilePath $ encodePretty c
  createDirectoryIfMissing f p = Directory.createDirectoryIfMissing f (unDirPath p)

instance HasTezosClient IO where
  getAddressAndPKForAlias a = do
    config <- throwLeft $ readConfig @_ @ClientConfig
    getAddressAndPKForAlias' a config
  signWithTezosClient a = do
    config <- throwLeft $ readConfig @_ @ClientConfig
    signWithTezosClient' a config

instance HasTezosRpc IO where
  runTransaction addr param = do
    config <- throwLeft $ readConfig @_ @ClientConfig
    runTransaction' addr param config
  getStorage addr = do
    config <- throwLeft $ readConfig @_ @ClientConfig
    clientEnv <- getClientEnv' config
    throwClientError $ runClientM (API.getStorage addr) clientEnv
  getCounter addr = do
    config <- throwLeft $ readConfig @_ @ClientConfig
    clientEnv <- getClientEnv' config
    throwClientError $ runClientM (API.getCounter addr) clientEnv
  getCosts ro = do
    config <- throwLeft $ readConfig @_ @ClientConfig
    clientEnv <- getClientEnv' config
    throwLeft $ getCosts' clientEnv ro
  getFromBigMap bid scriptExpr = do
    config@ClientConfig{..}  <- throwLeft $ readConfig @_ @ClientConfig
    clientEnv <- getClientEnv' config
    r <- runClientM (API.getFromBigMap bid scriptExpr) clientEnv
    case r of
      Left err -> pure $ Left $ TzbtcServantError err
      Right rawVal -> pure $ Right rawVal
  waitForOperation op = do
    config@ClientConfig{..}  <- throwLeft $ readConfig @_ @ClientConfig
    waitForOperationInclusion' op config

tezosNodeArgs :: ClientConfig -> [String]
tezosNodeArgs ClientConfig{..} = ["-A", toString ccNodeAddress, "-P", show ccNodePort]

getAddressAndPKForAlias'
  :: Text -> ClientConfig -> IO (Either TzbtcClientError (Address, PublicKey))
getAddressAndPKForAlias' alias config@ClientConfig{..} = do
  (exitCode, stdout', _) <- readProcessWithExitCode ccTezosClientExecutable
    (tezosNodeArgs config <> ["show", "address", toString alias]) ""
  case exitCode of
    ExitSuccess -> do
      addr <- throwLeft $ pure $ (parseAddressFromOutput $ toText stdout')
      pure $ Right addr
    ExitFailure _ -> pure $ Left $ TzbtcUnknownAliasError alias

signWithTezosClient'
  :: Either InternalByteString Text -> ClientConfig -> IO (Either Text Signature)
signWithTezosClient' bs config@ClientConfig{..} = do
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

getOperationHex
  :: ClientEnv -> ForgeOperation
  -> IO (Either ClientError InternalByteString)
getOperationHex env forgeOp = runClientM (API.forgeOperation forgeOp) env

injectOp :: Text -> Signature -> ClientConfig -> IO Text
injectOp opToInject sign config = do
  clientEnv <- getClientEnv' config
  throwClientError $ runClientM
    (API.injectOperation (Just "main") $ prepareForInjection opToInject sign) clientEnv

getCurrentChainId :: ClientEnv -> IO (Either ClientError Text)
getCurrentChainId env = runClientM API.getMainChainId env

getLastBlockHash :: ClientEnv -> IO (Either ClientError Text)
getLastBlockHash env = runClientM API.getLastBlock env

getAddressCounter :: ClientEnv -> Address -> IO (Either ClientError TezosWord64)
getAddressCounter env address = runClientM (API.getCounter $ formatAddress address) env

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
  -- Forge operation with given limits and get its hexadecimal representation
  }

stubSignature :: Signature
stubSignature = unsafeParseSignature
  "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"

addOperationPrefix :: InternalByteString -> InternalByteString
addOperationPrefix (InternalByteString bs) =
  InternalByteString $ cons 0x03 bs

runTransaction'
  :: (ParamConstraints param)
  => Address -> param -> ClientConfig -> IO ()
runTransaction' to param config@ClientConfig{..} = do
  clientEnv <- getClientEnv' config
  lastBlockHash <- throwClientError $ getLastBlockHash clientEnv
  (sourceAddr, _) <- throwLeft $ getAddressAndPKForAlias' ccUserAlias config
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
  (consumedGas, storageSize) <- throwLeft $ getCosts' clientEnv runOp
  hex <- throwClientError $ getOperationHex clientEnv ForgeOperation
    { foBranch = lastBlockHash
    , foContents = [opToRun { toGasLimit = consumedGas + 200
                            , toStorageLimit = storageSize + 20
                            , toFee = calcFees consumedGas storageSize
                            }]
    }
  signRes <- signWithTezosClient' (Left $ addOperationPrefix hex) config
  case signRes of
    Left err -> putStrLn err
    Right signature' -> do
      opHash <- injectOp (formatByteString hex) signature' config
      putStrLn $ "Operation hash: " <> opHash
      waitForOperationInclusion' opHash config

-- | Add header, required by the Tezos RPC interface
fixRequest :: Request -> IO Request
fixRequest req = return $
  req { requestHeaders = ("Content-Type", "application/json") :
        filter (("Content-Type" /=) . fst) (requestHeaders req)
      }

getClientEnv' :: ClientConfig -> IO ClientEnv
getClientEnv' ClientConfig{..} = do
  manager' <- newManager $ defaultManagerSettings { managerModifyRequest = fixRequest }
  let nodeUrl = BaseUrl Http (toString ccNodeAddress) ccNodePort ""
  return $ mkClientEnv manager' nodeUrl

getCosts'
  :: ClientEnv -> RunOperation
  -> IO (Either TzbtcClientError (TezosWord64, TezosWord64))
getCosts' env runOp = do
  req <- runClientM (API.runOperation runOp) env
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

waitForOperationInclusion' :: Text -> ClientConfig -> IO ()
waitForOperationInclusion' op config@ClientConfig{..} = do
  (exitCode, stdout', stderr') <- readProcessWithExitCode ccTezosClientExecutable
    (tezosNodeArgs config <> ["wait", "for", toString op, "to", "be", "included"]) ""
  case exitCode of
    ExitSuccess -> putStr stdout'
    ExitFailure _ -> putStr stderr'

runConfigEdit
  :: (MonadThrow m, HasEditor m, HasConfig m, HasCmdLine m, HasFilesystem m)
  => Bool -> ClientConfigPartial -> m ()
runConfigEdit doEdit configPartial = do
  (_, path) <- getConfigPaths
  if doEdit then
    if hasDelta configPartial then do
      econfig <- readConfig
      case econfig of
        Left err -> printTextLn (pretty err)
        Right config -> writeConfig $ mergeConfig config configPartial
    else openEditor path (writeFile path)
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

checkConfig :: (HasCmdLine m, HasConfig m) => m ()
checkConfig = do
  econfig <- readConfig @_ @ClientConfig
  case econfig of
    Left _ -> printTextLn "WARNING! Config file is incomplete."
    Right _ -> pass

printConfig :: (HasFilesystem m, HasCmdLine m) => FilePath -> m ()
printConfig path = do
  fileContents <- readFile path
  printTextLn "Config file path:"
  printTextLn "-----------------"
  printStringLn path
  printTextLn ""
  printTextLn "Config file content:"
  printStringLn "--------------------"
  printByteString fileContents

-- | Write the configuration values to file as JSON in the following way.
--
-- 1. If none of the configuration values were provided, write the file
-- with placeholders for missing values, and notify the user.
--
-- 2. If a configuration file exists alredy, do not overwrite it, no matter
-- what.
setupClient
  :: (HasCmdLine m, HasFilesystem m, HasConfig m)
  => ClientConfigPartial
  -> m ()
setupClient configPartial = do
  (appPath, configFilePath) <- getConfigPaths
  fileExists <- doesFileExist configFilePath
  if fileExists  then
    printStringLn $
      "Not overwriting the existing config file at, \n\n" <> configFilePath <>
      "\n\nPlease remove the file and try again"
  else do
    createDirectoryIfMissing True appPath
    case toConfigFilled configPartial of
      Just config -> do
        writeConfig config
        printStringLn $ "Config file has been written to " <> configFilePath
      Nothing -> do
        writeConfig configPartial
        printStringLn $ "Since the required configuration values were missing from the command, \
          \a config file, with placeholders in place of missing values has been \
          \written to, \n\n" <> configFilePath <> "\n\n\
          \Use `tzbtc-client setupClient --help` to see the command arguments."

addrOrAliasToAddr :: (MonadThrow m, HasTezosClient m, HasConfig m) => Text -> m Address
addrOrAliasToAddr addrOrAlias =
  case parseAddress addrOrAlias of
    Right addr -> pure addr
    Left _ -> fst <$> (throwLeft $ getAddressAndPKForAlias addrOrAlias)

runTzbtcContract :: (MonadThrow m, HasTezosRpc m, HasConfig m, HasTezosRpc m) => Parameter s -> m ()
runTzbtcContract param = do
  ClientConfig{..} <- throwLeft $ (readConfig @_ @ClientConfig)
  runTransaction ccContractAddress param

runMultisigContract :: (MonadThrow m, HasTezosRpc m, HasConfig m) => NonEmpty Package -> m ()
runMultisigContract packages = do
  config <- throwLeft $ readConfig
  package <- throwLeft $ pure $ mergePackages packages
  multisigAddr <- throwLeft $ pure (fst <$> getToSign package)
  (_, (_, keys')) <- getMultisigStorage multisigAddr config
  (_, multisigParam) <- throwLeft $ pure $ mkMultiSigParam keys' packages
  runTransaction multisigAddr multisigParam

getMultisigStorage
  :: (MonadThrow m, HasTezosRpc m) => Address -> ClientConfig -> m MSig.Storage
getMultisigStorage addr config@ClientConfig{..} = do
  mSigStorageRaw <- getStorage $ formatAddress addr -- @REVIEW
  throwLeft $ pure $ exprToValue @MSig.Storage mSigStorageRaw

createMultisigPackage :: (MonadThrow m, HasConfig m, HasTezosRpc m, HasTezosClient m) => FilePath -> SafeParameter s -> m ()
createMultisigPackage packagePath parameter = do
  config@ClientConfig{..} <- throwLeft $ readConfig
  case ccMultisigAddress of
    Nothing -> throwLeft $ pure $ Left TzbtcMutlisigConfigUnavailable
    Just msAddr ->  do
      (counter, _) <- getMultisigStorage msAddr config
      let package = mkPackage msAddr counter ccContractAddress parameter
      writePackageToFile package packagePath

writePackageToFile :: (HasFilesystem m) => Package -> FilePath -> m ()
writePackageToFile package fileToWrite =
  writeFile fileToWrite $ encodePackage package

getPackageFromFile :: (HasFilesystem m) => FilePath -> m (Either Text Package)
getPackageFromFile packageFilePath = do
  fileContents <- readFile packageFilePath
  return $ case decodePackage fileContents of
    Left err -> Left $ "Failed to decode multisig package: " <> fromString err
    Right package -> Right package

confirmAction :: (HasCmdLine m) => m ConfirmationResult
confirmAction = do
  printStringLn "Are you sure? [Y/N]"
  res <- getLineFromUser
  case res of
    x | x `elem` ["Y", "y", "yes"] -> pure Confirmed
    x | x `elem` ["N", "n", "no"] -> pure Canceled
    _ -> confirmAction

signPackageForConfiguredUser
  :: ( MonadThrow m , HasCmdLine m , HasTezosClient m, HasConfig m)
  => Package
  -> m (Either String Package)
signPackageForConfiguredUser pkg = do
  config@ClientConfig{..} <- throwLeft $ readConfig @_ @ClientConfig
  (_, pk) <- throwLeft $ getAddressAndPKForAlias ccUserAlias
  printStringLn "You are going to sign the following package:\n"
  printStringLn $ pretty pkg
  confirmationResult <- confirmAction
  case confirmationResult of
    Canceled -> pure $ Left $ "Package signing was canceled"
    Confirmed -> do
      signRes <- signWithTezosClient (Right $ getBytesToSign pkg)
      case signRes of
        Left err -> pure $ Left $ toString err
        Right signature' -> pure $ addSignature pkg (pk, signature')

getFromLedger :: (MonadThrow m, HasTezosRpc m, HasConfig m) => Address -> m (Either UnpackError (Natural, Map Address Natural))
getFromLedger addr = do
  AlmostStorage{..} <- getTzbtcStorage
  let scriptExpr = encodeBase58Check $ valueToScriptExpr addr
  ledgerValueRaw <- getFromBigMap asBigMapId scriptExpr
  case ledgerValueRaw of
    Left err -> case err of
      TzbtcServantError (Servant.FailureResponse _ Servant.Response{..}) -> do
        if responseStatusCode == notFound404 then pure $ Right (0, Map.empty)
        else throwM err
      _ -> throwM err
    Right rawVal -> pure $
      exprToValue @(Natural, Map Address Natural) rawVal

getTzbtcStorage
  :: (MonadThrow m, HasConfig m, HasTezosRpc m) => m (AlmostStorage Interface)
getTzbtcStorage = do
  config <- throwLeft $ readConfig @_ @ClientConfig
  storageRaw <- getStorage $ formatAddress $ ccContractAddress config
  throwLeft $ pure $ exprToValue @(AlmostStorage Interface) storageRaw

getBalance :: (HasTezosRpc m) => Address -> m Natural
getBalance addr = do
  (balance, _) <- throwLeft $ getFromLedger addr
  pure balance

getAllowance :: (HasTezosRpc m) => Address -> Address -> m Natural
getAllowance owner spender = do
  (_, allowanceMap) <- throwLeft $ getFromLedger owner
  maybe (pure 0) pure $ Map.lookup spender allowanceMap

getFromTzbtcStorage :: (HasTezosRpc m) => (AlmostStorage Interface -> a) -> m a
getFromTzbtcStorage field = do
  storage <- getTzbtcStorage
  pure $ field storage

mainProgram :: (MonadThrow m, MonadFail m, HasTezosRpc m, HasTezosClient m, HasEditor m, HasCmdLine m) => m ()
mainProgram = do
  ClientArgs cmd dryRunFlag <- parseCmdLine programInfo
  case dryRunFlag of
    True -> pass
    False -> case cmd of
      CmdConfig editFlag partialConfig ->
        runConfigEdit editFlag partialConfig
      CmdSetupClient config -> setupClient config
      CmdMint to' value mbMultisig -> do
        to <- addrOrAliasToAddr to'
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ Mint (#to .! to, #value .! value)
      CmdBurn burnParams mbMultisig ->
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ Burn burnParams
      CmdTransfer from' to' value -> do
        [from, to] <- mapM addrOrAliasToAddr [from', to']
        runTzbtcContract $
          fromFlatParameter $ Transfer (#from .! from, #to .! to, #value .! value)
      CmdApprove spender' value -> do
        spender <- addrOrAliasToAddr spender'
        runTzbtcContract $
          fromFlatParameter $ Approve (#spender .! spender, #value .! value)
      CmdGetAllowance (owner', spender') mbCallback' ->
        case mbCallback' of
          Just callback' -> do
            [owner, spender, callback] <- mapM addrOrAliasToAddr [owner', spender', callback']
            runTzbtcContract $ fromFlatParameter $ GetAllowance $
              View (#owner .! owner, #spender .! spender) (ContractAddr callback)
          Nothing -> do
            [owner, spender] <- mapM addrOrAliasToAddr [owner', spender']
            allowance <- getAllowance owner spender
            printTextLn $ "Allowance: " <> show allowance
      CmdGetBalance owner' mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            [owner, callback] <- mapM addrOrAliasToAddr [owner', callback']
            runTzbtcContract $
              fromFlatParameter $ GetBalance $ View owner (ContractAddr callback)
          Nothing -> do
            owner <- addrOrAliasToAddr owner'
            balance <- getBalance owner
            printTextLn $ "Balance: " <> show balance
      CmdAddOperator operator' mbMultisig -> do
        operator <- addrOrAliasToAddr operator'
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ AddOperator (#operator .! operator)
      CmdRemoveOperator operator' mbMultisig -> do
        operator <- addrOrAliasToAddr operator'
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ RemoveOperator (#operator .! operator)
      CmdPause mbMultisig -> runMultisigTzbtcContract mbMultisig $
        fromFlatParameter $ Pause ()
      CmdUnpause mbMultisig -> runMultisigTzbtcContract mbMultisig $
        fromFlatParameter $ Unpause ()
      CmdSetRedeemAddress redeem' mbMultisig -> do
        redeem <- addrOrAliasToAddr redeem'
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ SetRedeemAddress (#redeem .! redeem)
      CmdTransferOwnership newOwner' mbMultisig -> do
        newOwner <- addrOrAliasToAddr newOwner'
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ TransferOwnership (#newOwner .! newOwner)
      CmdAcceptOwnership p -> runTzbtcContract $
        fromFlatParameter $ AcceptOwnership p
      CmdGetTotalSupply mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            callback <- addrOrAliasToAddr callback'
            runTzbtcContract $
              fromFlatParameter $ GetTotalSupply $ View () (ContractAddr callback)
          Nothing -> do
            error "To be done in TBTC-55"
            --printFieldFromStorage "Total supply: " (totalSupply . asFields) show
      CmdGetTotalMinted mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            callback <- addrOrAliasToAddr callback'
            runTzbtcContract $
              fromFlatParameter $ GetTotalMinted $ View () (ContractAddr callback)
          Nothing ->
            error "To be done in TBTC-55"
            --printFieldFromStorage "Total minted: " (totalMinted . asFields) show
      CmdGetTotalBurned mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            callback <- addrOrAliasToAddr callback'
            runTzbtcContract $
              fromFlatParameter $ GetTotalBurned $ View () (ContractAddr callback)
          Nothing ->
            error "To be done in TBTC-55"
            --printFieldFromStorage "Total burned: " (totalMinted . asFields) show
      CmdGetAdministrator mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            callback <- addrOrAliasToAddr callback'
            runTzbtcContract $
              fromFlatParameter $ GetAdministrator $ View () (ContractAddr callback)
          Nothing ->
            error "To be done in TBTC-55"
            --printFieldFromStorage
            --"Admininstator: " (admin . asFields) formatAddress
      CmdGetOpDescription packageFilePath -> do
        pkg <- getPackageFromFile packageFilePath
        case pkg of
          Left err -> printTextLn err
          Right package -> printStringLn $ pretty package
      CmdGetBytesToSign packageFilePath -> do
        pkg <- getPackageFromFile packageFilePath
        case pkg of
          Left err -> printTextLn err
          Right package -> printTextLn $ getBytesToSign package
      CmdAddSignature pk sign packageFilePath -> do
        pkg <- getPackageFromFile packageFilePath
        case pkg of
          Left err -> printTextLn err
          Right package -> case addSignature package (pk, sign) of
            Right signedPackage -> writePackageToFile signedPackage packageFilePath
            Left err -> printStringLn err
      CmdSignPackage packageFilePath -> do
        pkg <- getPackageFromFile packageFilePath
        case pkg of
          Left err -> printTextLn err
          Right package -> do
            signRes <- signPackageForConfiguredUser package
            case signRes of
              Left err -> printStringLn err
              Right signedPackage -> writePackageToFile signedPackage packageFilePath
      CmdCallMultisig packagesFilePaths -> do
        pkgs <- fmap sequence $ mapM getPackageFromFile packagesFilePaths
        case pkgs of
          Left err -> printTextLn err
          Right packages -> runMultisigContract packages
  where
    runMultisigTzbtcContract :: (HasCmdLine m, HasTezosRpc m) => (Maybe FilePath) -> Parameter a -> m ()
    runMultisigTzbtcContract mbMultisig param =
      case mbMultisig of
        Just fp -> case toSafeParam param of
          Just subParam -> createMultisigPackage fp subParam
          _ -> printStringLn "Unable to call multisig for View entrypoints"
        Nothing -> runTzbtcContract param
    _printFieldFromStorage :: Text -> (AlmostStorage Interface -> a) -> (a -> Text) -> IO ()
    _printFieldFromStorage prefix fieldGetter formatter = do
      field' <- getFromTzbtcStorage fieldGetter
      putTextLn $ prefix <> formatter field'
    programInfo =
      info (helper <*> versionOption <*> clientArgParser) $
      mconcat
        [ fullDesc
        , progDesc
            "TZBTC - Wrapped bitcoin on tezos blockchain"
        , header "TZBTC Client"
        , footerDoc $ usageDoc
        ]
    versionOption =
      infoOption
        ("tzbtc-" <> showVersion version)
        (long "version" <> help "Show version.")
    usageDoc :: Maybe Doc
    usageDoc =
      Just $ mconcat
      [ "You can use help for specific COMMAND", linebreak
      , "EXAMPLE:", linebreak
      , "  tzbtc-client mint --help", linebreak
      , "USAGE EXAMPLE:", linebreak
      , "  tzbtc-client mint --to tz1U1h1YzBJixXmaTgpwDpZnbrYHX3fMSpvb --value 100500", linebreak
      , linebreak
      , "  This command will perform transaction insertion", linebreak
      , "  to the chain.", linebreak
      , "  Operation hash is returned as a result.", linebreak
      ]

