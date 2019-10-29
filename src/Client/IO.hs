{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# OPTIONS_GHC -Wno-orphans #-}
module Client.IO
  ( runConfigEdit
  , runTzbtcContract
  , deployTzbtcContract
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
  --, getFromTzbtcStorage
  , mainProgram
  ) where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson as Aeson (ToJSON, FromJSON, decodeFileStrict)
import Data.ByteString (cons)
import qualified Data.ByteString.Lazy as BSL hiding (putStrLn)
import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.ByteString.Char8 as BSC (putStrLn)
import qualified Data.Map as Map
import Data.Version (showVersion)
import Data.Vinyl
import Fmt (pretty)
import Network.HTTP.Types.Status (notFound404)
import Network.HTTP.Client (ManagerSettings(..), Request(..), defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
  (footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Options.Applicative as Opt hiding (value)
import qualified System.Directory as Directory (doesFileExist, createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserConfigFile)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Servant.Client.Core as Servant (ClientError(..))
import Servant.Client as Servant (ResponseF(..))
import Servant.Client
  (BaseUrl(..), ClientEnv, Scheme(..), mkClientEnv, runClientM)

import Lorentz hiding (balance, map, cons, address, chainId)
import Lorentz.Macro (View(..))
import Lorentz.UStore.Common
import Lorentz.UStore.Migration (manualConcatMigrationScripts)
import Lorentz.Contracts.ManagedLedger.Types
import Michelson.Runtime.GState (genesisAddress1, genesisAddress2)
import Michelson.Typed.Haskell.Value (ContractAddr(..))
import Michelson.Untyped (InternalByteString(..))
import Paths_tzbtc (version)
import Tezos.Address
import Tezos.Crypto hiding (sign)
import Tezos.Json (TezosWord64(..))
import Util.Named ((.!))
import Util.TypeLits

import qualified Client.API as API
import Client.Crypto
import Client.Error
import Client.Parser
import Client.Types
import Client.Util
import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.MultiSig as MSig
import Lorentz.Contracts.TZBTC.Types (StorageFields(..))
import Lorentz.Contracts.TZBTC.Preprocess (migrationScripts, tzbtcContract, tzbtcContractRouter)
import Lorentz.Contracts.TZBTC.V0 (mkEmptyStorageV0)
import Lorentz.Contracts.TZBTC.V1 (originationParams)
import Util.AbstractIO
import Util.Editor
import Util.MultiSig
import qualified Util.IO as UIO (writeFileUtf8)

appName, configFile :: FilePath
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
  createDirectoryIfMissing f p = Directory.createDirectoryIfMissing f (unDirPath p)

instance HasCmdLine IO  where
  parseCmdLine = execParser
  printTextLn = putStrLn
  printStringLn = putStrLn
  printByteString = BSC.putStrLn
  getLineFromUser = getLine

instance HasConfig IO where
  getConfigPaths = do
    configDir <- getUserConfigDir appName
    configPath <- getUserConfigFile appName configFile
    pure (DirPath configDir, configPath)
  readConfigText = readConfig_
  readConfig = readConfig_
  writeConfigFull = writeConfig
  writeConfigPartial = writeConfig
  writeConfigText = writeConfig

readConfig_ :: (Aeson.FromJSON a, HasConfig m) => m (Either TzbtcClientError a)
readConfig_ = do
  (_, configPath) <- getConfigPaths
  fileExists <- doesFileExist configPath
  if fileExists then do
    mbConfig <- decodeFileStrict configPath
    case mbConfig of
      Just config -> return $ Right config
      Nothing -> return $ Left TzbtcClientConfigError
  else return $ Left $ TzbtcClientConfigFileNotFound configPath

instance HasTezosClient IO where
  getAddressAndPKForAlias a = do
    config <- throwLeft $ readConfig
    getAddressAndPKForAlias' a config
  signWithTezosClient a = do
    config <- throwLeft $ readConfig
    signWithTezosClient' (first InternalByteString a) config
  waitForOperation op = do
    config@ClientConfig{..}  <- throwLeft $ readConfig
    waitForOperationInclusion' op config

instance HasTezosRpc IO where
  runTransactions addr param = do
    config <- throwLeft $ readConfig
    runTransactions' addr param config
  getStorage addr = do
    config <- throwLeft $ readConfig
    clientEnv <- getClientEnv' config
    throwClientError $ runClientM (API.getStorage addr) clientEnv
  getCounter addr = do
    config <- throwLeft $ readConfig
    clientEnv <- getClientEnv' config
    throwClientError $ runClientM (API.getCounter addr) clientEnv
  getFromBigMap bid scriptExpr = do
    config@ClientConfig{..}  <- throwLeft $ readConfig
    clientEnv <- getClientEnv' config
    r <- runClientM (API.getFromBigMap bid scriptExpr) clientEnv
    case r of
      Left err -> pure $ Left $ TzbtcServantError err
      Right rawVal -> pure $ Right rawVal
  deployTzbtcContract address redeem = do
    deployTzbtcContract' address redeem

writeConfig :: (Aeson.ToJSON a) => a -> IO ()
writeConfig c = do
  (_, configFilePath) <- getConfigPaths
  putStrLn  configFilePath
  BSL.writeFile configFilePath $ encodePretty c

tezosNodeArgs :: ClientConfig -> [String]
tezosNodeArgs ClientConfig{..} = ["-A", toString ccNodeAddress, "-P", show ccNodePort]
  ++ bool [] ["-S"] ccNodeUseHttps

waitForOperationInclusion :: Text -> ClientConfig -> IO ()
waitForOperationInclusion op config@ClientConfig{..} = do
  (exitCode, stdout', stderr') <- readProcessWithExitCode ccTezosClientExecutable
    (tezosNodeArgs config <> ["wait", "for", toString op, "to", "be", "included"]) ""
  case exitCode of
    ExitSuccess -> putStr stdout'
    ExitFailure _ -> putStr stderr'

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

getLastBlockHash :: ClientEnv -> IO (Either ClientError Text)
getLastBlockHash env = runClientM API.getLastBlock env

getCurrentBlockConstants :: ClientEnv -> Text -> IO (Either ClientError BlockConstants)
getCurrentBlockConstants env block = runClientM (API.getBlockConstants block) env

getAddressCounter :: ClientEnv -> Address -> IO (Either ClientError TezosWord64)
getAddressCounter env address = runClientM (API.getCounter $ formatAddress address) env

dumbOp :: TransactionOperation
dumbOp = TransactionOperation
  { toKind = "transaction"
  , toSource = genesisAddress1
  , toFee = 50000
  , toCounter = 0
  , toGasLimit = 800000
  , toStorageLimit = 60000
  , toAmount = 0
  , toDestination = genesisAddress2
  , toParameters = ParametersInternal
    { piEntrypoint = "default"
    , piValue = nicePackedValueToExpression $ fromFlatParameter $ Pause ()
    }
  -- Forge operation with given limits and get its hexadecimal representation
  }

stubSignature :: Signature
stubSignature = unsafeParseSignature
  "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"

addOperationPrefix :: InternalByteString -> InternalByteString
addOperationPrefix (InternalByteString bs) =
  InternalByteString $ cons 0x03 bs

-- | Datatype that contains various values required for
-- chain operations.
data OperationConstants = OperationConstants
  { ocClientEnv :: ClientEnv
  -- ^ Environment required for network operations
  , ocLastBlockHash :: Text
  -- ^ Block in which operations is going to be injected
  , ocSourceAddr :: Address
  -- ^ The address of the operations sender
  , ocBlockConstants :: BlockConstants
  -- ^ Information about block: chain_id and protocol
  , ocCounter :: TezosWord64
  -- ^ Sender counter
  }

-- | Function which obtains `OperationConstant` by given
-- `ClientConfig`.
preProcessOperation :: ClientConfig -> IO OperationConstants
preProcessOperation config@ClientConfig{..} = do
  ocClientEnv <- getClientEnv' config
  ocLastBlockHash <- throwClientError $ getLastBlockHash ocClientEnv
  (ocSourceAddr, _) <- throwLeft $ getAddressAndPKForAlias' ccUserAlias config
  ocBlockConstants <-
    throwClientError $ getCurrentBlockConstants ocClientEnv ocLastBlockHash
  ocCounter <- throwClientError $ getAddressCounter ocClientEnv ocSourceAddr
  pure OperationConstants{..}

-- | Function which waits for operation to be included into
-- the chain.
postProcessOperation :: ClientConfig -> Text -> IO ()
postProcessOperation config opHash = do
  putStrLn $ "Operation hash: " <> opHash
  waitForOperationInclusion opHash config

toParametersInternals :: (NicePackedValue param) => EntrypointParam param -> ParametersInternal
toParametersInternals = \case
  DefaultEntrypoint param -> ParametersInternal
    { piEntrypoint = "default"
    , piValue = nicePackedValueToExpression param
    }
  Entrypoint ep param -> ParametersInternal
    { piEntrypoint = ep
    , piValue = nicePackedValueToExpression param
    }

-- It is possible to include multiple transaction within single operations batch
-- so we are accepting list of param's here.
runTransactions'
  :: (NicePackedValue param)
  => Address -> [EntrypointParam param] -> ClientConfig -> IO ()
runTransactions' to params config@ClientConfig{..} = do
  OperationConstants{..} <- preProcessOperation config
  let opsToRun = map (\(param, i) -> dumbOp
        { toDestination = to
        , toSource = ocSourceAddr
        , toCounter = ocCounter + (fromInteger i)
        , toParameters = toParametersInternals param
        }) $ zip params [1..]
  let runOp = RunOperation
        { roOperation =
          RunOperationInternal
          { roiBranch = ocLastBlockHash
          , roiContents = map (\opToRun -> Left opToRun) opsToRun
          , roiSignature = stubSignature
          }
        , roChainId = bcChainId ocBlockConstants
        }
  -- Perform run_operation with dumb signature in order
  -- to estimate gas cost, storage size and paid storage diff
  results <- getAppliedResults ocClientEnv (Left runOp)
  -- Forge operation with given limits and get its hexadecimal representation
  hex <- throwClientError $ getOperationHex ocClientEnv ForgeOperation
    { foBranch = ocLastBlockHash
    , foContents = map (\(opToRun, AppliedResult{..}) ->
                          Left $ opToRun
                          { toGasLimit = arConsumedGas + 200
                          , toStorageLimit = arStorageSize + 20
                          , toFee = calcFees arConsumedGas arPaidStorageDiff
                          }
                       ) $ zip opsToRun results
    }
  -- Sign operation with sender secret key
  signRes <- signWithTezosClient' (Left $ addOperationPrefix hex) config
  case signRes of
    Left err -> putStrLn err
    Right signature' -> do
      -- Sign and inject the operation
      injectOp (formatByteString hex) signature' config >>=
        (postProcessOperation config)

getAppliedResults
  :: ClientEnv -> Either RunOperation PreApplyOperation
  -> IO [AppliedResult]
getAppliedResults env op = do
  results <- case op of
    Left runOp ->
      (sequence . (: [])) <$> throwLeft $ runClientM (API.runOperation runOp) env
    Right preApplyOp ->
      throwLeft $ runClientM (API.preApplyOperations [preApplyOp]) env
  concatMapM handleRunRes results
  where
    handleRunRes :: MonadThrow m => RunRes -> m [AppliedResult]
    handleRunRes RunRes{..} =
      case rrOperationContents of
        [] -> throwM $ TzbtcUnexpectedRunResult "empty result"
        opContents ->
          mapM (\(OperationContent (RunMetadata res internalOps)) ->
                  let internalResults = map unInternalOperation internalOps in
                    case foldr combineResults res internalResults of
                      RunOperationApplied appliedRes -> pure appliedRes
                      RunOperationFailed errors -> throwM (TzbtcRunFailed errors)
               ) opContents

-- | Add header, required by the Tezos RPC interface
fixRequest :: Request -> IO Request
fixRequest req = return $
  req { requestHeaders = ("Content-Type", "application/json") :
        filter (("Content-Type" /=) . fst) (requestHeaders req)
      }

getClientEnv' :: ClientConfig -> IO ClientEnv
getClientEnv' ClientConfig{..} = do
  manager' <- newManager $ bool
    (defaultManagerSettings { managerModifyRequest = fixRequest })
    (tlsManagerSettings { managerModifyRequest = fixRequest })
    ccNodeUseHttps
  let nodeUrl = BaseUrl (bool Http Https ccNodeUseHttps)
                (toString ccNodeAddress) ccNodePort ""
  return $ mkClientEnv manager' nodeUrl

waitForOperationInclusion' :: Text -> ClientConfig -> IO ()
waitForOperationInclusion' op config@ClientConfig{..} = do
  (exitCode, stdout', stderr') <- readProcessWithExitCode ccTezosClientExecutable
    (tezosNodeArgs config <> ["wait", "for", toString op, "to", "be", "included"]) ""
  case exitCode of
    ExitSuccess -> putStr stdout'
    ExitFailure _ -> putStr stderr'

runConfigEdit
  :: (MonadThrow m, HasEditor m, HasConfig m, HasCmdLine m)
  => Bool -> ClientConfigPartial -> m ()
runConfigEdit doEdit configPartial = do
  (_, path) <- getConfigPaths
  if doEdit then
    if hasDelta configPartial then do
      econfig <- readConfigText
      case econfig of
        Left err -> printTextLn (pretty err :: Text)
        Right config -> writeConfigText $ mergeConfig config configPartial
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
      (withDefaultConfig (ccNodeUseHttps cc) (ccNodeUseHttps ccp))
      (withDefaultConfig (ccContractAddress cc) (ccContractAddress ccp))
      (withDefaultConfig (ccMultisigAddress cc) (ccMultisigAddress ccp))
      (withDefaultConfig (ccUserAlias cc) (ccUserAlias ccp))
      (withDefaultConfig (ccTezosClientExecutable cc) (ccTezosClientExecutable ccp))

    withDefaultConfig :: TextConfig a -> Partial s a -> TextConfig a
    withDefaultConfig _ (Available a) = ConfigVal a
    withDefaultConfig a Unavilable = a

checkConfig :: (HasCmdLine m, HasConfig m) => m ()
checkConfig = do
  econfig <- readConfig
  case econfig of
    Left _ -> printStringLn "WARNING! Config file is incomplete."
    Right _ -> pass

printConfig :: (HasFilesystem m, HasCmdLine m) => FilePath -> m ()
printConfig path = do
  fileContents <- readFile path
  printStringLn "Config file path:"
  printStringLn "-----------------"
  printStringLn path
  printStringLn ""
  printStringLn "Config file content:"
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
  :: (HasCmdLine m, HasConfig m)
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
        writeConfigFull config
        printStringLn $ "Config file has been written to " <> configFilePath
      Nothing -> do
        writeConfigPartial configPartial
        printStringLn $ "Since the required configuration values were missing from the command, \
          \a config file, with placeholders in place of missing values has been \
          \written to, \n\n" <> configFilePath <> "\n\n\
          \Use `tzbtc-client setupClient --help` to see the command arguments."

addrOrAliasToAddr :: (MonadThrow m, HasTezosClient m) => Text -> m Address
addrOrAliasToAddr addrOrAlias =
  case parseAddress addrOrAlias of
    Right addr -> pure addr
    Left _ -> fst <$> (throwLeft $ getAddressAndPKForAlias addrOrAlias)

runTzbtcContract :: (MonadThrow m, HasTezosRpc m) => Parameter s -> m ()
runTzbtcContract param = do
  ClientConfig{..} <- throwLeft readConfig
  case ccContractAddress of
    Nothing -> throwM TzbtcContractConfigUnavailable
    Just contractAddr ->
      runTransactions contractAddr [DefaultEntrypoint param]

runMultisigContract :: (MonadThrow m, MonadFail m, HasTezosRpc m) => NonEmpty Package -> m ()
runMultisigContract packages = do
  config <- throwLeft $ readConfig
  package <- throwLeft $ pure $ mergePackages packages
  multisigAddr <- throwLeft $ pure (fst <$> getToSign package)
  (_, (_, keys')) <- getMultisigStorage multisigAddr config
  (_, MSig.ParameterMain multisigParam) <- throwLeft $ pure $ mkMultiSigParam keys' packages
  runTransactions multisigAddr [Entrypoint "main" multisigParam]

getMultisigStorage
  :: (MonadThrow m, HasTezosRpc m) => Address -> ClientConfig -> m MSig.Storage
getMultisigStorage addr ClientConfig{..} = do
  mSigStorageRaw <- getStorage $ formatAddress addr
  throwLeft $ pure $ exprToValue @MSig.Storage mSigStorageRaw

createMultisigPackage :: (MonadThrow m, HasConfig m, HasTezosRpc m) => FilePath -> SafeParameter s -> m ()
createMultisigPackage packagePath parameter = do
  config@ClientConfig{..} <- throwLeft readConfig
  case ccMultisigAddress of
    Nothing -> throwLeft $ pure $ Left TzbtcMutlisigConfigUnavailable
    Just msAddr ->  do
      (counter, _) <- getMultisigStorage msAddr config
      case ccContractAddress of
        Nothing -> throwM TzbtcContractConfigUnavailable
        Just contractAddr ->
          let package = mkPackage msAddr counter contractAddr parameter in
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

confirmAction :: (HasCmdLine m) => Text -> m ConfirmationResult
confirmAction msg = do
  printTextLn $ msg <> " [Y/N]"
  res <- getLineFromUser
  case res of
    x | x `elem` ["Y", "y", "yes"] -> pure Confirmed
    x | x `elem` ["N", "n", "no"] -> pure Canceled
    _ -> confirmAction msg

signPackageForConfiguredUser
  :: ( MonadThrow m , HasCmdLine m , HasTezosClient m)
  => Package
  -> m (Either String Package)
signPackageForConfiguredUser pkg = do
  ClientConfig{..} <- throwLeft readConfig
  (_, pk) <- throwLeft $ getAddressAndPKForAlias ccUserAlias
  printStringLn "You are going to sign the following package:\n"
  printStringLn $ pretty pkg
  confirmationResult <- confirmAction "Are you sure?"
  case confirmationResult of
    Canceled -> pure $ Left $ "Package signing was canceled"
    Confirmed -> do
      signRes <- signWithTezosClient (Right $ getBytesToSign pkg)
      case signRes of
        Left err -> pure $ Left $ toString err
        Right signature' -> pure $ addSignature pkg (pk, signature')

getTzbtcStorage
  :: (MonadThrow m, HasTezosRpc m) => Address -> m (AlmostStorage Interface)
getTzbtcStorage contractAddr = do
  storageRaw <- getStorage $ formatAddress contractAddr
  throwLeft $ pure $ exprToValue @(AlmostStorage Interface) storageRaw

getBalance :: (HasTezosRpc m) => Address -> m Natural
getBalance addr = do
  mbLedgerValue <- getValueFromTzbtcUStoreSubmap @"ledger" @Address @LedgerValue addr
  case mbLedgerValue of
    Nothing -> pure 0
    Just ledgerValue -> pure $ arg #balance . fst $ ledgerValue

getAllowance :: (HasTezosRpc m) => Address -> Address -> m Natural
getAllowance owner spender = do
  mbLedgerValue <- getValueFromTzbtcUStoreSubmap @"ledger" @Address @LedgerValue owner
  case mbLedgerValue of
    Nothing -> pure 0
    Just ledgerValue -> let approvals = arg #approvals . snd $ ledgerValue in
      pure $ maybe 0 id $ Map.lookup spender approvals

--getFromTzbtcStorage :: (HasTezosRpc m) => (AlmostStorage Interface -> a) -> m a
--getFromTzbtcStorage field = do
--  storage <- getTzbtcStorage
--  pure $ field storage

mainProgram
  :: ( MonadThrow m , MonadFail m, HasTezosRpc m, HasEditor m, HasCmdLine m
     ) => m ()
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
            printStringLn $ "Allowance: " <> show allowance
      CmdGetBalance owner' mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            [owner, callback] <- mapM addrOrAliasToAddr [owner', callback']
            runTzbtcContract $
              fromFlatParameter $ GetBalance $ View (#owner .! owner) (ContractAddr callback)
          Nothing -> do
            owner <- addrOrAliasToAddr owner'
            balance <- getBalance owner
            printStringLn $ "Balance: " <> show balance
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
            printFieldFromStorage @Natural #totalSupply "Total supply: " show
      CmdGetTotalMinted mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            callback <- addrOrAliasToAddr callback'
            runTzbtcContract $
              fromFlatParameter $ GetTotalMinted $ View () (ContractAddr callback)
          Nothing ->
            printFieldFromStorage @Natural #totalMinted "Total minted: " show
      CmdGetTotalBurned mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            callback <- addrOrAliasToAddr callback'
            runTzbtcContract $
              fromFlatParameter $ GetTotalBurned $ View () (ContractAddr callback)
          Nothing ->
            printFieldFromStorage @Natural #totalBurned "Total burned: " show
      CmdGetAdministrator mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            callback <- addrOrAliasToAddr callback'
            runTzbtcContract $
              fromFlatParameter $ GetAdministrator $ View () (ContractAddr callback)
          Nothing ->
            printFieldFromStorage @Address #admin "Admininstator: " formatAddress
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
      CmdDeployContract admin' redeem' -> do
        [admin, redeem] <- mapM addrOrAliasToAddr [admin', redeem']
        deployTzbtcContract admin redeem
  where
    runMultisigTzbtcContract :: (HasCmdLine m, HasTezosRpc m) => (Maybe FilePath) -> Parameter a -> m ()
    runMultisigTzbtcContract mbMultisig param =
      case mbMultisig of
        Just fp -> case toSafeParam param of
          Just subParam -> createMultisigPackage fp subParam
          _ -> printStringLn "Unable to call multisig for View entrypoints"
        Nothing -> runTzbtcContract param
    printFieldFromStorage
      :: forall t name m. (HasCmdLine m, HasTezosRpc m, HasStoreTemplateField t name)
      => Label name -> Text -> (t -> Text) -> m ()
    printFieldFromStorage _ prefix formatter = do
      mbField <- getFieldFromTzbtcUStore @name @t
      case mbField of
        Just field' -> printTextLn $ prefix <> formatter field'
        Nothing -> printTextLn $ "Field " <>
          symbolValT' @name <> " not found in the contract storage"
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
  :: forall name t m. (HasTezosRpc m, HasStoreTemplateField t name)
  => m (Maybe t)
getFieldFromTzbtcUStore =
  getFromTzbtcUStore $ fieldNameToMText @name

-- | Get value assosiated with given key from given submap of
-- TZBTC contract UStore.
getValueFromTzbtcUStoreSubmap
  :: forall name key value m. (HasTezosRpc m, HasStoreTemplateSubmap key value name)
  => key -> m (Maybe value)
getValueFromTzbtcUStoreSubmap key =
  getFromTzbtcUStore (fieldNameToMText @name , key)

-- | Get value for given key from UStore.
--
-- UStore is basically `BigMap Bytestring ByteString`.
-- So in order to get value from it we pack the key into
-- `ByteString` and perform getting from BigMap using RPC call.
-- After obtaining value we unpack it to the desired type.
getFromTzbtcUStore
  :: forall m key value.
     ( NicePackedValue key
     , NiceUnpackedValue value
     , HasTezosRpc m
     )
  => key -> m (Maybe value)
getFromTzbtcUStore key = do
  ClientConfig{..} <- throwLeft readConfig
  case ccContractAddress of
    Nothing -> throwM TzbtcContractConfigUnavailable
    Just contractAddr -> do
      AlmostStorage{..} <- getTzbtcStorage contractAddr
      let scriptExpr = encodeBase58Check . valueToScriptExpr . lPackValue $ key
      fieldValueRaw <- getFromBigMap asBigMapId scriptExpr
      case fieldValueRaw of
        Left err -> case err of
          TzbtcServantError (FailureResponse _ Response{..}) -> do
            if responseStatusCode == notFound404 then pure Nothing
            else throwM err
          _ -> throwM err
        Right veryRawVal -> do
          rawVal <- throwLeft $ pure $ exprToValue @ByteString veryRawVal
          fmap Just . throwLeft . pure $ lUnpackValue rawVal

originateTzbtcContract
  :: Address -> ClientConfig -> IO (Either TzbtcClientError Address)
originateTzbtcContract admin config@ClientConfig{..} = do
  OperationConstants{..} <- preProcessOperation config
  let origOp = OriginationOperation
        { ooKind = "origination"
        , ooSource = ocSourceAddr
        , ooFee = 50000
        , ooCounter = ocCounter + 1
        , ooGasLimit = 800000
        , ooStorageLimit = 60000
        , ooBalance = 0
        , ooScript =
          mkOriginationScript tzbtcContract (mkEmptyStorageV0 admin)
        }
  let runOp = RunOperation
        { roOperation =
          RunOperationInternal
          { roiBranch = ocLastBlockHash
          , roiContents = [Right origOp]
          , roiSignature = stubSignature
          }
        , roChainId = bcChainId ocBlockConstants
        }
  -- Estimate limits and paid storage diff
  -- We run only one op and expect to have only one result
  [ar1] <- getAppliedResults ocClientEnv (Left runOp)
  -- Update limits and fee
  let updOrigOp = origOp { ooGasLimit = arConsumedGas ar1 + 200
                         , ooStorageLimit = arStorageSize ar1 + 1000
                         , ooFee = calcFees (arConsumedGas ar1) (arPaidStorageDiff ar1)
                         }
  hex <- throwClientError $ getOperationHex ocClientEnv ForgeOperation
    { foBranch = ocLastBlockHash
    , foContents = [Right updOrigOp]
    }
  signRes <- signWithTezosClient' (Left $ addOperationPrefix hex) config
  case signRes of
    Left err -> pure $ Left $ TzbtcOriginationError err
    Right signature' -> do
      let preApplyOp = PreApplyOperation
            { paoProtocol = bcProtocol ocBlockConstants
            , paoBranch = ocLastBlockHash
            , paoContents = [Right updOrigOp]
            , paoSignature = signature'
            }
      -- Perform operations preapplying in order to obtain contract address
      -- We preapply only one op and expect to have only one result
      [ar2] <- getAppliedResults ocClientEnv (Right preApplyOp)
      injectOp (formatByteString hex) signature' config >>=
        (postProcessOperation config)
      case arOriginatedContracts ar2 of
        [contractAddr] -> pure $ Right contractAddr
        _ -> pure $ Left $ TzbtcOriginationError
          "Error during contract origination, expecting to \
          \originate exactly one contract."

deployTzbtcContract' :: Address -> Address -> IO ()
deployTzbtcContract' admin redeem = do
  putTextLn "Originate contract"
  config@ClientConfig{..} <- throwLeft readConfig
  contractAddr <- throwLeft $ originateTzbtcContract admin config
  let migration = migrationScripts (originationParams admin redeem mempty)
      transactionsToTzbtc params = runTransactions' contractAddr params config
  AlmostStorage{..} <- getTzbtcStorage contractAddr
  putTextLn "Upgrade contract to V1"
  transactionsToTzbtc $ (DefaultEntrypoint . fromFlatParameter) <$>
    [ Upgrade ( #newVersion .! (currentVersion asFields + 1)
              , #migrationScript .! manualConcatMigrationScripts migration
              , #newCode .! tzbtcContractRouter
              )
    ]
  putTextLn $ "Contract was successfully deployed. Contract address: " <>
    formatAddress contractAddr
  case ccContractAddress of
    Nothing -> putTextLn "Current contract_address in the config is not set"
    Just curAddr -> putTextLn $ "Current contract_address in the config is " <>
      formatAddress curAddr
  res <- confirmAction "Would you like to replace current contract_address \
                       \with the newly deployed contract?"
  case res of
    Canceled -> pass
    Confirmed -> do
      (curConfig :: ClientConfig) <- throwLeft readConfig
      writeConfig $ curConfig { ccContractAddress = Just contractAddr}
