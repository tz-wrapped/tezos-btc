{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.IO
  ( HasStoreTemplateField
  , addrOrAliasToAddr
  , createMultisigPackage
  , deployTzbtcContract
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
import Network.HTTP.Client (ManagerSettings(..), Request(..), defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (notFound404)
import Servant.Client
  (BaseUrl(..), ClientEnv, ClientError(..), ResponseF(..), Scheme(..), mkClientEnv, runClientM)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserConfigFile)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Tezos.Json (TezosWord64(..))

import Lorentz.Constraints (NicePackedValue, NiceUnpackedValue)
import Lorentz.Contracts.ManagedLedger.Types (LedgerValue)
import Lorentz.Pack (lPackValue, lUnpackValue)
import Lorentz.UStore (HasUField, HasUStore)
import Lorentz.UStore.Common (fieldNameToMText)
import Lorentz.UStore.Migration (manualConcatMigrationScripts)
import Michelson.Runtime.GState (genesisAddress1, genesisAddress2)
import Michelson.Untyped (InternalByteString(..))
import Tezos.Address (Address, formatAddress, parseAddress)
import Tezos.Crypto (PublicKey, Signature, encodeBase58Check)
import Util.Named ((.!))

import Client.API
import Client.Crypto
import Client.Error
import Client.Parser
import Client.Types
import Client.Util
import Lorentz.Contracts.TZBTC
  (FlatParameter(..), Interface, Parameter, SafeParameter, StoreTemplate, fromFlatParameter)
import qualified Lorentz.Contracts.TZBTC.MultiSig as MSig
import Lorentz.Contracts.TZBTC.Preprocess (migrationScripts, tzbtcContract, tzbtcContractRouter)
import Lorentz.Contracts.TZBTC.Types (StorageFields(..))
import Lorentz.Contracts.TZBTC.V0 (mkEmptyStorageV0)
import Lorentz.Contracts.TZBTC.V1 (originationParams)
import Util.Editor
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
  , toStorageLimit = 60000
  , toAmount = 0
  , toDestination = genesisAddress2
  , toParameters = ParametersInternal
    { piEntrypoint = "default"
    , piValue = nicePackedValueToExpression $ fromFlatParameter $ Pause ()
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
  config@ClientConfig{..} <- throwLeft readConfigFile
  case ccMultisigAddress of
    Nothing -> throwM TzbtcMutlisigConfigUnavailable
    Just msAddr ->  do
      (counter, _) <- getMultisigStorage msAddr config
      case ccContractAddress of
        Nothing -> throwM TzbtcContractConfigUnavailable
        Just contractAddr ->
          let package = mkPackage msAddr counter contractAddr parameter in
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
  config@ClientConfig{..} <- throwLeft readConfigFile
  case ccContractAddress of
    Nothing -> throwM TzbtcContractConfigUnavailable
    Just contractAddr -> do
      clientEnv <- getClientEnv config
      AlmostStorage{..} <- getTzbtcStorage contractAddr config clientEnv
      let scriptExpr = encodeBase58Check . valueToScriptExpr . lPackValue $ key
      fieldValueRaw <- runClientM (getFromBigMap asBigMapId scriptExpr) clientEnv
      case fieldValueRaw of
        Left err -> case err of
          FailureResponse _ Response{..} -> do
            if responseStatusCode == notFound404 then pure Nothing
            else throwM err
          _ -> throwM err
        Right veryRawVal -> do
          rawVal <- throwLeft $ pure $ exprToValue @ByteString veryRawVal
          fmap Just . throwLeft . pure $ lUnpackValue rawVal

getTzbtcStorage
  :: Address -> ClientConfig -> ClientEnv -> IO (AlmostStorage Interface)
getTzbtcStorage contractAddr ClientConfig{..} clientEnv =do
  storageRaw <- throwClientError $
    runClientM (getStorage $ formatAddress contractAddr) clientEnv
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

getAppliedResults
  :: ClientEnv -> Either RunOperation PreApplyOperation
  -> IO [AppliedResult]
getAppliedResults env op = do
  results <- case op of
    Left runOp ->
      (sequence . (: [])) <$> throwLeft $ runClientM (runOperation runOp) env
    Right preApplyOp ->
      throwLeft $ runClientM (preApplyOperations [preApplyOp]) env
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

getOperationHex
  :: ClientEnv -> ForgeOperation
  -> IO (Either ClientError InternalByteString)
getOperationHex env forgeOp = runClientM (forgeOperation forgeOp) env

getLastBlockHash :: ClientEnv -> IO (Either ClientError Text)
getLastBlockHash env = runClientM getLastBlock env

getCurrentBlockConstants :: ClientEnv -> Text -> IO (Either ClientError BlockConstants)
getCurrentBlockConstants env block = runClientM (getBlockConstants block) env

injectOp :: Text -> Signature -> ClientConfig -> IO Text
injectOp opToInject sign config = do
  clientEnv <- getClientEnv config
  throwClientError $ runClientM
    (injectOperation (Just "main") $ prepareForInjection opToInject sign) clientEnv

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
  ocClientEnv <- getClientEnv config
  ocLastBlockHash <- throwClientError $ getLastBlockHash ocClientEnv
  (ocSourceAddr, _) <- throwLeft $ getAddressAndPKForAlias ccUserAlias config
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

-- It is possible to include multiple transaction within single operations batch
-- so we are accepting list of param's here.
runTransactions
  :: (NicePackedValue param)
  => Address -> [param] -> ClientConfig -> IO ()
runTransactions to params config@ClientConfig{..} = do
  OperationConstants{..} <- preProcessOperation config
  let opsToRun = map (\(param, i) -> dumbOp
        { toDestination = to
        , toSource = ocSourceAddr
        , toCounter = ocCounter + (fromInteger i)
        , toParameters = ParametersInternal
          { piEntrypoint = "default"
          , piValue = nicePackedValueToExpression $ param
          }
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
  -- Forge operation with given limits and get its hexadecimal representation
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
  signRes <- signWithTezosClient (Left $ addOperationPrefix hex) config
  case signRes of
    Left err -> putStrLn err
    Right signature' -> do
      -- Sign and inject the operation
      injectOp (formatByteString hex) signature' config >>=
        (postProcessOperation config)

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
  signRes <- signWithTezosClient (Left $ addOperationPrefix hex) config
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

deployTzbtcContract :: Address -> Address -> IO ()
deployTzbtcContract admin redeem = do
  putTextLn "Originate contract"
  config@ClientConfig{..} <- throwLeft readConfigFile
  contractAddr <- throwLeft $ originateTzbtcContract admin config
  let migration = migrationScripts (originationParams admin redeem mempty)
      transactionsToTzbtc params = runTransactions contractAddr params config
  clientEnv <- getClientEnv config
  AlmostStorage{..} <- getTzbtcStorage contractAddr config clientEnv
  putTextLn "Upgrade contract to V1"
  transactionsToTzbtc $ fromFlatParameter <$>
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
      (curConfig :: ClientConfig) <- throwLeft readConfigFile
      writeConfig $ curConfig { ccContractAddress = Just contractAddr}

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
     [ "sign", "bytes", toString toSign
     , "for", toString ccUserAlias
     ]) ""
  case exitCode of
    ExitSuccess -> do
      Right <$> (throwLeft $ pure $ parseSignatureFromOutput (fromString stdout'))
    ExitFailure _ -> return . Left . fromString $
      "Operation signing failed: " <> stderr'

data ConfirmationResult = Confirmed | Canceled

confirmAction :: Text -> IO ConfirmationResult
confirmAction msg = do
  putTextLn $ msg <> " [Y/N]"
  res <- getLine
  case res of
    x | x `elem` ["Y", "y", "yes"] -> pure Confirmed
    x | x `elem` ["N", "n", "no"] -> pure Canceled
    _ -> confirmAction msg

signPackageForConfiguredUser :: Package -> IO (Either String Package)
signPackageForConfiguredUser pkg = do
  config@ClientConfig{..} <- throwLeft readConfigFile
  (_, pk) <- throwLeft $ getAddressAndPKForAlias ccUserAlias config
  putTextLn "You are going to sign the following package:\n"
  putTextLn $ pretty pkg
  confirmationResult <- confirmAction "Are you sure?"
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
      config <- throwLeft readConfigFile
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
  config@ClientConfig{..} <- throwLeft readConfigFile
  case ccContractAddress of
    Nothing -> throwM TzbtcContractConfigUnavailable
    Just contractAddr ->
      runTransactions contractAddr [param] config

runMultisigContract :: NonEmpty Package -> IO ()
runMultisigContract packages = do
  config <- throwLeft readConfigFile
  package <- throwLeft $ pure $ mergePackages packages
  multisigAddr <- throwLeft $ pure (fst <$> getToSign package)
  (_, (_, keys')) <- getMultisigStorage multisigAddr config
  (_, multisigParam) <- throwLeft $ pure $ mkMultiSigParam keys' packages
  runTransactions multisigAddr [multisigParam] config

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
  BSL.putStrLn $ BSL.fromStrict fileContents
