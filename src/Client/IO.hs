{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# OPTIONS_GHC -Wno-orphans #-}
module Client.IO
  ( HasStoreTemplateField
  , HasStoreTemplateSubmap
  , addrOrAliasToAddr
  , createMultisigPackage
  , deployTzbtcContract
  , getAllowance
  , getBalance
  , getFieldFromTzbtcUStore
  , getPackageFromFile
  , getTzbtcStorage
  , runConfigEdit
  , runMultisigContract
  , runTzbtcContract
  , setupClient
  , signPackageForConfiguredUser
  , writePackageToFile
  ) where

import qualified Data.Aeson as Aeson (FromJSON, ToJSON, decodeFileStrict)
import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.ByteString.Char8 as BSC (putStrLn)
import qualified Data.Map as Map
import Fmt (pretty)
import Network.HTTP.Types.Status (notFound404)
import Options.Applicative as Opt hiding (value)
import Servant.Client as Servant (ResponseF(..))
import Servant.Client (runClientM)
import Servant.Client.Core as Servant (ClientError(..))
import qualified System.Directory as Directory (createDirectoryIfMissing, doesFileExist)

import Lorentz hiding (address, balance, chainId, cons, map)
import Lorentz.Contracts.ManagedLedger.Types
import Lorentz.UStore.Common
import Michelson.Untyped (InternalByteString(..))
import Tezos.Address
import Tezos.Crypto

import qualified Client.API as API
import Client.Error
import qualified Client.IO.CmdLine as IO
import qualified Client.IO.Config as IO
import qualified Client.IO.HttpClient as IO
import qualified Client.IO.TezosClient as IO
import qualified Client.IO.TezosRpc as IO
import Client.Types
import Client.Util
import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.MultiSig as MSig
import Util.AbstractIO
import Util.Editor
import qualified Util.IO as UIO (writeFileUtf8)
import Util.MultiSig

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
  confirmAction = IO.confirmAction

instance HasConfig IO where
  getConfigPaths = IO.getConfigPaths
  readConfigText = readConfig_
  readConfig = readConfig_
  writeConfigFull = writeConfig_
  writeConfigPartial = writeConfig_
  writeConfigText = writeConfig_

readConfig_ :: (Aeson.FromJSON a) => IO (Either TzbtcClientError a)
readConfig_ = do
  p <- snd <$> IO.getConfigPaths
  IO.readConfig p

writeConfig_ :: (Aeson.ToJSON a) => a -> IO ()
writeConfig_ config = do
  p <- snd <$> IO.getConfigPaths
  IO.writeConfig p config

instance HasTezosClient IO where
  getAddressAndPKForAlias a = do
    config <- throwLeft $ readConfig
    IO.getAddressAndPKForAlias a config
  signWithTezosClient a = do
    config <- throwLeft $ readConfig
    IO.signWithTezosClient (first InternalByteString a) config
  waitForOperation op = do
    config@ClientConfig{..}  <- throwLeft $ readConfig
    IO.waitForOperationInclusion op config

instance HasTezosRpc IO where
  runTransactions addr param = do
    config <- throwLeft readConfig
    IO.runTransactions addr param config
  getStorage addr = do
    config <- throwLeft readConfig
    IO.getStorage config addr
  getCounter addr = do
    config <- throwLeft readConfig
    clientEnv <- IO.getClientEnv config
    throwClientError $ runClientM (API.getCounter addr) clientEnv
  getFromBigMap bid scriptExpr = do
    config@ClientConfig{..}  <- throwLeft $ readConfig
    clientEnv <- IO.getClientEnv config
    r <- runClientM (API.getFromBigMap bid scriptExpr) clientEnv
    case r of
      Left err -> pure $ Left $ TzbtcServantError err
      Right rawVal -> pure $ Right rawVal
  deployTzbtcContract address redeem = do
    config@ClientConfig{..} <- throwLeft readConfig
    contractAddr <- IO.deployTzbtcContract config address redeem
    putTextLn $ "Contract was successfully deployed. Contract address: " <>
      formatAddress contractAddr
    case ccContractAddress of
      Nothing -> putTextLn "Current contract_address in the config is not set"
      Just curAddr ->
        putTextLn $ "Current contract_address in the config is " <> formatAddress curAddr
    res <- confirmAction "Would you like to replace current contract_address \
                         \with the newly deployed contract?"
    case res of
      Canceled -> pass
      Confirmed -> do
        (curConfig :: ClientConfig) <- throwLeft readConfig
        writeConfigFull $ curConfig { ccContractAddress = Just contractAddr}

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
    withDefaultConfig a Unavailable = a

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

runTzbtcContract :: (MonadThrow m, HasTezosRpc m) => Parameter i s -> m ()
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

createMultisigPackage :: (MonadThrow m, HasConfig m, HasTezosRpc m) => FilePath -> SafeParameter i s -> m ()
createMultisigPackage packagePath parameter = do
  config@ClientConfig{..} <- throwLeft readConfig
  case ccMultisigAddress of
    Nothing -> throwM TzbtcMutlisigConfigUnavailable
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

signPackageForConfiguredUser
  :: (MonadThrow m, HasCmdLine m, HasTezosClient m)
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

getTzbtcStorage
  :: (HasTezosRpc m) => Address -> m (AlmostStorage Interface StoreTemplate)
getTzbtcStorage contractAddr = do
  storageRaw <- getStorage $ formatAddress contractAddr
  throwLeft $ pure $ exprToValue @(AlmostStorage Interface StoreTemplate) storageRaw
