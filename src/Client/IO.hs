{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# OPTIONS_GHC -Wno-orphans #-}
module Client.IO
  ( HasStoreTemplateField
  , HasStoreTemplateSubmap
  , addrOrAliasToAddr
  , createMultisigPackage
  , deployTzbtcContractV1
  , deployTzbtcContractV2
  , getAllowance
  , getBalance
  , getFieldFromTzbtcUStore
  , getPackageFromFile
  , getTzbtcStorage
  , mkInitEnv
  , runMultisigContract
  , runTzbtcContract
  , runAppM
  , signPackageForConfiguredUser
  , writePackageToFile
  ) where

import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.ByteString.Char8 as BSC (putStrLn)
import qualified Data.Map as Map
import Fmt (pretty)
import Network.HTTP.Types.Status (notFound404)
import Options.Applicative as Opt hiding (value)
import Servant.Client as Servant (ResponseF(..))
import Servant.Client (runClientM)
import Servant.Client.Core as Servant (ClientError(..))
import qualified System.Directory as Directory (doesFileExist)
import qualified System.Environment as SE
import Util.Exception

import Lorentz hiding (address, balance, chainId, cons, map)
import Lorentz.Contracts.Multisig
import Lorentz.UStore
import Lorentz.UStore.Types
import Michelson.Text
import Michelson.Untyped (InternalByteString(..))
import Tezos.Address
import Tezos.Core (parseChainId)
import Tezos.Crypto

import qualified Client.API as API
import Client.Env
import Client.Error
import qualified Client.IO.CmdLine as IO
import qualified Client.IO.HttpClient as IO
import qualified Client.IO.TezosClient as IO
import qualified Client.IO.TezosRpc as IO
import Client.Types
import Client.Util (exprToValue, throwClientError)
import qualified Data.Text.Lazy.IO.Utf8 as Utf8
import Lorentz.Contracts.TZBTC
import Util.AbstractIO
import Util.MultiSig

instance HasFilesystem AppM where
  writeFile fp bs = liftIO $ BS.writeFile fp bs
  writeFileUtf8 fp c = liftIO $ writeFileUtf8 fp c
  readFile fp = liftIO $ BS.readFile fp
  doesFileExist fp = liftIO $ Directory.doesFileExist fp

instance HasFilesystem IO where
  writeFile = BS.writeFile
  writeFileUtf8 fp c = Utf8.writeFile fp (toLText c)
  readFile = BS.readFile
  doesFileExist = Directory.doesFileExist

instance HasCmdLine AppM  where
  parseCmdLine p = liftIO $ execParser p
  printTextLn t = liftIO $ putStrLn t
  printStringLn t = liftIO $ putStrLn t
  printByteString bs = liftIO $ BSC.putStrLn bs
  confirmAction msg = liftIO $ IO.confirmAction msg

instance HasCmdLine IO  where
  parseCmdLine = execParser
  printTextLn = putStrLn
  printStringLn = putStrLn
  printByteString = BSC.putStrLn
  confirmAction = IO.confirmAction

instance (Monad m, MonadThrow m, HasTezosClient m, HasEnv m) => HasConfig m where
  readConfig = do
    c <- getTezosClientConfig
    -- Lookup config overrides from environment
    -- and apply them
    configOverrides <- aeConfigOverride <$> lookupEnv
    case c of
      Right (path, tzc) -> do
        config <- toTzbtcConfig tzc path
        let overridenUserAlias = fromMaybe (ccUserAlias config) (coTzbtcUser configOverrides)
        overridenMultisig <- case coTzbtcMultisig configOverrides of
          Just multisig -> Just <$> addrOrAliasToAddr multisig
          _ -> pure $ ccMultisigAddress config

        overridenContract <- case coTzbtcContract configOverrides of
          Just cntr -> Just <$> addrOrAliasToAddr cntr
          _ -> pure $ ccContractAddress config
        pure $ Right $ config
          { ccUserAlias = overridenUserAlias
          , ccMultisigAddress = overridenMultisig
          , ccContractAddress = overridenContract
          }
      Left _ -> pure $ Left TzbtcClientConfigError

toTzbtcConfig :: (HasTezosClient m) => TezosClientConfig -> FilePath -> m ClientConfig
toTzbtcConfig TezosClientConfig {..} path = do
  contractAddr <- rightToMaybe <$> getAddressForContract "tzbtc"
  multisigAddr <- rightToMaybe <$> getAddressForContract "tzbtc-multisig"
  pure $ ClientConfig
    { ccNodeAddress = tcNodeAddr
    , ccNodePort = tcNodePort
    , ccNodeUseHttps = tcTls
    , ccUserAlias = "tzbtc-user"
    , ccTezosClientExecutable = path
    , ccContractAddress = contractAddr
    , ccMultisigAddress = multisigAddr
    }

instance HasTezosClient AppM where
  getAddressForContract a = do
    tcPath <- lookupTezosClient
    v <- aeVerbose <$> lookupEnv
    liftIO $ IO.getAddressForContract v tcPath a
  getAddressAndPKForAlias a = do
    tcPath <- lookupTezosClient
    v <- aeVerbose <$> lookupEnv
    liftIO $ IO.getAddressAndPKForAlias v tcPath a
  signWithTezosClient a for = do
    tcPath <- lookupTezosClient
    v <- aeVerbose <$> lookupEnv
    liftIO $ IO.signWithTezosClient v tcPath (first InternalByteString a) for
  waitForOperation op = do
    tcPath <- lookupTezosClient
    v <- aeVerbose <$> lookupEnv
    liftIO $ IO.waitForOperationInclusion v tcPath op
  getTezosClientConfig = do
    tcPath <- lookupTezosClient
    v <- aeVerbose <$> lookupEnv
    ec <- liftIO $ IO.getTezosClientConfig v tcPath
    case ec of
      Right config -> pure $ Right (tcPath, config)
      Left err -> pure $ Left err
  rememberContractAs addr alias = do
    tcPath <- lookupTezosClient
    v <- aeVerbose <$> lookupEnv
    liftIO $ IO.rememberContractAs v tcPath addr alias

---

lookupTezosClient :: (MonadThrow m, HasEnv m) => m String
lookupTezosClient = throwLeft $ aeTezosClientPath <$> lookupEnv

instance HasEnv AppM where
  lookupEnv = ask
  withLocal fn act = local fn act

mkInitEnv :: IO AppEnv
mkInitEnv = do
  tzPath <- fromMaybe "tezos-client" <$> SE.lookupEnv "TZBTC_TEZOS_CLIENT"
  pure $ emptyEnv { aeTezosClientPath = Right tzPath }

instance HasTezosRpc AppM where
  runTransaction addr param amt mbFees = do
    config <- throwLeft readConfig
    v <- aeVerbose <$> lookupEnv
    liftIO $ IO.runTransactions v addr param amt config mbFees
  getStorage addr = do
    config <- throwLeft readConfig
    liftIO $ IO.getStorage config addr
  getCounter addr = do
    config <- throwLeft readConfig
    clientEnv <- liftIO $ IO.getClientEnv config
    liftIO $ throwClientError $ runClientM (API.getCounter addr) clientEnv
  getChainId name = do
    config <- throwLeft readConfig
    clientEnv <- liftIO $ IO.getClientEnv config
    chainIdRaw <- liftIO $ throwClientError $ runClientM (API.getChainId name) clientEnv
    case parseChainId chainIdRaw of
      Right x -> pure x
      Left pe -> throwM (TzbtcParseError $ "Error in parsing of chain id:" <> show pe)
  getFromBigMap bid scriptExpr = do
    config@ClientConfig{..}  <- throwLeft $ readConfig
    clientEnv <- liftIO $ IO.getClientEnv config
    r <- liftIO $ runClientM (API.getFromBigMap bid scriptExpr) clientEnv
    case r of
      Left err -> pure $ Left $ TzbtcServantError err
      Right rawVal -> pure $ Right rawVal
  deployTzbtcContractV1 mbFees dp =
    performTzbtcDeployment $ \v config ->
      IO.deployTzbtcContractV1 v config mbFees dp
  deployTzbtcContractV2 mbFees dp =
    performTzbtcDeployment $ \v config ->
      IO.deployTzbtcContractV2 v config mbFees dp
  deployMultisigContract mbFees msigStorage useCustomErrors = do
    v <- aeVerbose <$> lookupEnv
    let (_, (Threshold thresholdValue, Keys keysList)) = msigStorage
    when (thresholdValue == 0) $ throwM TzbtcMultisigZeroThreshold
    when (thresholdValue > fromIntegral (length keysList)) $
      throwM TzbtcMultisigThresholdLargerThanKeys
    let msigToOriginate = if useCustomErrors
          then tzbtcMultisigContract @'CustomErrors
          else tzbtcMultisigContract @'BaseErrors
    config@ClientConfig{..} <- throwLeft readConfig
    msigAddr <- throwLeft $ liftIO $
      IO.originateContract v msigToOriginate msigStorage config mbFees
    putTextLn $ "Multisig contract was successfully deployed. Contract address: " <>
      formatAddress msigAddr
    liftIO $ case ccMultisigAddress of
      Just c -> putTextLn $
        "Current multisig address for alias 'tzbtc-multisig' in tezos-client is " <>
        formatAddress c <> "."
      Nothing -> putTextLn
        "Right now there is no contract aliased as 'tzbtc-multisig' in \
        \tezos-client config."
    res <- confirmAction
      "Would you like to add/replace alias 'tzbtc-multisig' with the newly deployed contract?"
    case res of
      Canceled -> pass
      Confirmed -> rememberContractAs msigAddr "tzbtc-multisig"

addrOrAliasToAddr :: (MonadThrow m, HasTezosClient m) => Text -> m Address
addrOrAliasToAddr addrOrAlias =
  case parseAddress addrOrAlias of
    Right addr -> pure addr
    Left _ -> aliasToAddr addrOrAlias

aliasToAddr :: (MonadThrow m, HasTezosClient m) => Text -> m Address
aliasToAddr alias = do
  a <- getAddressAndPKForAlias alias
  case a of
    Right (addr, _) -> pure addr
    Left _ -> throwLeft $ getAddressForContract alias

runTzbtcContract
  :: forall m.
     (MonadThrow m, HasTezosRpc m)
  => Parameter SomeTZBTCVersion -> m ()
runTzbtcContract param = do
  ClientConfig{..} <- throwLeft readConfig
  case ccContractAddress of
    Nothing -> throwM TzbtcContractConfigUnavailable
    Just contractAddr -> do
      mbFees <- aeFees <$> lookupEnv
      runTransaction contractAddr (DefaultEntrypoint param) 0 mbFees

runMultisigContract :: forall m.(MonadThrow m, HasTezosRpc m) => NonEmpty Package -> m ()
runMultisigContract packages = do
  config <- throwLeft $ readConfig
  package <- throwLeft $ pure $ mergePackages packages
  multisigAddr <- throwLeft $ pure (snd . fst <$> getToSign package)
  (_, (_, (Keys keys'))) <- getMultisigStorage multisigAddr config
  (_, multisigParam) <- throwLeft $ pure $ mkMultiSigParam keys' packages
  mbFees <- aeFees <$> lookupEnv
  runTransaction multisigAddr (Entrypoint "mainParameter" multisigParam) 0 mbFees

getMultisigStorage
  :: (MonadThrow m, HasTezosRpc m) => Address -> ClientConfig -> m MSigStorage
getMultisigStorage addr ClientConfig{..} = do
  mSigStorageRaw <- getStorage $ formatAddress addr
  throwLeft $ pure $ exprToValue @MSigStorage mSigStorageRaw

createMultisigPackage
  :: (MonadThrow m, HasFilesystem m, HasTezosRpc m)
  => FilePath
  -> SafeParameter SomeTZBTCVersion
  -> m ()
createMultisigPackage packagePath parameter = do
  config@ClientConfig{..} <- throwLeft readConfig
  case ccMultisigAddress of
    Nothing -> throwM TzbtcMutlisigConfigUnavailable
    Just msAddr ->  do
      (counter, _) <- getMultisigStorage msAddr config
      case ccContractAddress of
        Nothing -> throwM TzbtcContractConfigUnavailable
        Just (toTAddress -> contractAddr) -> do
          chainId <- getChainId "main"
          let package = mkPackage msAddr chainId counter contractAddr parameter
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
      signRes <- signWithTezosClient (Right $ getBytesToSign pkg) ccUserAlias
      case signRes of
        Left err -> pure $ Left $ toString err
        Right signature' -> pure $ addSignature pkg (pk, TSignature signature')

getBalance :: (HasTezosRpc m) => Address -> m Natural
getBalance addr = do
  mbLedgerValue <- getValueFromTzbtcUStoreSubmap @"ledger" addr
  case mbLedgerValue of
    Nothing -> pure 0
    Just ledgerValue -> pure $ arg #balance . fst $ ledgerValue

getAllowance :: (HasTezosRpc m) => Address -> Address -> m Natural
getAllowance owner spender = do
  mbLedgerValue <- getValueFromTzbtcUStoreSubmap @"ledger" owner
  case mbLedgerValue of
    Nothing -> pure 0
    Just ledgerValue -> let approvals = arg #approvals . snd $ ledgerValue in
      pure $ maybe 0 id $ Map.lookup spender approvals

type HasStoreTemplateField t name =
  ( HasUField name t StoreTemplateV1
  , NiceUnpackedValue t
  )

type HasStoreTemplateSubmap key value name =
  ( HasUStore name key value StoreTemplateV1
  , NicePackedValue key, NiceUnpackedValue value
  )

-- | Get field with given name from TZBTC contract UStore.
getFieldFromTzbtcUStore
  :: forall name t m. (HasTezosRpc m, HasStoreTemplateField t name)
  => m (Maybe t)
getFieldFromTzbtcUStore =
  getFromTzbtcUStore $ mkFieldMarkerUKeyL @UMarkerPlainField (fromLabel @name)

-- | Get value assosiated with given key from given submap of
-- TZBTC contract UStore.
getValueFromTzbtcUStoreSubmap
  :: forall name key value m. (HasTezosRpc m, HasStoreTemplateSubmap key value name)
  => key -> m (Maybe value)
getValueFromTzbtcUStoreSubmap key =
  getFromTzbtcUStore (symbolToMText @name , key)

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
      -- Exact version does not matter for such low-level operation
      AlmostStorage{..} <- getTzbtcStorage @SomeTZBTCVersion contractAddr
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
          fmap Just . throwLeft . pure $ lUnpackValueRaw rawVal

getTzbtcStorage
  :: forall ver m. (Typeable ver, HasTezosRpc m) => Address -> m (AlmostStorage ver)
getTzbtcStorage contractAddr = do
  storageRaw <- getStorage $ formatAddress contractAddr
  throwLeft $ pure $ exprToValue @(AlmostStorage ver) storageRaw

performTzbtcDeployment
  :: (MonadIO m, MonadThrow m, HasTezosClient m, HasCmdLine m)
  => (Bool -> ClientConfig -> IO Address) -> m ()
performTzbtcDeployment deployAction = do
  config@ClientConfig{..} <- throwLeft readConfig
  v <- aeVerbose <$> lookupEnv
  contractAddr <- liftIO $ deployAction v config
  putTextLn $ "Contract was successfully deployed. Contract address: " <> formatAddress contractAddr
  liftIO $ case ccContractAddress of
    Just c -> putTextLn $ "Current contract address for alias 'tzbtc' in the tezos-client config is " <> formatAddress c <> "."
    Nothing -> putTextLn "Right now there is no contract aliased as 'tzbtc' in tezos-client config."
  res <- confirmAction "Would you like to add/replace alias 'tzbtc' with the newly deployed contract?"
  case res of
    Canceled -> pass
    Confirmed -> rememberContractAs contractAddr "tzbtc"
