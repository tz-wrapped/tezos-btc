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
  , runMultisigContract
  , runTzbtcContract
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

import Lorentz hiding (address, balance, chainId, cons, map)
import Lorentz.Contracts.ManagedLedger.Types
import Lorentz.UStore.Common
import Michelson.Untyped (InternalByteString(..))
import Tezos.Address
import Tezos.Crypto

import qualified Client.API as API
import Client.Error
import qualified Client.IO.CmdLine as IO
import qualified Client.IO.HttpClient as IO
import qualified Client.IO.TezosClient as IO
import qualified Client.IO.TezosRpc as IO
import Client.Types
import Client.Util
import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.MultiSig as MSig
import Util.AbstractIO
import qualified Util.IO as UIO (writeFileUtf8)
import Util.MultiSig

instance HasFilesystem IO where
  writeFile = BS.writeFile
  writeFileUtf8 = UIO.writeFileUtf8
  readFile = BS.readFile
  doesFileExist = Directory.doesFileExist

instance HasCmdLine IO  where
  parseCmdLine = execParser
  printTextLn = putStrLn
  printStringLn = putStrLn
  printByteString = BSC.putStrLn
  confirmAction = IO.confirmAction

instance (Monad m, HasTezosClient m) => HasConfig m where
  readConfig = do
    c <- getTezosClientConfig
    case c of
      Right (path, tzc) -> Right <$> toTzbtcConfig tzc path
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

instance HasTezosClient IO where
  getAddressForContract a = do
    tcPath <- lookupTezosClient
    IO.getAddressForContract tcPath a
  getAddressAndPKForAlias a = do
    tcPath <- lookupTezosClient
    IO.getAddressAndPKForAlias tcPath a
  signWithTezosClient a for = do
    tcPath <- lookupTezosClient
    IO.signWithTezosClient tcPath (first InternalByteString a) for
  waitForOperation op = do
    tcPath <- lookupTezosClient
    IO.waitForOperationInclusion tcPath op
  getTezosClientConfig = do
    tcPath <- lookupTezosClient
    ec <- IO.getTezosClientConfig tcPath
    case ec of
      Right config -> pure $ Right (tcPath, config)
      Left err -> pure $ Left err
  rememberContractAs addr alias = do
    tcPath <- lookupTezosClient
    IO.rememberContractAs tcPath addr alias

lookupTezosClient :: (HasEnv m) => m String
lookupTezosClient = fromMaybe "tezos-client" <$> lookupEnv "TZBTC_TEZOS_CLIENT"

instance HasEnv IO where
  lookupEnv = SE.lookupEnv

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
  deployTzbtcContract op = do
    config@ClientConfig{..} <- throwLeft readConfig
    contractAddr <- IO.deployTzbtcContract config op
    putTextLn $ "Contract was successfully deployed. Contract address: " <> formatAddress contractAddr
    case ccContractAddress of
      Just c -> putTextLn $ "Current contract address for alias 'tzbtc' in the tezos-client config is " <> formatAddress c <> "."
      Nothing -> putTextLn $ "Right now there is no contract aliased as 'tzbtc' in tezos-client config."
    res <- confirmAction "Would you like to add/replace alias 'tzbtc' with the newly deployed contract?"
    case res of
      Canceled -> pass
      Confirmed -> rememberContractAs contractAddr "tzbtc"

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

createMultisigPackage
  :: (MonadThrow m, HasFilesystem m, HasTezosRpc m)
  => FilePath
  -> SafeParameter i s
  -> m ()
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
      signRes <- signWithTezosClient (Right $ getBytesToSign pkg) ccUserAlias
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
