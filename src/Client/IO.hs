{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# OPTIONS_GHC -Wno-orphans #-}
module Client.IO
  ( HasStoreTemplateField
  , HasStoreTemplateSubmap
  , addressOrAliasToAddr
  , createMultisigPackage
  , deployMultisigContract
  , deployTzbtcContractV1
  , deployTzbtcContractV2
  , getAllowance
  , getBalance
  , getFieldFromTzbtcUStore
  , getPackageFromFile
  , getTzbtcStorage
  , getTzbtcUserAddress
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
import qualified System.Directory as Directory (doesFileExist)

import Lorentz hiding (address, balance, chainId, cons, map)
import Lorentz.Contracts.Multisig
import Lorentz.UStore
import Lorentz.UStore.Types
import Michelson.Text
import Michelson.Typed (UnpackedValScope)
import Michelson.Untyped (EpName(..))
import Morley.Client.Action (lTransfer, lOriginateContract)
import Morley.Client.Logging (WithClientLog)
import Morley.Client.RPC.Class hiding (getBalance)
import Morley.Client.RPC.Getters (BigMapId(..), readBigMapValueMaybe)
import Morley.Client.TezosClient.Class
import Morley.Client.TezosClient.Types
  (AddressOrAlias(..), Alias(..), AliasHint(..))
import Morley.Micheline
import Tezos.Address
import Tezos.Core (unsafeMkMutez)
import Util.ByteString (HexJSONByteString(..))
import Util.Exception (throwLeft)

import Client.Env
import Client.Error
import qualified Client.IO.CmdLine as IO
import Client.Types
import qualified Data.Text.Lazy.IO.Utf8 as Utf8
import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.Preprocess
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
  printTextLn t = liftIO $ putStrLn t
  printStringLn t = liftIO $ putStrLn t
  printByteString bs = liftIO $ BSC.putStrLn bs
  confirmAction msg = liftIO $ IO.confirmAction msg

instance HasCmdLine IO  where
  printTextLn = putStrLn
  printStringLn = putStrLn
  printByteString = BSC.putStrLn
  confirmAction = IO.confirmAction

instance HasEnv AppM where
  lookupEnv = tzbtcEnv <$> ask
  withLocal fn act = local (\env -> env { tzbtcEnv = fn (tzbtcEnv env) }) act

addressOrAliasToAddr :: (MonadThrow m, HasTezosClient m) => AddressOrAlias -> m Address
addressOrAliasToAddr addressOrAlias = do
  mbAddr <- resolveAddressMaybe addressOrAlias
  case mbAddr of
    Just addr -> pure addr
    Nothing -> throwM $ TzbtcUnknownAliasError $
      "failed to resolve address: " <> pretty addressOrAlias

getTzbtcSpecificAddress
  :: (MonadThrow m, HasTezosClient m, HasEnv m)
  => Text -> (ConfigOverride -> Maybe AddressOrAlias) -> m Address
getTzbtcSpecificAddress defaultAlias getFromOverride = do
  tzbtcOverrides <- aeConfigOverride <$> lookupEnv
  let alias = fromMaybe (AddressAlias $ Alias defaultAlias) $
        getFromOverride tzbtcOverrides
  addressOrAliasToAddr alias

getTzbtcUserAddress
  :: (MonadThrow m, HasTezosClient m, HasEnv m)
  => m Address
getTzbtcUserAddress = getTzbtcSpecificAddress "tzbtc-user" coTzbtcUser

getTzbtcContractAddress
  :: (MonadThrow m, HasTezosClient m, HasEnv m)
  => m Address
getTzbtcContractAddress = getTzbtcSpecificAddress "tzbtc" coTzbtcContract

getMultisigAddress
  :: (MonadThrow m, HasTezosClient m, HasEnv m)
  => m Address
getMultisigAddress = getTzbtcSpecificAddress "tzbtc-multisig" coTzbtcMultisig

runTzbtcContract
  :: forall m env.
     (MonadThrow m, HasTezosRpc m, HasTezosClient m, WithClientLog env m, HasEnv m)
  => Parameter SomeTZBTCVersion -> m ()
runTzbtcContract param = do
  from <- getTzbtcUserAddress
  to <- getTzbtcContractAddress
  mbFee <- aeFees <$> lookupEnv
  void $ lTransfer from to (unsafeMkMutez 0) DefEpName param mbFee

runMultisigContract
  :: forall m env.
     (MonadThrow m, HasTezosRpc m, HasTezosClient m, HasEnv m, WithClientLog env m)
  => NonEmpty Package -> m ()
runMultisigContract packages = do
  from <- getTzbtcUserAddress
  package <- throwLeft $ pure $ mergePackages packages
  multisigAddr <- throwLeft $ pure (snd . fst <$> getToSign package)
  (_, (_, (Keys keys'))) <- getMultisigStorage multisigAddr
  (_, multisigParam) <- throwLeft $ pure $ mkMultiSigParam keys' packages
  mbFees <- aeFees <$> lookupEnv
  void $ lTransfer from multisigAddr (unsafeMkMutez 0) (EpNameUnsafe "mainParameter") multisigParam mbFees

getMultisigStorage
  :: (MonadThrow m, HasTezosRpc m)
  => Address -> m MSigStorage
getMultisigStorage addr = do
  mSigStorageRaw <- getContractStorage addr
  either throwM (pure . fromVal) (fromExpression @(Value (ToT MSigStorage)) mSigStorageRaw)

createMultisigPackage
  :: (MonadThrow m, HasFilesystem m, HasTezosRpc m, HasTezosClient m, HasEnv m)
  => FilePath
  -> SafeParameter SomeTZBTCVersion
  -> m ()
createMultisigPackage packagePath parameter = do
  msAddr <- getMultisigAddress
  tzbtcAddr <- getTzbtcContractAddress
  (counter, _) <- getMultisigStorage msAddr
  chainId <- getChainId
  let package = mkPackage msAddr chainId counter (toTAddress tzbtcAddr) parameter
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
  :: (MonadThrow m, HasCmdLine m, HasTezosClient m, HasEnv m)
  => Package
  -> m (Either String Package)
signPackageForConfiguredUser pkg = do
  userAddr <- getTzbtcUserAddress
  pk <- getPublicKey $ AddressResolved userAddr
  printStringLn "You are going to sign the following package:\n"
  printStringLn $ pretty pkg
  confirmationResult <- confirmAction "Are you sure?"
  case confirmationResult of
    Canceled -> pure $ Left $ "Package signing was canceled"
    Confirmed -> do
      mbPassword <- getKeyPassword (AddressResolved userAddr)
      signature' <- signBytes (AddressResolved userAddr) mbPassword $ unHexJSONByteString $ pkToSign pkg
      pure $ addSignature pkg (pk, TSignature signature')

getBalance
  :: (HasTezosRpc m, HasTezosClient m, HasEnv m)
  => Address -> m Natural
getBalance addr = do
  mbLedgerValue <- getValueFromTzbtcUStoreSubmap @"ledger" addr
  case mbLedgerValue of
    Nothing -> pure 0
    Just ledgerValue -> pure $ arg #balance . fst $ ledgerValue

getAllowance
  :: (HasTezosRpc m, HasTezosClient m, HasEnv m)
  => Address -> Address -> m Natural
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
  :: forall name t m.
     ( HasTezosRpc m, HasTezosClient m, HasEnv m
     , HasStoreTemplateField t name, UnpackedValScope (ToT t)
     )
  => m (Maybe t)
getFieldFromTzbtcUStore =
  getFromTzbtcUStore $ mkFieldMarkerUKeyL @UMarkerPlainField (fromLabel @name)

-- | Get value assosiated with given key from given submap of
-- TZBTC contract UStore.
getValueFromTzbtcUStoreSubmap
  :: forall name key value m.
     ( HasTezosRpc m, HasTezosClient m, HasEnv m
     , HasStoreTemplateSubmap key value name, UnpackedValScope (ToT value)
     )
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
     , UnpackedValScope (ToT value)
     , HasTezosRpc m, HasTezosClient m, HasEnv m
     )
  => key -> m (Maybe value)
getFromTzbtcUStore key = do
  contractAddr <- getTzbtcContractAddress
  -- Exact version does not matter for such low-level operation
  AlmostStorage{..} <- getTzbtcStorage @SomeTZBTCVersion contractAddr
  fieldValueRaw <- readBigMapValueMaybe (BigMapId asBigMapId) (lPackValue key)
  case fieldValueRaw of
    Nothing -> pure Nothing
    Just packedValue -> Just <$> do
      throwLeft $ pure $ lUnpackValue packedValue

getTzbtcStorage
  :: forall ver m. (HasTezosRpc m)
  => Address -> m (AlmostStorage ver)
getTzbtcStorage contractAddr = do
  storageRaw <- getContractStorage contractAddr
  either throwM (pure . fromVal) $
    fromExpression @(Value (ToT (AlmostStorage ver))) storageRaw

performTzbtcDeployment
  :: (MonadThrow m, HasTezosClient m, HasCmdLine m)
  => m Address -> m ()
performTzbtcDeployment deployAction = do
  contractAddr <- deployAction
  printTextLn $ "Contract was successfully deployed. Contract address: " <> formatAddress contractAddr
  let contractAlias = "tzbtc"
  mbTzbtcAddress <- resolveAddressMaybe (AddressAlias $ Alias contractAlias)
  printTextLn $ case mbTzbtcAddress of
    Just c ->
      "Current contract address for alias '" <> contractAlias <>
      "' in the tezos-client config is " <> formatAddress c <> "."
    Nothing ->
      "Right now there is no contract aliased as '" <> contractAlias <>
      "' in tezos-client config."
  res <- confirmAction $
    "Would you like to add/replace alias '" <> contractAlias <>
    "' with the newly deployed contract?"
  case res of
    Canceled -> pass
    Confirmed -> rememberContract True contractAddr $ AliasHint $ contractAlias

originateTzbtcContract
  :: (HasTezosClient m, HasTezosRpc m, HasEnv m, WithClientLog env m)
  => Address -> Address -> m Address
originateTzbtcContract originator owner = do
  mbFee <- aeFees <$> lookupEnv
  snd <$> lOriginateContract False "" (AddressResolved originator) (unsafeMkMutez 0)
    tzbtcContract (mkEmptyStorageV0 owner) mbFee

deployTzbtcContractV1
  :: (HasTezosClient m, HasTezosRpc m, HasEnv m, HasCmdLine m, WithClientLog env m)
  => V1DeployParameters -> m ()
deployTzbtcContractV1 V1DeployParameters{..} = performTzbtcDeployment $ do
  tzbtcUser <- getTzbtcUserAddress
  mbFee <- aeFees <$> lookupEnv
  contractAddr <- originateTzbtcContract tzbtcUser v1Owner
  void $ lTransfer tzbtcUser contractAddr (unsafeMkMutez 0)
    (EpNameUnsafe "upgrade") (upgradeParametersV1 v1MigrationParams) mbFee
  pure contractAddr

deployTzbtcContractV2
  :: (HasTezosClient m, HasTezosRpc m, HasEnv m, HasCmdLine m, WithClientLog env m)
  => V2DeployParameters -> m ()
deployTzbtcContractV2 V2DeployParameters{..} = performTzbtcDeployment $ do
  tzbtcUser <- getTzbtcUserAddress
  mbFee <- aeFees <$> lookupEnv
  contractAddr <- originateTzbtcContract tzbtcUser v2Owner
  void $ lTransfer tzbtcUser contractAddr (unsafeMkMutez 0)
    (EpNameUnsafe "upgrade") (upgradeParametersV2 v2MigrationParams) mbFee
  pure contractAddr

deployMultisigContract
  :: (HasTezosClient m, HasTezosRpc m, HasEnv m, HasCmdLine m, WithClientLog env m)
  => MSigStorage -> Bool -> m ()
deployMultisigContract msigStorage useCustomErrors = do
  let (_, (Threshold thresholdValue, Keys keysList)) = msigStorage
  when (thresholdValue == 0) $ throwM TzbtcMultisigZeroThreshold
  when (thresholdValue > fromIntegral (length keysList)) $
    throwM TzbtcMultisigThresholdLargerThanKeys
  let msigToOriginate = if useCustomErrors
        then tzbtcMultisigContract @'CustomErrors
        else tzbtcMultisigContract @'BaseErrors
  tzbtcUser <- getTzbtcUserAddress
  mbFee <- aeFees <$> lookupEnv
  msigAddr <- snd <$> lOriginateContract False "" (AddressResolved tzbtcUser) (unsafeMkMutez 0)
    msigToOriginate msigStorage mbFee
  printTextLn $ "Contract was successfully deployed. Contract address: " <> formatAddress msigAddr
  let contractAlias = "tzbtc-multisig"
  mbMsigAddress <- resolveAddressMaybe (AddressAlias $ Alias contractAlias)
  printTextLn $ case mbMsigAddress of
    Just c ->
      "Current contract address for alias '" <> contractAlias <>
      "' in the tezos-client config is " <> formatAddress c <> "."
    Nothing ->
      "Right now there is no contract aliased as '" <> contractAlias <>
      "' in tezos-client config."
  res <- confirmAction $
    "Would you like to add/replace alias '" <> contractAlias <>
    "' with the newly deployed contract?"
  case res of
    Canceled -> pass
    Confirmed -> rememberContract True msigAddr $ AliasHint contractAlias
