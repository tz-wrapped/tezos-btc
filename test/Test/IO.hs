{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.IO
  ( test_createMultisigPackage
  , test_createMultisigPackageWithMSigOverride
  , test_multisigSignPackage
  ) where

import Debug qualified (show)

import Data.Map qualified as Map
import Data.Text qualified as T
import Fmt (pretty)
import Test.HUnit (Assertion, assertFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.Hex (decodeHex)

import Client.Env
import Client.Main (mainProgram)
import Client.Types
import Lorentz (TSignature(..), toTAddress)
import Lorentz.Contracts.Multisig
import Lorentz.Contracts.TZBTC qualified as TZBTC
import Lorentz.Contracts.TZBTC.Types qualified as TZBTCTypes
import Morley.Micheline
import Morley.Michelson.Typed.Haskell.Value (toVal)
import Morley.Tezos.Address
import Morley.Tezos.Address.Alias (AddressOrAlias(..), Alias(..))
import Morley.Tezos.Core (ChainId, dummyChainId)
import Morley.Tezos.Crypto
import Morley.Tezos.Crypto.Ed25519 qualified as Ed25519
import Morley.Util.Named
import TestM
import Util.MultiSig

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

deriving stock instance Eq (TSignature a)

testChainId :: ChainId
testChainId = dummyChainId

-- | The default mock handlers that indvidual tests could
-- override.
defaultHandlers :: Handlers TestM
defaultHandlers = Handlers
  { hWriteFile = \fp bs -> meetExpectation $ WritesFile fp (Just bs)
  , hWriteFileUtf8 = \fp _ -> meetExpectation $ WritesFileUtf8 fp
  , hReadFile  = \_ -> unavailable "readFile"
  , hDoesFileExist  = \_ -> unavailable "doesFileExist"
  , hPrintStringLn = \_ -> meetExpectation PrintsMessage
  , hPrintTextLn = \_ -> meetExpectation PrintsMessage
  , hPrintByteString = \bs -> meetExpectation (PrintByteString bs)
  , hConfirmAction = \_ -> unavailable "confirmAction"

  , hGetHeadBlock = unavailable "getHeadBlock"
  , hGetCounter = \_ -> unavailable "getCounter"
  , hGetBlockHeader = \_ -> unavailable "getBlockHeader"
  , hGetBlockConstants = \_ -> unavailable "getBlockConstants"
  , hGetBlockOperations = \_ -> unavailable "getBlockOperations"
  , hGetProtocolParameters = unavailable "getProtocolParameters"
  , hRunOperation = \_ -> unavailable "runOperation"
  , hPreApplyOperations = \_ -> unavailable "preApplyOperations"
  , hForgeOperation = \_ -> unavailable "forgeOperation"
  , hInjectOperation = \_ -> unavailable "injectOperation"
  , hGetContractScript = \_ -> unavailable "getContractScript"
  , hGetContractStorageAtBlock = \_ _ -> unavailable "getContractStorageAtBlock"
  , hGetContractBigMap = \_ _ -> unavailable "getContractBigMap"
  , hGetBigMapValueAtBlock = \_ _ _ -> unavailable "getBigMapValue"
  , hGetBalance = \_ -> unavailable "getBalance"
  , hRunCode = \_ -> unavailable "runCode"
  , hGetChainId = pure $ testChainId
  , hGetBigMapValuesAtBlock = \_ _ _ _ -> unavailable "hGetBigMapValuesAtBlock"
  , hGetManagerKeyAtBlock = \_ _ -> unavailable "hGetManagerKeyAtBlock"
  , hGetDelegateAtBlock = \_ _ -> unavailable "hGetDelegateAtBlock"

  , hSignBytes = \_ _ _ -> unavailable "signBytes"
  , hGenKey = \_ -> unavailable "genKey"
  , hGenFreshKey = \_ -> unavailable "genFreshKey"
  , hRevealKey = \_ _ -> unavailable "revealKey"
  , hWaitForOperation = \_ -> unavailable "waitForOperation"
  , hRememberContract = \_ c a -> meetExpectation $ RememberContract c a
  , hImportKey = \_ _ _ -> unavailable "importKey"
  , hResolveAddressMaybe = \_ -> unavailable "resolveAddressMaybe"
  , hGetAlias = \_ -> unavailable "getAlias"
  , hGetPublicKey = \_ -> unavailable "getPublicKey"
  , hGetTezosClientConfig = unavailable "getTezosClientConfig"
  , hCalcTransferFee = \_ _ _ _ -> unavailable "calcTransferFee"
  , hCalcOriginationFee = \_ -> unavailable "calcOriginationFee"
  , hGetKeyPassword = \_ -> unavailable "getKeyPassword"
  , hRegisterDelegate = \_ _ -> unavailable "registerDelegate"

  , hLookupEnv = do
      meetExpectation LooksupEnv;
      snd <$> ask
  , hWithLocal = \fn action -> local (second fn) action

  , hLogAction = mempty
  , hGetBlockOperationHashes = \_ -> unavailable "getBlockOperationHashes"
  , hGetSecretKey = \_ -> unavailable "getSecretKey"
  , hGetScriptSizeAtBlock = \_ _ -> unavailable "getScriptSizeAtBlock"
  }
  where
    unavailable :: String -> TestM a
    unavailable msg = throwM $ TestError $ "Unexpected method call : " <> msg

-- | Run a test using the given mock handlers in TestM
runMock :: forall a . Handlers TestM -> AppEnv -> TestM a -> Assertion
runMock h env m = case runReaderT (runStateT m Map.empty) (MyHandlers h, env) of
  Right _ -> pass
  Left e -> assertFailure $ displayException e

-- | Add a test expectation
addExpectation :: (MonadState ST m) => Expectation -> ExpectationCount -> m ()
addExpectation s i = state (\m -> ((), Map.insert s (ExpectationStatus i 0)  m))

-- | Meet a previously set expectation
meetExpectation :: forall m. (MonadThrow m, MonadState ST m) => Expectation -> m ()
meetExpectation s = do
  m <- get
  case Map.lookup s m of
    Just es -> put $ Map.insert s (es { exOccurCount = exOccurCount es + 1 }) m
    Nothing  -> throwM $ TestError $ "Unset expectation:" ++ Debug.show s

-- | Check if all the expectation have been met.
checkExpectations :: (MonadThrow m, MonadState ST m) =>  m ()
checkExpectations = do
  m <- get
  let filtered = (Map.filter flFn m)
  if Map.null filtered  then pass else throwM $
    TestError $ "Test expectation was not met" ++ Debug.show (Map.assocs filtered)
  where
    flFn :: ExpectationStatus -> Bool
    flFn es = case exExpectCount es of
      Multiple ->  exOccurCount es == 0
      Once -> exOccurCount es /= 1
      Exact x -> exOccurCount es /= x

-- Some constants
--
johnAddress = mkKeyAddress johnAddressPK
johnAddressPK = PublicKeyEd25519 . Ed25519.toPublic $ johnSecretKey
johnSecretKey = Ed25519.detSecretKey "john"

contractAddress :: Address
contractAddress = [ta|KT1HmhmNcZKmm2NsuyahdXAaHQwYfWfdrBxi|]

multiSigAddress :: Address
multiSigAddress = [ta|KT1MLCp7v3NiY9xeLe4XyPoS4AEgfXT7X5PX|]

multiSigOverrideAddress :: Address
multiSigOverrideAddress = [ta|KT1MwaBC3G3cUa3PfjJ1StFkSuBLbRuoReRK|]

operatorAddress1 :: Address
operatorAddress1 = [ta|tz1cLwfiFZWA4ZgDdxKiMgxACvGZbTJ2tiQQ|]

multiSigFilePath = "/home/user/multisig_package"

sign_ :: Ed25519.SecretKey -> Text -> Sign
sign_ sk bs = case decodeHex (T.drop 2 bs) of
  Just dbs -> TSignature . SignatureEd25519 $ Ed25519.sign sk dbs
  Nothing -> error "Error with making signatures"

---- Test Creation of multisig package. Checks the following.
---- Checks the package is created with the provided parameter
---- The replay attack counter is correct
---- The multisig address is correct
---- The expected calls are made.
multiSigCreationTestHandlers :: Handlers TestM
multiSigCreationTestHandlers =
  defaultHandlers
    { hReadFile = \_ -> throwM $ TestError "Unexpected file read"
    , hInjectOperation = \_ -> throwM $ TestError "Unexpected `injectOperation` call"
    , hGetTezosClientConfig = throwM $ TestError "Unexpected tezos-client get config call"
    , hResolveAddressMaybe = \case
        AddressResolved addr -> pure $ Just addr
        AddressAlias a -> case a of
          "tzbtc" -> pure $ Just contractAddress
          "tzbtc-multisig" -> pure $ Just multiSigAddress
          _ -> pure $ Nothing
    , hGetContractStorageAtBlock = \_ x -> if x == multiSigAddress
      then pure $ toExpression . toVal $ mkStorage 14 3 []
      else throwM $ TestError "Unexpected contract address"
    , hWriteFile = \fp bs -> do
        packageOk <- checkPackage bs
        if packageOk then
          meetExpectation (WritesFile fp Nothing)
          else throwM $ TestError "Package check failed"
    }
    where

      checkToSign package = case getToSign package of
        Right ((chainId, addr), (Counter counter, _)) -> pure $
          ( addr == multiSigAddress && chainId == testChainId &&
            counter == 14 )
        _ -> throwM $ TestError "Getting address and counter from package failed"
      checkPackage bs = case decodePackage bs of
        Right package -> case fetchSrcParam package of
          Right param -> do
            toSignOk <- checkToSign package
            pure $ toSignOk &&
              (param == (TZBTC.fromFlatParameter $ TZBTC.AddOperator
                (#operator :! operatorAddress1)))
          _ -> throwM $ TestError "Fetching parameter failed"
        _ -> throwM $ TestError "Decoding package failed"

test_createMultisigPackage :: TestTree
test_createMultisigPackage = testGroup "Create multisig package"
  [ testCase "Check package creation" $
    let
      test = do
        -- addExpectation ParseCmdLine Once
        addExpectation (WritesFile multiSigFilePath Nothing) Once
        addExpectation LooksupEnv Multiple
        (mainProgram mockArgs)
        checkExpectations
    in runMock multiSigCreationTestHandlers emptyEnv test
  ]
  where
    mockArgs = CmdAddOperator (AddressResolved operatorAddress1) (Just multiSigFilePath)

-- Test multisig contract address override
multiSigCreationWithMSigOverrideTestHandlers :: Handlers TestM
multiSigCreationWithMSigOverrideTestHandlers =
  defaultHandlers
    { hReadFile = \_ -> throwM $ TestError "Unexpected file read"
    , hInjectOperation = \_ -> throwM $ TestError "Unexpected `injectOperation` call"
    , hGetTezosClientConfig = throwM $ TestError "Unexpected tezos-client get config call"
    , hResolveAddressMaybe = \case
        AddressResolved addr -> pure $ Just addr
        AddressAlias a -> case a of
          "tzbtc" -> pure $ Just contractAddress
          "tzbtc-multisig-override" -> pure $ Just multiSigOverrideAddress
          _ -> pure $ Nothing
    , hGetContractStorageAtBlock = \_ x -> if x == multiSigOverrideAddress
      then pure $ toExpression . toVal $ mkStorage 14 3 []
      else throwM $ TestError "Unexpected contract address"
    , hWriteFile = \fp bs -> do
        packageOk <- checkPackage bs
        if packageOk then
          meetExpectation (WritesFile fp Nothing)
          else throwM $ TestError "Package check failed"
    }
    where

      checkToSign package = case getToSign package of
        Right ((chainId, addr), (Counter counter, _)) -> pure $
          ( addr == multiSigOverrideAddress && chainId == testChainId &&
            counter == 14 )
        _ -> throwM $ TestError "Getting address and counter from package failed"
      checkPackage bs = case decodePackage bs of
        Right package -> case fetchSrcParam package of
          Right param -> do
            toSignOk <- checkToSign package
            pure $ toSignOk &&
              (param == (TZBTC.fromFlatParameter $ TZBTC.AddOperator
                (#operator :! operatorAddress1)))
          _ -> throwM $ TestError "Fetching parameter failed"
        _ -> throwM $ TestError "Decoding package failed"

test_createMultisigPackageWithMSigOverride :: TestTree
test_createMultisigPackageWithMSigOverride = testGroup "Create multisig package with multisig override"
  [ testCase "Check package creation with override" $
    let
      test = do
        addExpectation (WritesFile multiSigFilePath Nothing) Once
        addExpectation LooksupEnv Multiple
        mainProgram mockArgs
        checkExpectations
    in runMock multiSigCreationWithMSigOverrideTestHandlers envWithOverride test
  ]
  where
    mockArgs = CmdAddOperator (AddressResolved operatorAddress1) (Just multiSigFilePath)
    envWithOverride = emptyEnv { aeConfigOverride = emptyConfigOverride
                                 { coTzbtcMultisig = Just $ AddressAlias "tzbtc-multisig-override" }
                               }

---- Test Signing of multisig package
---- Checks that the `signPackage` command correctly includes the
---- signature returned by the tezos-client.
multisigSigningTestHandlers :: Handlers TestM
multisigSigningTestHandlers =
  defaultHandlers
    { hConfirmAction = \_ -> do
        meetExpectation GetsUserConfirmation
        pure Confirmed
    , hWriteFile = \fp bs -> do
          checkSignature_ bs
          meetExpectation (WritesFile fp Nothing)
    , hReadFile = \fp -> do
        meetExpectation ReadsFile
        if fp == multiSigFilePath then pure $ encodePackage multisigSignPackageTestPackage
        else throwM $ TestError "Unexpected file read"
    , hGetPublicKey = \case
        AddressResolved addr -> if addr == johnAddress then pure $ johnAddressPK else
          throwM $ TestError ("Unexpected address " ++ pretty addr)
        AddressAlias "tzbtc-user" -> pure johnAddressPK
        AddressAlias alias -> throwM $
          TestError ("Unexpected alias " ++ toString (unAlias alias))
    , hSignBytes = \_ _ _ ->
       pure $ unTSignature multisigSignPackageTestSignature
    , hResolveAddressMaybe = \case
        AddressResolved addr -> pure $ Just addr
        AddressAlias a -> case a of
          "tzbtc" -> pure $ Just contractAddress
          "tzbtc-multisig" -> pure $ Just multiSigAddress
          "tzbtc-user" -> pure $ Just johnAddress
          _ -> pure $ Nothing
    , hGetKeyPassword = \_ -> pure Nothing
    }
    where
      checkSignature_ bs = case decodePackage bs of
        Right package -> case pkSignatures package of
          ((pk, sig):_) -> if pk == johnAddressPK && sig == multisigSignPackageTestSignature
            then pass
            else throwM $ TestError "Bad signature found in package"
          _ -> throwM $ TestError "Unexpected package signatures"
        _ -> throwM $ TestError "Decoding package failed"

multisigSignPackageTestPackage :: Package
multisigSignPackageTestPackage = mkPackage
  multiSigAddress
  testChainId
  14
  (toTAddress @(TZBTC.Parameter TZBTC.SomeTZBTCVersion) contractAddress)
  (TZBTCTypes.AddOperator (#operator :! operatorAddress1))

multisigSignPackageTestSignature :: Sign
multisigSignPackageTestSignature =
  sign_ johnSecretKey $ getBytesToSign multisigSignPackageTestPackage

test_multisigSignPackage :: TestTree
test_multisigSignPackage = testGroup "Sign multisig package"
  [ testCase "Check multisig signing" $
    let
      test = do
        addExpectation ReadsFile $ Exact 1
        addExpectation PrintsMessage Multiple
        addExpectation LooksupEnv Multiple
        addExpectation GetsUserConfirmation Once
        addExpectation (WritesFile multiSigFilePath Nothing) Once
        mainProgram mockArgs
        checkExpectations
    in runMock multisigSigningTestHandlers emptyEnv test
  ]
  where
    mockArgs = CmdSignPackage multiSigFilePath
