{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Test.IO
  ( test_dryRunFlag
  , test_setupClient
  , test_setupClientTemplate
  , test_setupClientTemplateFull
  , test_createMultisigPackage
  , test_multisigSignPackage
  , test_multisigExecutePackage
  ) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Typeable as Typ (cast)
import Options.Applicative (ParserResult(..), defaultPrefs, execParserPure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Text.Hex (decodeHex)
import Util.Named

import Client.Main (mainProgram)
import Client.Types
import Client.Util
import qualified Lorentz.Contracts.TZBTC as TZBTC
import qualified Lorentz.Contracts.TZBTC.MultiSig as MS
import qualified Lorentz.Contracts.TZBTC.Types as TZBTCTypes
import Michelson.Typed.Haskell.Value (fromVal, toVal)
import TestM
import Tezos.Address
import Tezos.Crypto
import qualified Tezos.Crypto.Ed25519 as Ed25519
import Util.MultiSig

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- Some configuration values to configure the
-- base/default mock behavior.
data MockInput = MockInput
  { miCmdLine :: [String]
  , miConfig :: ClientConfig
  , miConfigPaths :: (DirPath, FilePath)
  }

defaultMockInput = MockInput
  { miCmdLine = []
  , miConfig = ClientConfig
     { ccNodeAddress = "localhost"
     , ccNodePort = 2990
     , ccNodeUseHttps = False
     , ccContractAddress = Just contractAddress
     , ccMultisigAddress = Nothing
     , ccUserAlias = "bob"
     , ccTezosClientExecutable = "tezos-client"
     }
  , miConfigPaths = (DirPath configDir, configPath)
  }

-- | The default mock handlers that indvidual tests could
-- override.
defaultHandlers :: MockInput -> Handlers TestM
defaultHandlers mi = Handlers
  { hWriteFile = \fp bs -> meetExpectation $ WritesFile fp (Just bs)
  , hWriteFileUtf8 = \fp _ -> meetExpectation $ WritesFileUtf8 fp
  , hReadFile  = \_ -> unavailable "readFile"
  , hDoesFileExist  = \_ -> unavailable "doesFileExist"
  , hCreateDirectoryIfMissing = \_ _ -> unavailable "createDirectoryIfMissing"
  , hGetConfigPaths = do
      meetExpectation GetConfigPaths
      pure $ miConfigPaths mi
  , hParseCmdLine = \p -> do
      case execParserPure defaultPrefs p (miCmdLine mi) of
        Success a -> do
          meetExpectation ParseCmdLine
          pure a
        Failure _ -> throwM $ TestError "CMDline parsing failed"
        _ -> throwM $ TestError "Unexpected cmd line autocompletion"
  , hPrintStringLn = \_ -> meetExpectation PrintsMessage
  , hPrintTextLn = \_ -> meetExpectation PrintsMessage
  , hPrintByteString = \bs -> meetExpectation (PrintByteString bs)
  , hConfirmAction = \_ -> unavailable "confirmAction"
  , hRunTransactions = \_ _ -> unavailable "runTransactions"
  , hGetStorage = \_ -> unavailable "getStorage"
  , hGetCounter = \_ -> unavailable "getCounter"
  , hGetFromBigMap = \_ _ -> unavailable "getFromBigMap"
  , hWaitForOperation = \_ -> unavailable "waitForOperation"
  , hDeployTzbtcContract = \_ -> meetExpectation DeployTzbtcContract
  , hGetAddressAndPKForAlias = \_ -> unavailable "getAddressAndPKForAlias"
  , hSignWithTezosClient = \_ -> unavailable "signWithTezosClient"
  , hOpenEditor = \fp _ -> meetExpectation (OpenEditor fp)
  }
  where
    unavailable :: String -> TestM a
    unavailable msg = throwM $ TestError $ "Unexpected method call : " <> msg

-- | Run a test using the given mock handlers in TestM
runMock :: forall a . Handlers TestM -> TestM a -> Assertion
runMock h m = case runReaderT (runStateT m Map.empty) (MyHandlers h) of
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
    Nothing  -> throwM $ TestError $ "Unset expectation:" ++ show s

-- | Check if all the expectation have been met.
checkExpectations :: (MonadThrow m, MonadState ST m) =>  m ()
checkExpectations = do
  m <- get
  let filtered = (Map.filter flFn m)
  if Map.null filtered  then pass else throwM $
    TestError $ "Test expectation was not met" ++ show (Map.assocs filtered)
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
johnAlias = "john"

bobAddressPK = PublicKeyEd25519 . Ed25519.toPublic $ bobSecretKey
bobSecretKey = Ed25519.detSecretKey "bob"

aliceAddressPK = PublicKeyEd25519 . Ed25519.toPublic $ aliceSecretKey
aliceSecretKey = Ed25519.detSecretKey "alice"

contractAddressRaw :: IsString s => s
contractAddressRaw = "KT1HmhmNcZKmm2NsuyahdXAaHQwYfWfdrBxi"
contractAddress = unsafeParseAddress contractAddressRaw

multiSigAddressRaw :: IsString s => s
multiSigAddressRaw = "KT1MLCp7v3NiY9xeLe4XyPoS4AEgfXT7X5PX"
multiSigAddress = unsafeParseAddress multiSigAddressRaw

operatorAddress1Raw :: IsString s => s
operatorAddress1Raw = "tz1cLwfiFZWA4ZgDdxKiMgxACvGZbTJ2tiQQ"
operatorAddress1 = unsafeParseAddress operatorAddress1Raw

configPath = "/home/user/.config/tzbtc/config.json"

configDir = "/home/user/.config/tzbtc"

multiSigFilePath = "/home/user/multisig_package"

sign_ :: Ed25519.SecretKey -> Text -> Signature
sign_ sk bs = case decodeHex (T.drop 2 bs) of
  Just dbs -> SignatureEd25519 $ Ed25519.sign sk dbs
  Nothing -> error "Error with making signatures"

-- Test that no operations are called if the --dry-run flag
-- is provided in cmdline.
test_dryRunFlag :: TestTree
test_dryRunFlag = testGroup "Dry run does not execute any action"
  [ testCase "Handle values correctly with placeholders" $ do
      runMock (defaultHandlers $ defaultMockInput { miCmdLine = ["burn", "--value", "100", "--dry-run"] }) $ do
        addExpectation ParseCmdLine Once
        mainProgram
  ]

-- Test the setupClient command
-- Setup client with out any arguments should not overwrite the
-- existing config file
setupClientTestHandlers = (defaultHandlers (defaultMockInput { miCmdLine = ["setupClient"] }))
  { hPrintStringLn = \x ->
      let
          expectedMessage = "Not overwriting the existing config file at, \n\n" <> configPath <> "\n\nPlease remove the file and try again"
      in if x == expectedMessage
          then meetExpectation PrintsMessage
          else throwM $ TestError "Unexpected message"
  , hGetConfigPaths = do
      meetExpectation GetConfigPaths
      pure (DirPath configDir, configPath)
  , hDoesFileExist = \x -> if x == configPath then do
      meetExpectation ChecksFileExist
      pure True
      else throwM $ TestError "Unexpected file existence check"
  }

test_setupClient :: TestTree
test_setupClient = testGroup "`SetupClient` without arguments does not overwrite existing file"
  [ testCase "Check config file overwrite check" $
    let
      test = do
        addExpectation PrintsMessage Once
        addExpectation GetConfigPaths Once
        addExpectation ParseCmdLine Once
        addExpectation ChecksFileExist Once
        mainProgram
        checkExpectations
    in runMock setupClientTestHandlers test
  ]

-- TestSetup client with out any arguments creates a template file
setupClientWithoutArgsTestHandlers :: Handlers TestM
setupClientWithoutArgsTestHandlers = (defaultHandlers (defaultMockInput { miCmdLine = ["setupClient"] }))
  { hGetConfigPaths = do
      meetExpectation GetConfigPaths
      pure (DirPath configDir, configPath)
  , hCreateDirectoryIfMissing = \b x ->
      if unDirPath x ==  configDir && b
        then meetExpectation CreateDirectory
        else throwM $ TestError "Unexpected directiory creation request"
  , hDoesFileExist = \x -> if x == configPath
    then do
      meetExpectation ChecksFileExist
      pure False
    else throwM $ TestError $ "Unexpected file existence check"
  }

test_setupClientTemplate :: TestTree
test_setupClientTemplate = testGroup "SetupClient without arguments create file with placeholders"
  [ testCase "Check template file" $ do
      expectedConfig <- BSL.readFile "test/resources/default-config.json"
      let
        test = do
          addExpectation ParseCmdLine Once
          addExpectation CreateDirectory Once
          addExpectation ChecksFileExist Once
          addExpectation (WritesFile configPath (Just $ BSL.toStrict expectedConfig)) Once
          addExpectation GetConfigPaths Multiple
          addExpectation PrintsMessage Multiple
          mainProgram
          checkExpectations
      runMock setupClientWithoutArgsTestHandlers test
  ]

-- TestSetup client with required arguments creates a filled template file
setupClientTemplateFullTestHandlers :: Handlers TestM
setupClientTemplateFullTestHandlers =
  (defaultHandlers (defaultMockInput { miCmdLine =  args}))
    { hCreateDirectoryIfMissing = \b x ->
        if unDirPath x ==  configDir && b then meetExpectation CreateDirectory
        else throwM $ TestError "Unexpected directiory creation request"
    , hDoesFileExist = \x -> if x == configPath
        then do
          meetExpectation ChecksFileExist
          pure False
        else throwM $ TestError $ "Unexpected file existence check"
    }
  where
    args =
      [ "setupClient" , "--node-url", "localhost", "--node-port", "2990"
      , "--alias", "bob"
      ]

test_setupClientTemplateFull :: TestTree
test_setupClientTemplateFull = testGroup "SetupClient with arguments create file with placeholders"
  [ testCase "Check file template file" $ do
    expectedConfig <- BSL.readFile "test/resources/config-with-required.json"
    let
      test = do
        addExpectation ParseCmdLine Once
        addExpectation PrintsMessage Multiple
        addExpectation GetConfigPaths Multiple
        addExpectation ChecksFileExist Once
        addExpectation CreateDirectory Once
        addExpectation (WritesFile configPath $ Just $ BSL.toStrict expectedConfig) Once
        mainProgram
        checkExpectations
    runMock setupClientTemplateFullTestHandlers test
  ]

---- Test Creation of multisig package. Checks the following.
---- The command is parsed correctely
---- Checks the package is created with the provided parameter
---- The replay attack counter is correct
---- The multisig address is correct
---- The expected calls are made.
multiSigCreationTestHandlers :: Handlers TestM
multiSigCreationTestHandlers =
  (defaultHandlers (defaultMockInput { miCmdLine =  args}))
    { hDoesFileExist = \fp -> if fp == configPath
        then do
          meetExpectation ChecksFileExist
          return True
        else throwM $ TestError "Unexpected file existence check"
    , hReadFile = \fp -> if fp == configPath then do
          meetExpectation ReadsFile
          pure $ BSL.toStrict $ encode cc
        else throwM $ TestError "Unexpected file read"
    , hRunTransactions = \_ _ -> throwM $ TestError "Unexpected `runTransactions` call"
    , hGetStorage = \x -> if x == multiSigAddressRaw
      then pure $ nicePackedValueToExpression (MS.mkStorage 14 3 [])
      else throwM $ TestError "Unexpected contract address"
    , hWriteFile = \fp bs -> do
        packageOk <- checkPackage bs
        if packageOk then
          meetExpectation (WritesFile fp Nothing)
          else throwM $ TestError "Package check failed"
    }
    where
      args =
        [ "addOperator"
        , "--operator", operatorAddress1Raw
        , "--multisig", multiSigFilePath
        ]
      cc :: ClientConfig
      cc = ClientConfig
        { ccNodeAddress = "localhost"
        , ccNodePort = 2990
        , ccNodeUseHttps = False
        , ccContractAddress = Just contractAddress
        , ccMultisigAddress = Just multiSigAddress
        , ccUserAlias = "bob"
        , ccTezosClientExecutable = "tezos-client"
        }
      checkToSign package = case getToSign package of
        Right (addr, (counter, _)) -> pure $
          ( addr == multiSigAddress &&
            counter == 14 )
        _ -> throwM $ TestError "Getting address and counter from package failed"
      checkPackage bs = case decodePackage bs of
        Right package -> case fetchSrcParam package of
          Right param -> do
            toSignOk <- checkToSign package
            pure $ toSignOk &&
              (param == (TZBTC.fromFlatParameter $ TZBTC.AddOperator
                (#operator .! operatorAddress1)))
          _ -> throwM $ TestError "Fetching parameter failed"
        _ -> throwM $ TestError "Decoding package failed"

test_createMultisigPackage :: TestTree
test_createMultisigPackage = testGroup "Create multisig package"
  [ testCase "Check package creation" $
    let
      test = do
        addExpectation ParseCmdLine Once
        addExpectation ReadsFile Once
        addExpectation ChecksFileExist Once
        addExpectation GetConfigPaths Multiple
        addExpectation (WritesFile multiSigFilePath Nothing) Once
        mainProgram
        checkExpectations
    in runMock multiSigCreationTestHandlers test
  ]

---- Test Signing of multisig package
---- Checks that the `signPackage` command correctly includes the
---- signature returned by the tezos-client.
multisigSigningTestHandlers :: Handlers TestM
multisigSigningTestHandlers =
  (defaultHandlers (defaultMockInput { miCmdLine =  args}))
    { hDoesFileExist = \fp -> if fp == configPath
        then do
          meetExpectation ChecksFileExist
          return True
        else throwM $ TestError "Unexpected file existence check"
    , hConfirmAction = \_ -> do
        meetExpectation GetsUserConfirmation
        pure Confirmed
    , hWriteFile = \fp bs -> do
          checkSignature_ bs
          meetExpectation (WritesFile fp Nothing)
    , hReadFile = \fp -> do
        meetExpectation ReadsFile
        if fp == configPath then pure $ BSL.toStrict $ encode cc
        else if fp == multiSigFilePath then pure $ encodePackage multisigSignPackageTestPackage
        else throwM $ TestError "Unexpected file read"
    , hGetAddressAndPKForAlias = \a -> if a == johnAlias
       then pure $ Right (johnAddress, johnAddressPK)
       else throwM $ TestError "Unexpected alias"
    , hSignWithTezosClient = \_ ->
       pure $ Right multisigSignPackageTestSignature
    }
    where
      args = [ "signPackage" , "--package", multiSigFilePath]
      cc :: ClientConfig
      cc = ClientConfig
        { ccNodeAddress = "localhost"
        , ccNodePort = 2990
        , ccNodeUseHttps = False
        , ccContractAddress = Just contractAddress
        , ccMultisigAddress = Just multiSigAddress
        , ccUserAlias = "john"
        , ccTezosClientExecutable = "tezos-client"
        }
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
  14
  contractAddress
  (TZBTCTypes.AddOperator (#operator .! operatorAddress1))

multisigSignPackageTestSignature :: Signature
multisigSignPackageTestSignature =
  sign_ johnSecretKey $ getBytesToSign multisigSignPackageTestPackage

test_multisigSignPackage :: TestTree
test_multisigSignPackage = testGroup "Sign multisig package"
  [ testCase "Check multisig signing" $
    let
      test = do
        addExpectation ParseCmdLine Once
        addExpectation ChecksFileExist Once
        addExpectation GetConfigPaths Multiple
        addExpectation ReadsFile $ Exact 2
        addExpectation PrintsMessage Multiple
        addExpectation GetsUserConfirmation Once
        addExpectation (WritesFile multiSigFilePath Nothing) Once
        mainProgram
        checkExpectations
    in runMock multisigSigningTestHandlers  test
  ]

---- Test Execution of multisig package
---- Checks that the multisig contract parameter is created correctly
---- from the provided signed packages
multisigExecutionTestHandlers :: Handlers TestM
multisigExecutionTestHandlers =
  (defaultHandlers (defaultMockInput { miCmdLine =  args}))
    { hDoesFileExist = \fp -> if
        fp == configPath then do
          meetExpectation ChecksFileExist
          pure True
        else throwM $ TestError "Unexpected file existence check"
    , hRunTransactions =  \addr params ->
        if addr == multiSigAddress then do
          case params of
            [] -> throwM $ TestError "Unexpected empty parameters"
            [Entrypoint "main" param] -> do
              case Typ.cast (toVal param) of
                Just param' -> case (fromVal param') of
                  (_ :: MS.ParamPayload, sigs) ->
                    if sigs ==
                      -- The order should be same as the one that we
                      -- return from getStorage mock
                      [ Just multisigExecutePackageTestSignatureAlice
                      , Just multisigExecutePackageTestSignatureBob
                      , Just multisigExecutePackageTestSignatureJohn
                      ] then meetExpectation RunsTransaction
                    else throwM $ TestError "Unexpected signature list"
                Nothing -> throwM $ TestError "Decoding parameter failed"
            [Entrypoint x _] -> throwM $ TestError $ "Unexpected entrypoint: " <> toString x
            [DefaultEntrypoint _] -> throwM $ TestError "Unexpected default entrypoint"
            _ -> throwM $ TestError "Unexpected multiple parameters"
        else throwM $ TestError "Unexpected multisig address"
    , hGetStorage = \x -> if x == multiSigAddressRaw
        then pure $ nicePackedValueToExpression (MS.mkStorage 14 3 [aliceAddressPK, bobAddressPK, johnAddressPK])
        else throwM $ TestError "Unexpected contract address"
    , hReadFile = \fp -> do
        case fp of
          "/home/user/multisig_package_bob" -> do
            meetExpectation ReadsFile
            encodePackage <$> addSignature_ multisigSignPackageTestPackage (bobAddressPK, multisigExecutePackageTestSignatureBob)
          "/home/user/multisig_package_alice" -> do
            meetExpectation ReadsFile
            encodePackage <$> addSignature_ multisigSignPackageTestPackage (aliceAddressPK, multisigExecutePackageTestSignatureAlice)
          "/home/user/multisig_package_john" -> do
            meetExpectation ReadsFile
            encodePackage <$> addSignature_ multisigSignPackageTestPackage (johnAddressPK, multisigExecutePackageTestSignatureJohn)
          _ -> if fp == configPath then  do
              meetExpectation ReadsFile
              pure $ BSL.toStrict $ encode cc
            else throwM $ TestError "Unexpected file read"
    }
  where
    args =
      [ "callMultisig"
      , "--package", "/home/user/multisig_package_bob"
      , "--package", "/home/user/multisig_package_alice"
      , "--package", "/home/user/multisig_package_john"
      ]
    cc :: ClientConfig
    cc = ClientConfig
      { ccNodeAddress = "localhost"
      , ccNodePort = 2990
      , ccNodeUseHttps = False
      , ccContractAddress = Just contractAddress
      , ccMultisigAddress = Just multiSigAddress
      , ccUserAlias = "john"
      , ccTezosClientExecutable = "tezos-client"
      }
    addSignature_ package s = case addSignature package s of
      Right x -> pure x
      Left _ -> throwM $ TestError "There was an error signing the package"

multisigExecutePackageTestSignatureJohn :: Signature
multisigExecutePackageTestSignatureJohn =
  sign_ johnSecretKey $ getBytesToSign multisigSignPackageTestPackage

multisigExecutePackageTestSignatureBob :: Signature
multisigExecutePackageTestSignatureBob =
  sign_ bobSecretKey $ getBytesToSign multisigSignPackageTestPackage

multisigExecutePackageTestSignatureAlice :: Signature
multisigExecutePackageTestSignatureAlice =
  sign_ aliceSecretKey $ getBytesToSign multisigSignPackageTestPackage

test_multisigExecutePackage :: TestTree
test_multisigExecutePackage = testGroup "Sign multisig execution"
  [ testCase "Check multisig execution" $
    let
      test = do
        addExpectation ParseCmdLine Once
        addExpectation GetConfigPaths Multiple
        addExpectation ChecksFileExist Once
        addExpectation ReadsFile $ Exact 4
        addExpectation RunsTransaction Once
        mainProgram
        checkExpectations
    in runMock multisigExecutionTestHandlers test
  ]
