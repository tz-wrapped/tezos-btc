{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Main
  ( main
  ) where

import Control.Exception.Safe (throwString)
import Data.Aeson (decodeFileStrict, encodeFile)
import Data.Char (isAlpha, isDigit)
import Data.List ((!!))
import Data.Text as T (breakOn, strip, takeWhile)
import Data.Version (showVersion)
import Fmt (pretty)
import Options.Applicative
  (execParser, footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Network.HTTP.Client
  (ManagerSettings(..), Request(..), newManager, defaultManagerSettings)
import Servant.Client
  (BaseUrl(..), ClientError, ClientEnv, Scheme(..), mkClientEnv, runClientM)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Tezos.Json (TezosWord64)

import Lorentz (lcwDumb, parseLorentzValue, printLorentzContract, printLorentzValue)
import Lorentz.Common (showTestScenario)
import Michelson.Runtime.GState (genesisAddress1, genesisAddress2)
import Michelson.Untyped (InternalByteString(..))
import Paths_tzbtc (version)
import Tezos.Address (Address, formatAddress)
import Tezos.Crypto (Signature)
import Util.IO (writeFileUtf8)

import CLI.Parser
import Client.API
import Client.Crypto
import Client.Error
import Client.Parser
import Client.Types
import Client.Util
import Lorentz.Contracts.TZBTC
  (Parameter(..), agentContract, mkStorage, tzbtcCompileWay, tzbtcContract, tzbtcDoc)
import Lorentz.Contracts.TZBTC.Proxy (tzbtcProxyContract)
import Lorentz.Contracts.TZBTC.Test (mkTestScenario)

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
  , toParameters = paramToExpression $ Pause ()
  }

fixRequest :: Request -> IO Request
fixRequest req = return $
  req { requestHeaders = ("Content-Type", "application/json") :
        filter (("Content-Type" /=) . fst) (requestHeaders req)
      }

getAddressCounter :: ClientEnv -> Address -> IO (Either ClientError TezosWord64)
getAddressCounter env address = runClientM (getCounter $ formatAddress address) env

getCosts
  :: ClientEnv -> RunOperation
  -> IO (Either TzbtcClientError (TezosWord64, TezosWord64))
getCosts env preApplyOp = do
  req <- runClientM (runOperation preApplyOp) env
  case req of
    Left err -> return $ Left (TzbtcServantError err)
    Right RunRes{..} -> do
      let (OperationContent (RunMetadata res internalOps)) =
            rrOperationContents !! 0
          internalResults = map unInternalOperation internalOps
      case foldr combineResults res internalResults of
        RunOperationApplied consumedGas storageSize ->
          return $ Right (consumedGas, storageSize)
        RunOperationFailed errors ->
          return $ Left (TzbtcRunFailed errors)

getOperationHex
  :: ClientEnv -> ForgeOperation
  -> IO (Either ClientError InternalByteString)
getOperationHex env forgeOp = runClientM (forgeOperation forgeOp) env

getLastBlockHash :: ClientEnv -> IO (Either ClientError Text)
getLastBlockHash env = runClientM getLastBlock env

injectOp :: Text -> Signature -> IO ()
injectOp opToInject sign = do
  config@ClientConfig{..} <- throwLeft $ readConfigFile
  clientEnv <- getClientEnv config
  opHash <- throwClientError $ runClientM
    (injectOperation (Just "main") $ prepareForInjection opToInject sign) clientEnv
  putStrLn $ "Operation hash: " <> opHash
  pass

readConfigFile :: IO (Either TzbtcClientError ClientConfig)
readConfigFile = do
  homeDir <- getHomeDirectory
  let fullPath = homeDir <> directory
  mbConfig <- decodeFileStrict (fullPath <> configPath)
  case mbConfig of
    Just config -> return $ Right config
    Nothing -> return $ Left TzbtcClientConfigError

getClientEnv :: ClientConfig -> IO ClientEnv
getClientEnv ClientConfig{..} = do
  manager' <- newManager $ defaultManagerSettings { managerModifyRequest = fixRequest }
  let nodeUrl = BaseUrl Http (toString ccNodeAddress) ccNodePort ""
  return $ mkClientEnv manager' nodeUrl

runTransaction :: Parameter -> IO ()
runTransaction param = do
  config@ClientConfig{..} <- throwLeft $ readConfigFile
  clientEnv <- getClientEnv config
  lastBlockHash <- throwClientError $ getLastBlockHash clientEnv
  counter <- throwClientError $ getAddressCounter clientEnv ccUserAddress
  let opToRun = dumbOp
        { toCounter = counter + 1
        , toDestination = ccContractAddress
        , toSource = ccUserAddress
        , toParameters = paramToExpression $ param
        }
  let runOp = RunOperation
        { roBranch = lastBlockHash
        , roContents = [opToRun]
        , roSignature = stubSignature
        }
  (consumedGas, storageSize) <- throwLeft $ getCosts clientEnv runOp
  hex <- throwClientError $ getOperationHex clientEnv ForgeOperation
    { foBranch = lastBlockHash
    , foContents = [opToRun { toGasLimit = consumedGas + 200
                            , toStorageLimit = storageSize + 20
                            , toFee = calcFees consumedGas storageSize
                            }]
    }
  (exitCode, stdout', stderr') <- readProcessWithExitCode ccTezosClientExecutable
    [ "-A", toString ccNodeAddress, "-P", show ccNodePort
    , "sign", "bytes", toString $ "0x03" <> formatByteString hex
    , "for", toString ccUserAlias
    ] ""
  case exitCode of
    ExitSuccess -> do
      -- Tezos signature always has `edsig` prefix and uses base58 alphabet
      let signature' = T.takeWhile isBase58Char . snd . breakOn "edsig" . strip . toText $ stdout'
      injectOp (formatByteString hex) (unsafeParseSignature signature')
    ExitFailure _ -> do
      putStrLn ("Operation signing failed" :: Text)
      putStrLn stderr'
   where
     isBase58Char :: Char -> Bool
     isBase58Char c =
       (isDigit c && c /= '0') || (isAlpha c && c /= 'O' && c /= 'I' && c /= 'l')

directory, configPath :: FilePath
directory = "/.tzbtc/"
configPath = "config.json"

setupClient :: ClientConfig -> IO ()
setupClient config = do
  homeDir <- getHomeDirectory
  let fullPath = homeDir <> directory
  createDirectoryIfMissing True fullPath
  encodeFile (fullPath <> configPath) config

main :: IO ()
main = do
  cmd <- execParser programInfo
  case cmd of
    CmdSetupClient config -> setupClient config
    CmdTransaction arg -> case arg of
      CmdMint mintParams -> runTransaction $ Mint mintParams
      CmdBurn burnParams -> runTransaction $ Burn burnParams
      CmdTransfer transferParams -> runTransaction $ Transfer transferParams
      CmdApprove approveParams -> runTransaction $ Approve approveParams
      CmdGetAllowance getAllowanceParams -> runTransaction $
        GetAllowance getAllowanceParams
      CmdGetBalance getBalanceParams -> runTransaction $ GetBalance getBalanceParams
      CmdAddOperator operatorParams -> runTransaction $ AddOperator operatorParams
      CmdRemoveOperator operatorParams -> runTransaction $
        RemoveOperator operatorParams
      CmdPause -> runTransaction $ Pause ()
      CmdUnpause -> runTransaction $ Unpause ()
      CmdSetRedeemAddress setRedeemAddressParams -> runTransaction $
        SetRedeemAddress setRedeemAddressParams
      CmdTransferOwnership p -> runTransaction $ TransferOwnership p
      CmdAcceptOwnership p -> runTransaction $ AcceptOwnership p
      CmdStartMigrateTo p -> runTransaction $ StartMigrateTo p
      CmdStartMigrateFrom p -> runTransaction $ StartMigrateFrom p
      CmdMigrate p -> runTransaction $ Migrate p
      CmdPrintContract singleLine mbFilePath ->
        maybe putStrLn writeFileUtf8 mbFilePath $
        printLorentzContract singleLine tzbtcCompileWay tzbtcContract
      CmdPrintAgentContract singleLine mbFilePath ->
        maybe putStrLn writeFileUtf8 mbFilePath $
        printLorentzContract singleLine lcwDumb (agentContract @Parameter)
      CmdPrintProxyContract singleLine mbFilePath ->
        maybe putStrLn writeFileUtf8 mbFilePath $
        printLorentzContract singleLine lcwDumb tzbtcProxyContract
      CmdPrintInitialStorage adminAddress redeemAddress ->
        putStrLn $ printLorentzValue True (mkStorage adminAddress redeemAddress mempty mempty)
      CmdPrintDoc mbFilePath ->
        maybe putStrLn writeFileUtf8 mbFilePath $ tzbtcDoc
      CmdParseParameter t ->
        either (throwString . pretty) (putTextLn . pretty) $
        parseLorentzValue @Parameter t
      CmdTestScenario TestScenarioOptions {..} -> do
        maybe (throwString "Not enough addresses")
          (maybe putStrLn writeFileUtf8 tsoOutput) $
          showTestScenario <$> mkTestScenario tsoMaster tsoAddresses
  where
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
