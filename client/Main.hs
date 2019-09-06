{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Main
  ( main
  ) where

import Control.Exception.Safe (throwString)
import Data.Aeson (decodeFileStrict, encodeFile)
import Data.List ((!!))
import Data.Version (showVersion)
import Fmt (pretty)
import Options.Applicative
  (execParser, footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Network.HTTP.Client
  (ManagerSettings(..), Request(..), newManager, defaultManagerSettings)
import Servant.Client
  (BaseUrl(..), ClientError, ClientEnv, Scheme(..), mkClientEnv, runClientM)
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
  (Parameter(..), agentContract, mkStorage, tzbtcCompileWay, tzbtcContract)
import Lorentz.Contracts.TZBTC.Test (mkTestScenario)

stubSignature :: Signature
stubSignature = unsafeParseSignature
  "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"

dumbOp :: TransactionOperation
dumbOp = TransactionOperation
  { source = genesisAddress1
  , fee = 50000
  , counter = 0
  , gasLimit = 800000
  , storageLimit = 10000
  , amount = 0
  , destination = genesisAddress2
  , parameters = paramToExpression $ Pause ()
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

injectOp :: ClientEnv -> Text -> IO (Either ClientError Text)
injectOp env opToInject = runClientM (injectOperation (Just "main") opToInject) env

readConfigFromFile :: FilePath -> IO (Either TzbtcClientError ClientConfig)
readConfigFromFile filepath = do
  mbConfig <- decodeFileStrict filepath
  case mbConfig of
    Just config -> return $ Right config
    Nothing -> return $ Left TzbtcClientConfigError

requestSignature :: InternalByteString -> IO Signature
requestSignature bs = do
  putStrLn $ "Please, sign the following operation: " <> "0x03" <> formatByteString bs
  putStrLn ("Paste your signature:" :: Text)
  unsafeParseSignature <$> getLine

runTransaction :: Parameter -> IO ()
runTransaction param = do
  manager' <- newManager $ defaultManagerSettings { managerModifyRequest = fixRequest }
  ClientConfig{..} <- throwLeft $ readConfigFromFile configPath
  let nodeUrl = BaseUrl Http (toString ccNodeAddress) ccNodePort ""
      clientEnv = mkClientEnv manager' nodeUrl
      operation = param
  lastBlockHash <- throwClientError $ getLastBlockHash clientEnv
  counter <- throwClientError $ getAddressCounter clientEnv ccUserAddress
  let opToRun = dumbOp
        { counter = counter + 1
        , destination = ccContractAddress
        , source = ccUserAddress
        , parameters = paramToExpression $ operation
        }
  let runOp = RunOperation
        { roBranch = lastBlockHash
        , roContents = [opToRun]
        , roSignature = stubSignature
        }
  (consumedGas, storageSize) <- throwLeft $ getCosts clientEnv runOp
  hex <- throwClientError $ getOperationHex clientEnv ForgeOperation
    { foBranch = lastBlockHash
    , foContents = [opToRun { toGasLimit = consumedGas + 100
                            , toStorageLimit = storageSize + 20
                            , toFee = calcFees consumedGas storageSize
                            }]
    }
  signature' <- requestSignature hex
  -- let signature' = signOperationHex ccUserSecretKey hex
  opHash <- throwClientError $ injectOp clientEnv $ prepareForInjection hex signature'
  putStrLn $ "Operation hash: " <> opHash

configPath :: FilePath
configPath = "config.json"

setupClient :: ClientConfig -> IO ()
setupClient config = do
  encodeFile configPath config

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
      CmdPrintInitialStorage adminAddress redeemAddress ->
        putStrLn $ printLorentzValue True (mkStorage adminAddress redeemAddress mempty mempty)
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
      , "  This command will return perform transaction that", linebreak
      , "  will mint given amout of token to the given address.", linebreak
      , "  As a result, this command returns operation hash,", linebreak
      , "  which later can be checked in the block explorer"
      ]
