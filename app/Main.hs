{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Main
  ( main
  ) where

import Control.Exception.Safe (throwString)
import Control.Lens (ix)
import Data.Version (showVersion)
import Fmt (pretty)
import Options.Applicative
  (execParser, footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)

import Lorentz (Address, parseLorentzValue, printLorentzContract, printLorentzValue, lcwDumb)
import Lorentz.Common (TestScenario, showTestScenario)
import Util.Named ((.!))
import Util.IO (writeFileUtf8)
import Paths_tzbtc (version)

import CLI.Parser
import Lorentz.Contracts.TZBTC
  (Parameter(..), agentContract, mkStorage, tzbtcCompileWay, tzbtcContract)

import Michelson.Typed (HasNoOp, IsoValue, ToT)
import Lorentz (NoOperation, View(..), NoBigMap, CanHaveBigMap, ContractAddr(..), Contract)
import Tezos.Address
import System.Process.Typed
import qualified Data.ByteString.Lazy as BSL
import System.IO.Temp
import qualified Data.Text.Lazy as T
import qualified Data.Text as ST
import Data.Singletons (SingI(..))
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Attoparsec.ByteString.Char8
import Lorentz.Test.Integrational
import qualified Lorentz.Contracts.TZBTC.Agent as Agent

-- Here in main function we will just accept commands from user
-- and print the smart contract parameter by using `printLorentzValue`
-- function from Lorentz
main :: IO ()
main = do
  cmd <- execParser programInfo
  case cmd of
    CmdMint mintParams -> printParam (Mint mintParams)
    CmdMintForMigrations params -> printParam (MintForMigration params)
    CmdBurn burnParams -> printParam (Burn burnParams)
    CmdTransfer transferParams -> printParam (Transfer transferParams)
    CmdApprove approveParams -> printParam (Approve approveParams)
    CmdGetAllowance getAllowanceParams ->
      printParam (GetAllowance getAllowanceParams)
    CmdGetBalance getBalanceParams -> printParam (GetBalance getBalanceParams)
    CmdAddOperator operatorParams -> printParam (AddOperator operatorParams)
    CmdRemoveOperator operatorParams -> printParam (RemoveOperator operatorParams)
    CmdPause -> printParam $ Pause ()
    CmdUnpause -> printParam $ Unpause ()
    CmdSetRedeemAddress setRedeemAddressParams ->
      printParam (SetRedeemAddress setRedeemAddressParams)
    CmdTransferOwnership p -> printParam (TransferOwnership p)
    CmdAcceptOwnership p -> printParam (AcceptOwnership p)
    CmdStartMigrateTo p -> printParam (StartMigrateTo p)
    CmdStartMigrateFrom p -> printParam (StartMigrateFrom p)
    CmdMigrate p -> printParam (Migrate p)
    CmdPrintContract singleLine mbFilePath ->
      maybe putStrLn writeFileUtf8 mbFilePath $
        printLorentzContract singleLine tzbtcCompileWay tzbtcContract
    CmdPrintAgentContract singleLine mbFilePath ->
      maybe putStrLn writeFileUtf8 mbFilePath $
        printLorentzContract singleLine lcwDumb (agentContract @Parameter)
        -- Here agentContract that is printed is the one that target a
        -- contract with the parameter `Parameter`. If we can obtain
        -- runtime witness or type class dictionaries for the constraints
        -- `agentContract` require it might be possible to read a contract
        -- from a file, and printout an agent contract that can migrate
        -- to it, or print out an error if it is incompatible.
    CmdPrintInitialStorage adminAddress redeemAddress ->
      putStrLn $ printLorentzValue True (mkStorage adminAddress redeemAddress mempty mempty)
    CmdParseParameter t ->
      either (throwString . pretty) (putTextLn . pretty) $
      parseLorentzValue @Parameter t
    CmdTestScenario TestScenarioOptions {..} -> do
      maybe (throwString "Not enough addresses")
        (maybe putStrLn writeFileUtf8 tsoOutput) $
        showTestScenario <$> mkTestScenario tsoMaster tsoAddresses
    CmdTest -> doTest
  where
    printParam :: Parameter -> IO ()
    printParam = putStrLn . printLorentzValue True
    programInfo =
      info (helper <*> versionOption <*> argParser) $
      mconcat
        [ fullDesc
        , progDesc
            "TZBTC - Wrapped bitcoin on tezos blockchain"
        , header "TZBTC Tools"
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
    , "  tzbtc mint --help", linebreak
    , "USAGE EXAMPLE:", linebreak
    , "  tzbtc mint --to tz1U1h1YzBJixXmaTgpwDpZnbrYHX3fMSpvb --value 100500", linebreak
    , linebreak
    , "  This command will return raw Michelson representation", linebreak
    , "  of `Mint` entrypoint with the given arguments.", linebreak
    , "  This raw Michelson value can later be submited to the", linebreak
    , "  chain using tezos-client"
    ]

mkTestScenario :: Address -> [Address] -> Maybe (TestScenario Parameter)
mkTestScenario owner addresses = do
  addr0 <- addresses ^? ix 0
  addr1 <- addresses ^? ix 1
  pure
    [ (owner, AddOperator (#operator .! owner))
    , (owner, Pause ())
    , (owner, Unpause ())
    , (owner, Mint (#to .! addr0, #value .! 100500))
    , (owner, Mint (#to .! addr1, #value .! 100500))
    ]

doTest :: IO ()
doTest = do
  let
    operatorAddress = unsafeParseAddress "tz1VwXeEPw2tkTgDSUUbEb5fe63b24gNEssa"
    alice = unsafeParseAddress "tz1SdBs7PEc75PSg7Cyxq6sv2TgqSqRy1ZKJ"
    adminAddress = unsafeParseAddress "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx"
    redeemAddress = adminAddress
  -- Deploy V1
  v1 <-
    alphanetOriginate
      tzbtcContract
      (mkStorage
        adminAddress
        redeemAddress
        mempty
        mempty)
      "TZBTC_V1"
      adminAddress
  -- Add an operator
  callContract v1 (AddOperator (#operator .! operatorAddress)) adminAddress
  -- Mint some coins for alice
  callContract v1 (Mint (#to .! alice, #value .! 763)) operatorAddress
  -- Deploy V2
  v2 <-
    alphanetOriginate
      tzbtcContract
      (mkStorage
        adminAddress
        redeemAddress
        mempty
        mempty)
      "TZBTC_V2"
      adminAddress
  -- Deploy Manager
  manager <- alphanetOriginate
    agentContract
    (Agent.mkStorage (unContractAddress v1) v2)
    "Manager"
    adminAddress
  -- Pause v1
  callContract v1 (Pause ()) operatorAddress
  -- Start migrationTo in V1
  callContract v1 (StartMigrateTo (#migrationManager .! manager) ) adminAddress
  -- Start migrationFrom in V2
  callContract v2 (StartMigrateFrom (#migrationManager .! manager) ) adminAddress
  -- Unpause v1
  callContract v1 (Unpause ()) adminAddress
  -- Alice migrates her coins : Step 1
  callContract v1 (Migrate ()) alice
  -- Alice migrates her coins : Step 2
  callContract manager (Agent.ForwardTokens ()) alice
  -- Check balance of alice in V2
  callContract v2 (GetBalance (View alice (ContractAddr $ unsafeParseAddress "KT19JjUrQr7A4CM9FmwtcYJvXvTjguvi4XaS"))) adminAddress

alphanetOriginate
  :: forall cp st.
  ( Each [Typeable, SingI] [ToT cp, ToT st]
  , Each '[NoOperation] [cp, st], NoBigMap cp, CanHaveBigMap st
  , IsoValue st, HasNoOp (ToT st)
  )
  => Contract cp st -> st -> String -> Address -> IO (ContractAddr cp)
alphanetOriginate c storage cname owner = do
  tzFile <- writeTempFile "." "contract.tz" $ T.unpack $ printLorentzContract True lcwDumb c
  (_, out, _) <- readProcess $ proc "./alphanet.sh"
    [ "client" , "originate", "contract", cname, "for", addrToStr owner
    ,    "transferring", "0", "from", addrToStr owner
    , "running", "container:" ++ tzFile
    , "--burn-cap", "100"
    , "--force"
    , "-q"
    , "--init"
    , T.unpack $ printLorentzValue True storage
    ]
  BSL.putStrLn out
  pure $
    either (const $ error "Unexpected output") ContractAddr $ parseOnly deployOutputParser (BSL.toStrict out)

addrToStr :: Address -> String
addrToStr = ST.unpack . formatAddress

callContract
  :: forall cp.
     ( Each '[Typeable, SingI] '[ToT cp]
     , HasNoOp (ToT cp)
     , IsoValue cp
     , Each '[NoOperation] '[cp], NoBigMap cp)
  => ContractAddr cp -> cp -> Address -> IO ()
callContract addr param caller = do
  putStrLn $ (addrToStr caller) ++ " Calling contract " ++ (addrToStr $ unContractAddress addr) ++ " with " ++ (T.unpack $ printLorentzValue True param)
  let
    args =
      [ "client" , "transfer", "0", "from", addrToStr caller
      , "to", addrToStr $ unContractAddress addr, "--arg"
      , (T.unpack $ printLorentzValue True param)
      , "-q"
      , "--burn-cap", "100"
      ]
  (_, out, err) <- readProcess $ proc "./alphanet.sh" args
  BSL.putStrLn out
  BSL.putStrLn err

deployOutputParser :: Parser Address
deployOutputParser = do
  manyTill anyWord8 $ string "New contract"
  skipMany space
  ca <- AP.takeTill isSpace_w8
  skipMany space
  void $ string "originated."
  pure $ unsafeParseAddress $ decodeUtf8 ca
