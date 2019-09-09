{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Main
  ( main
  ) where

import Control.Exception.Safe (throwString)
import Data.Version (showVersion)
import Fmt (pretty)
import Options.Applicative
  (execParser, footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)

import Lorentz
  (CanHaveBigMap, Contract, KnownValue, NoBigMap, NoOperation,
   parseLorentzValue, printLorentzContract, printLorentzValue, lcwDumb)
import Lorentz.Common (showTestScenario)
import Util.IO (writeFileUtf8)
import Paths_tzbtc (version)

import CLI.Parser
import Lorentz.Contracts.TZBTC (Parameter(..), agentContract, mkStorage, tzbtcContract)
import Lorentz.Contracts.TZBTC.Proxy (tzbtcProxyContract)
import Lorentz.Contracts.TZBTC.Test (mkTestScenario)

-- Here in main function we will just accept commands from user
-- and print the smart contract parameter by using `printLorentzValue`
-- function from Lorentz
main :: IO ()
main = do
  cmd <- execParser programInfo
  case cmd of
    CmdMint mintParams -> printParam (Mint mintParams)
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
      printContract singleLine mbFilePath tzbtcContract
    CmdPrintAgentContract singleLine mbFilePath ->
      printContract singleLine mbFilePath (agentContract @Parameter)
        -- Here agentContract that is printed is the one that target a
        -- contract with the parameter `Parameter`. If we can obtain
        -- runtime witness or type class dictionaries for the constraints
        -- `agentContract` require it might be possible to read a contract
        -- from a file, and printout an agent contract that can migrate
        -- to it, or print out an error if it is incompatible.
    CmdPrintProxyContract singleLine mbFilePath ->
      printContract singleLine mbFilePath tzbtcProxyContract
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
    printContract
      :: ( KnownValue parameter, KnownValue storage
         , NoOperation parameter, NoOperation storage
         , NoBigMap parameter, CanHaveBigMap storage)
      => Bool -> Maybe FilePath -> Contract parameter storage -> IO ()
    printContract singleLine mbFilePath contract =
      maybe putStrLn writeFileUtf8 mbFilePath $
        printLorentzContract singleLine lcwDumb contract
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

