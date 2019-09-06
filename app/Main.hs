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
