{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Main
  ( main
  ) where

import Data.Version (showVersion)
import Options.Applicative
  (execParser, footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)

import Lorentz (printLorentzContract, printLorentzValue)
import Paths_tzbtc (version)

import CLI.Parser
import Lorentz.Contracts.TZBTC (Parameter(..), tzbtcContract)

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
    CmdPause pauseParams -> printParam (Pause pauseParams)
    CmdSetRedeemAddress setRedeemAddressParams ->
      printParam (SetRedeemAddress setRedeemAddressParams)
    CmdTransferOwnership p -> printParam (TransferOwnership p)
    CmdAcceptOwnership p -> printParam (AcceptOwnership p)
    CmdStartMigrateTo p -> printParam (StartMigrateTo p)
    CmdStartMigrateFrom p -> printParam (StartMigrateFrom p)
    CmdMigrate p -> printParam (Migrate p)
    CmdPrintContract singleLine ->
      putStrLn $
        printLorentzContract singleLine tzbtcContract
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
    [ " TODO ", linebreak ]
