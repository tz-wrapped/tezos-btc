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

import Lorentz (lcwDumb, parseLorentzValue, printLorentzContract, printLorentzValue)
import Lorentz.Common (showTestScenario)
import Paths_tzbtc (version)
import Util.IO (writeFileUtf8)

import CLI.Parser
import Client.IO
import Client.Parser
import Lorentz.Contracts.TZBTC
  (Parameter(..), agentContract, mkStorage, tzbtcCompileWay, tzbtcContract, tzbtcDoc)
import Lorentz.Contracts.TZBTC.Proxy (tzbtcProxyContract)
import Lorentz.Contracts.TZBTC.Test (mkTestScenario)

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
