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
import Lorentz.Common (showTestScenario)
import Util.IO (writeFileUtf8)
import Paths_tzbtc (version)

import CLI.Parser
import Lorentz.Contracts.TZBTC
  (Parameter(..), agentContract, mkStorage, tzbtcContract, tzbtcDoc)
import Lorentz.Contracts.TZBTC.Proxy (tzbtcProxyContract)
import Lorentz.Contracts.TZBTC.Test (mkTestScenario)
import qualified Lorentz.Contracts.TZBTC.V0 as V0
import qualified Lorentz.Contracts.TZBTC.V1 as V1

-- Here in main function we will just accept commands from user
-- and print the smart contract parameter by using `printLorentzValue`
-- function from Lorentz
main :: IO ()
main = do
  cmd <- execParser programInfo
  case cmd of
    CmdPrintContract singleLine mbFilePath ->
      printContract singleLine mbFilePath lcwEntryPoints tzbtcContract
    CmdPrintAgentContract singleLine mbFilePath ->
      --printContract singleLine mbFilePath lcwDumb V0.tzbtcContract
      putStrLn $ V1.printContractCode
        -- Here agentContract that is printed is the one that target a
        -- contract with the parameter `Parameter`. If we can obtain
        -- runtime witness or type class dictionaries for the constraints
        -- `agentContract` require it might be possible to read a contract
        -- from a file, and printout an agent contract that can migrate
        -- to it, or print out an error if it is incompatible.
    CmdPrintProxyContract singleLine mbFilePath ->
      printContract singleLine mbFilePath lcwEntryPointsRecursive tzbtcProxyContract
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
    printContract
      :: (KnownValue parameter, KnownValue storage)
      => Bool
      -> Maybe FilePath
      -> LorentzCompilationWay parameter storage
      -> Contract parameter storage
      -> IO ()
    printContract singleLine mbFilePath lcw  c =
      maybe putStrLn writeFileUtf8 mbFilePath $
        printLorentzContract singleLine lcw c
    programInfo =
      info (helper <*> versionOption <*> argParser) $
      mconcat
        [ fullDesc
        , progDesc
            "TZBTC - Wrapped bitcoin on tezos blockchain"
        , header "TZBTC Developer tools"
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
    , "  tzbtc printInitialStorage --help", linebreak
    , "USAGE EXAMPLE:", linebreak
    , "  tzbtc printInitialStorage --admin-address \
      \tz1U1h1YzBJixXmaTgpwDpZnbrYHX3fMSpvby"
    , linebreak
    , "                            --redeem-address \
      \tz1U1h1YzBJixXmaTgpwDpZnbrYHX3fMSpvby"
    , linebreak
    , "  This command will return raw Michelson representation", linebreak
    , "  of TZBTC contract storage that can later be used for", linebreak
    , "  contract origination using tezos-client", linebreak
    ]

