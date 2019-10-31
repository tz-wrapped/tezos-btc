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
import Paths_tzbtc (version)
import Util.IO (writeFileUtf8)

import CLI.Parser
import Lorentz.Contracts.TZBTC
  ( Parameter, mkEmptyStorageV0, migrationScripts
  , originationParams, tzbtcContractCode, tzbtcDoc
  )
import Lorentz.Contracts.TZBTC.Test (mkTestScenario)
import qualified Lorentz.Contracts.TZBTC.V0 as V0
import Util.Migration

-- Here in main function we will just accept commands from user
-- and print the smart contract parameter by using `printLorentzValue`
-- function from Lorentz
main :: IO ()
main = do
  cmd <- execParser programInfo
  case cmd of
    CmdPrintContract singleLine mbFilePath ->
      printContract singleLine mbFilePath V0.tzbtcContract
    CmdPrintInitialStorage adminAddress -> do
      putStrLn $ printLorentzValue True (mkEmptyStorageV0 adminAddress)
    CmdPrintDoc mbFilePath ->
      maybe putStrLn writeFileUtf8 mbFilePath tzbtcDoc
    CmdParseParameter t ->
      either (throwString . pretty) (putTextLn . pretty) $
      parseLorentzValue @(Parameter _) t
    CmdTestScenario TestScenarioOptions {..} -> do
      maybe (throwString "Not enough addresses")
        (maybe putStrLn writeFileUtf8 tsoOutput) $
        showTestScenario <$> mkTestScenario tsoMaster tsoAddresses
    CmdMigrate
      (arg #version -> version_)
      (arg #adminAddress -> admin)
      (arg #redeemAddress -> redeem)
      (arg #output -> fp) -> do
        (maybe putStrLn writeFileUtf8 fp) $
          makeMigrationParams version_ tzbtcContractCode $
            (migrationScripts $ originationParams admin redeem mempty)
  where
    printContract
      :: (ParameterEntryPoints parameter, NiceStorage storage)
      => Bool
      -> Maybe FilePath
      -> Contract parameter storage
      -> IO ()
    printContract singleLine mbFilePath c =
      maybe putStrLn writeFileUtf8 mbFilePath $
        printLorentzContract singleLine c
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
