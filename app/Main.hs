{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
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
import Paths_tzbtc (version)

import CLI.Parser
import Client.IO ()
import Lorentz.Contracts.Multisig
import Lorentz.Contracts.TZBTC
  ( Parameter, TZBTCv1, V1Parameters (..), migrationScripts, mkEmptyStorageV0, tzbtcContract
  , tzbtcContractRouter, tzbtcDoc)
import Util.AbstractIO
import Util.Migration

main :: IO ()
main = do
  cmd <- execParser programInfo
  case cmd of
    CmdPrintContract singleLine mbFilePath ->
      printContract singleLine mbFilePath tzbtcContract
    CmdPrintMultisigContract singleLine customErrorsFlag mbFilePath ->
      if customErrorsFlag
        then printContract singleLine mbFilePath (multisigContract @'CustomErrors)
        else printContract singleLine mbFilePath (multisigContract @'BaseErrors)
    CmdPrintInitialStorage ownerAddress -> do
      printTextLn $ printLorentzValue True (mkEmptyStorageV0 ownerAddress)
    CmdPrintDoc mbFilePath -> let
      gitRev =
        $mkDGitRevision $ GitRepoSettings $
          mappend "https://github.com/tz-wrapped/tezos-btc/commit/"
      in maybe printTextLn writeFileUtf8 mbFilePath
        (contractDocToMarkdown $ buildLorentzDocWithGitRev gitRev tzbtcDoc)
    CmdParseParameter t ->
      either (throwString . pretty) (printStringLn . pretty) $
      parseLorentzValue @(Parameter TZBTCv1) t
    CmdMigrate
      (arg #version -> version_)
      (arg #redeemAddress -> redeem)
      (arg #tokenMetadata -> tokenMetadata)
      (arg #output -> fp) -> do
        let
          originationParams = V1Parameters
            { v1RedeemAddress = redeem
            , v1TokenMetadata = tokenMetadata
            , v1Balances = mempty
            }
        maybe printTextLn writeFileUtf8 fp $
          makeMigrationParams version_ tzbtcContractRouter $
            (migrationScripts originationParams)
  where
    multisigContract
      :: forall (e :: ErrorsKind).
        (Typeable e, ErrorHandler e)
      => ContractCode MSigParameter MSigStorage
    multisigContract = tzbtcMultisigContract @e
    printContract
      :: ( NiceParameterFull parameter
         , NiceStorage storage
         , HasFilesystem m
         , HasCmdLine m)
      => Bool
      -> Maybe FilePath
      -> ContractCode parameter storage
      -> m ()
    printContract singleLine mbFilePath c =
      maybe printTextLn writeFileUtf8 mbFilePath $
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
    , "  tzbtc printInitialStorage --owner-address \
      \tz1U1h1YzBJixXmaTgpwDpZnbrYHX3fMSpvby"
    , linebreak
    , "                            --redeem-address \
      \tz1U1h1YzBJixXmaTgpwDpZnbrYHX3fMSpvby"
    , linebreak
    , "  This command will return raw Michelson representation", linebreak
    , "  of TZBTC contract storage that can later be used for", linebreak
    , "  contract origination using tezos-client", linebreak
    ]
