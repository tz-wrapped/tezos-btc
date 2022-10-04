{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main
  ( main
  ) where

import Control.Exception.Safe (throwString)
import Data.Version (showVersion)
import Fmt (pretty)
import Named (arg)
import Options.Applicative
  (execParser, footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)

import Lorentz hiding (not)
import Paths_tzbtc (version)

import CLI.Parser
import Client.IO ()
import Lorentz.Contracts.Multisig
import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.V0 qualified as V0
import Lorentz.Contracts.TZBTC.V1 qualified as V1
import Lorentz.Contracts.TZBTC.V2 qualified as V2
import Morley.Michelson.Parser.Types
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
    CmdPrintDoc ver mbFilePath -> let
      gitRev =
        $mkDGitRevision $ GitRepoSettings $
          mappend "https://github.com/tz-wrapped/tezos-btc/commit/"
      contractDoc = case ver of
        V0 -> error "Documentation for V0 is not interesting"
        V1 -> V1.tzbtcDoc
        V2 -> V2.tzbtcDoc
      in maybe printTextLn writeFileUtf8 mbFilePath
        (contractDocToMarkdown $ buildDoc . attachDocCommons gitRev $ contractDoc)
    CmdParseParameter ver t ->
      let parseAndPrint :: forall ver. (Typeable ver, _) => IO ()
          parseAndPrint =
            either (throwString . pretty) (printStringLn . pretty) $
            parseLorentzValue @(Parameter ver) MSUnspecified t
      in case ver of
        V0 -> parseAndPrint @V0.TZBTCv0
        V1 -> parseAndPrint @V1.TZBTCv1
        V2 -> parseAndPrint @V2.TZBTCv2
    CmdMigrate (arg #output -> fp) migrateCmd ->
      maybe printTextLn writeFileUtf8 fp $
        case migrateCmd of
          MigrateV1
            (arg #redeemAddress -> redeem)
            (arg #tokenMetadata -> tokenMetadata) -> do
              let
                originationParams = V1Parameters
                  { v1RedeemAddress = toAddress redeem
                  , v1TokenMetadata = tokenMetadata
                  , v1Balances = mempty
                  }
              makeMigrationParamsV1 tzbtcContractRouterV1 $
                (migrationScriptsV1 originationParams)
          MigrateV2
            (arg #redeemAddress -> redeem)
            (arg #tokenMetadata -> tokenMetadata) -> do
              let
                originationParams = V1Parameters
                  { v1RedeemAddress = toAddress redeem
                  , v1TokenMetadata = tokenMetadata
                  , v1Balances = mempty
                  }
              makeMigrationParamsV2 tzbtcContractRouterV2 $
                (migrationScriptsV2 originationParams)
          MigrateV2FromV1 -> do
            makeMigrationParamsV2FromV1
              (migrationScriptsV2FromV1 V2.V2ParametersFromV1)

  where
    multisigContract
      :: forall (e :: ErrorsKind).
        (Typeable e, ErrorHandler e)
      => Contract MSigParameter MSigStorage ()
    multisigContract = tzbtcMultisigContract @e
    printContract
      :: ( HasFilesystem m
         , HasCmdLine m
         )
      => Bool
      -> Maybe FilePath
      -> Contract parameter storage ()
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
