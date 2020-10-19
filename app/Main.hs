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
import Options.Applicative
  (execParser, footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)

import Lorentz hiding (not)
import Morley.Nettest
import Paths_tzbtc (version)

import CLI.Parser
import Client.Env
import Client.IO (mkInitEnv)
import Client.Types (ClientConfig(..))
import Client.Util
import Lorentz.Contracts.Multisig
import Lorentz.Contracts.TZBTC
  (Parameter, TZBTCv0, TZBTCv1, V1Parameters(..), migrationScripts, mkEmptyStorageV0,
  tzbtcContract, tzbtcContractRouter, tzbtcDoc)
import Lorentz.Contracts.TZBTC.Test (TestUpgrade, smokeTests, testUpgradeToV1)
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
        V1 -> tzbtcDoc
      in maybe printTextLn writeFileUtf8 mbFilePath
        (contractDocToMarkdown $ buildLorentzDocWithGitRev gitRev contractDoc)
    CmdParseParameter ver t ->
      let parseAndPrint :: forall ver. (Typeable ver, _) => IO ()
          parseAndPrint =
            either (throwString . pretty) (printStringLn . pretty) $
            parseLorentzValue @(Parameter ver) t
      in case ver of
        V0 -> parseAndPrint @TZBTCv0
        V1 -> parseAndPrint @TZBTCv1
    CmdTestScenario ver (arg #verbosity -> verbose) (arg #dryRun -> dryRun) -> do
      env <- mkInitEnv
      let
        testUpgrade :: forall m. Monad m => TestUpgrade m
        testUpgrade = case ver of
          V0 -> mempty
          V1 -> testUpgradeToV1
      config <- runMaybeT $ do
        guard (not dryRun)
        tzbtcConfig <- lift . runAppM env $ throwLeft readConfig
        return $ toMorleyClientConfig verbose tzbtcConfig
      smokeTests config testUpgrade
    CmdMigrate (arg #output -> fp) migrateCmd ->
      maybe printTextLn writeFileUtf8 fp $
        case migrateCmd of
          MigrateV1
            (arg #redeemAddress -> redeem)
            (arg #tokenMetadata -> tokenMetadata) -> do
              let
                originationParams = V1Parameters
                  { v1RedeemAddress = redeem
                  , v1TokenMetadata = tokenMetadata
                  , v1Balances = mempty
                  }
              makeMigrationParamsV1 tzbtcContractRouter $
                (migrationScripts originationParams)
  where
    toMorleyClientConfig :: Word -> ClientConfig -> MorleyClientConfig
    toMorleyClientConfig verbose ClientConfig {..} =
      MorleyClientConfig
        { mccAliasPrefix = Just "TZBTC_Smoke_tests"
        , mccNodeAddress = Just ccNodeAddress
        , mccNodePort = Just (fromIntegral ccNodePort)
        , mccTezosClientPath = ccTezosClientExecutable
        , mccMbTezosClientDataDir = Nothing
        , mccNodeUseHttps = ccNodeUseHttps
        , mccVerbosity = verbose
        , mccSecretKey = Nothing
        }
    multisigContract
      :: forall (e :: ErrorsKind).
        (Typeable e, ErrorHandler e)
      => Contract MSigParameter MSigStorage
    multisigContract = tzbtcMultisigContract @e
    printContract
      :: ( NiceParameterFull parameter
         , NiceStorage storage
         , HasFilesystem m
         , HasCmdLine m)
      => Bool
      -> Maybe FilePath
      -> Contract parameter storage
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
