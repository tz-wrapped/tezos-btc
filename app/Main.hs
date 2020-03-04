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
import Morley.Nettest
import Paths_tzbtc (version)

import CLI.Parser
import Client.IO (mkInitEnv)
import Client.Env
import Client.Types (ClientConfig(..))
import Client.Util
import Lorentz.Contracts.Multisig
import Lorentz.Contracts.TZBTC
  (Parameter, TZBTCv1, V1Parameters(..), migrationScripts, mkEmptyStorageV0, tzbtcContract,
  tzbtcContractRouter, tzbtcDoc)
import Lorentz.Contracts.TZBTC.Test (smokeTests)
import Util.AbstractIO
import Util.Migration

-- Here in main function we will just accept commands from user
-- and print the smart contract parameter by using `printLorentzValue`
-- function from Lorentz
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
    CmdPrintDoc mbFilePath ->
      maybe printTextLn writeFileUtf8 mbFilePath (contractDocToMarkdown tzbtcDoc)
    CmdParseParameter t ->
      either (throwString . pretty) (printStringLn . pretty) $
      parseLorentzValue @(Parameter TZBTCv1) t
    CmdTestScenario verbose -> do
      env <- mkInitEnv
      tzbtcConfig <- runAppM env $ throwLeft readConfig
      smokeTests $ toNettestClientConfig verbose tzbtcConfig
    CmdMigrate
      (arg #version -> version_)
      (arg #redeemAddress -> redeem)
      (arg #tokenName -> tokenName)
      (arg #tokenCode -> tokenCode)
      (arg #output -> fp) -> do
        let
          originationParams = V1Parameters
            { v1RedeemAddress = redeem
            , v1Balances = mempty
            , v1TokenName = tokenName
            , v1TokenCode = tokenCode
            }
        maybe printTextLn writeFileUtf8 fp $
          makeMigrationParams version_ tzbtcContractRouter $
            (migrationScripts originationParams)
  where
    toNettestClientConfig :: Bool -> ClientConfig -> NettestClientConfig
    toNettestClientConfig verbose ClientConfig {..} =
      NettestClientConfig
        { nccScenarioName = Just "TZBTC_Smoke_tests"
        , nccNodeAddress = Just ccNodeAddress
        , nccNodePort = Just (fromIntegral ccNodePort)
        , nccTezosClient = ccTezosClientExecutable
        , nccVerbose = verbose
        , nccNodeUseHttps = ccNodeUseHttps
        }
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
