{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Main
  ( main
  ) where

import Data.Version (showVersion)
import Fmt (pretty)
import Options.Applicative
  (execParser, footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)

import Lorentz.Macro (View(..))
import Michelson.Typed.Haskell.Value (ContractAddr(..))
import Paths_tzbtc (version)
import Util.Named ((.!))

import Client.IO
import Client.Parser
import Lorentz.Contracts.TZBTC (Parameter(..))
import Lorentz.Contracts.TZBTC.Types
  (ParameterWithoutView(..), ParameterWithView(..))
import Util.MultiSig

main :: IO ()
main = do
  ClientArgs cmd dryRunFlag <- execParser programInfo
  case dryRunFlag of
    True -> pass
    False -> case cmd of
      CmdSetupClient config -> setupClient config
      CmdMint to' value mbMultisig -> do
        to <- addrOrAliasToAddr to'
        runMultisigTzbtcContract mbMultisig $
          EntrypointsWithoutView $ Mint (#to .! to, #value .! value)
      CmdBurn burnParams mbMultisig ->
        runMultisigTzbtcContract mbMultisig $
          EntrypointsWithoutView $ Burn burnParams
      CmdTransfer from' to' value -> do
        [from, to] <- mapM addrOrAliasToAddr [from', to']
        runTzbtcContract $
          EntrypointsWithoutView $ Transfer (#from .! from, #to .! to, #value .! value)
      CmdApprove spender' value -> do
        spender <- addrOrAliasToAddr spender'
        runTzbtcContract $
          EntrypointsWithoutView $ Approve (#spender .! spender, #value .! value)
      CmdGetAllowance (owner', spender') callback' -> do
        [owner, spender, callback] <- mapM addrOrAliasToAddr [owner', spender', callback']
        runTzbtcContract $ EntrypointsWithView $ GetAllowance $
          View (#owner .! owner, #spender .! spender) (ContractAddr callback)
      CmdGetBalance owner' callback' -> do
        [owner, callback] <- mapM addrOrAliasToAddr [owner', callback']
        runTzbtcContract $
          EntrypointsWithView $ GetBalance $ View owner (ContractAddr callback)
      CmdAddOperator operator' mbMultisig -> do
        operator <- addrOrAliasToAddr operator'
        runMultisigTzbtcContract mbMultisig $
          EntrypointsWithoutView $ AddOperator (#operator .! operator)
      CmdRemoveOperator operator' mbMultisig -> do
        operator <- addrOrAliasToAddr operator'
        runMultisigTzbtcContract mbMultisig $
          EntrypointsWithoutView $ RemoveOperator (#operator .! operator)
      CmdPause mbMultisig -> runMultisigTzbtcContract mbMultisig $
        EntrypointsWithoutView $ Pause ()
      CmdUnpause mbMultisig -> runMultisigTzbtcContract mbMultisig $
        EntrypointsWithoutView $ Unpause ()
      CmdSetRedeemAddress redeem' mbMultisig -> do
        redeem <- addrOrAliasToAddr redeem'
        runMultisigTzbtcContract mbMultisig $
          EntrypointsWithoutView $ SetRedeemAddress (#redeem .! redeem)
      CmdTransferOwnership newOwner' mbMultisig -> do
        newOwner <- addrOrAliasToAddr newOwner'
        runMultisigTzbtcContract mbMultisig $
          EntrypointsWithoutView $ TransferOwnership (#newOwner .! newOwner)
      CmdAcceptOwnership p -> runTzbtcContract $
        EntrypointsWithoutView $ AcceptOwnership p
      CmdStartMigrateTo manager' mbMultisig -> do
        manager <- addrOrAliasToAddr manager'
        runMultisigTzbtcContract mbMultisig $
          EntrypointsWithoutView $ StartMigrateTo (#migrationManager .! manager)
      CmdStartMigrateFrom manager' mbMultisig -> do
        manager <- addrOrAliasToAddr manager'
        runMultisigTzbtcContract mbMultisig $
          EntrypointsWithoutView $ StartMigrateFrom (#migrationManager .! manager)
      CmdMigrate p -> runTzbtcContract $ EntrypointsWithoutView $ Migrate p
      CmdGetOpDescription packageFilePath -> do
        pkg <- getPackageFromFile packageFilePath
        case pkg of
          Left err -> putStrLn err
          Right package -> putStrLn (pretty package :: Text)
      CmdGetBytesToSign packageFilePath -> do
        pkg <- getPackageFromFile packageFilePath
        case pkg of
          Left err -> putStrLn err
          Right package -> putStrLn $ getBytesToSign package
      CmdAddSignature pk sign packageFilePath -> do
        pkg <- getPackageFromFile packageFilePath
        case pkg of
          Left err -> putStrLn err
          Right package -> case addSignature package (pk, sign) of
            Right signedPackage -> writePackageToFile signedPackage packageFilePath
            Left err -> putStrLn err
      CmdSignPackage packageFilePath -> do
        pkg <- getPackageFromFile packageFilePath
        case pkg of
          Left err -> putStrLn err
          Right package -> do
            signRes <- signPackageForConfiguredUser package
            case signRes of
              Left err -> putStrLn err
              Right signedPackage -> writePackageToFile signedPackage packageFilePath
      CmdCallMultisig packagesFilePaths -> do
        pkgs <- fmap sequence $ mapM getPackageFromFile packagesFilePaths
        case pkgs of
          Left err -> putStrLn err
          Right packages -> runMultisigContract packages
  where
    runMultisigTzbtcContract :: (Maybe FilePath) -> Parameter -> IO ()
    runMultisigTzbtcContract mbMultisig param =
      case mbMultisig of
        Just fp -> case param of
          EntrypointsWithoutView subParam -> createMultisigPackage fp subParam
          _ -> putStrLn ("Unable to call multisig for View entrypoints" :: Text)
        Nothing -> runTzbtcContract param
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
