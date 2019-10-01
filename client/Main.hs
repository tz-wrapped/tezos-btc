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
        runMultisigTzbtcContract mbMultisig $ Mint (#to .! to, #value .! value)
      CmdBurn burnParams mbMultisig ->
        runMultisigTzbtcContract mbMultisig $ Burn burnParams
      CmdTransfer from' to' value -> do
        [from, to] <- mapM addrOrAliasToAddr [from', to']
        runTzbtcContract $ Transfer (#from .! from, #to .! to, #value .! value)
      CmdApprove spender' value -> do
        spender <- addrOrAliasToAddr spender'
        runTzbtcContract $ Approve (#spender .! spender, #value .! value)
      CmdGetAllowance (owner', spender') callback' -> do
        [owner, spender, callback] <- mapM addrOrAliasToAddr [owner', spender', callback']
        runTzbtcContract $ GetAllowance $
          View (#owner .! owner, #spender .! spender) (ContractAddr callback)
      CmdGetBalance owner' callback' -> do
        [owner, callback] <- mapM addrOrAliasToAddr [owner', callback']
        runTzbtcContract $
          GetBalance $ View owner (ContractAddr callback)
      CmdAddOperator operator' mbMultisig -> do
        operator <- addrOrAliasToAddr operator'
        runMultisigTzbtcContract mbMultisig $ AddOperator (#operator .! operator)
      CmdRemoveOperator operator' mbMultisig -> do
        operator <- addrOrAliasToAddr operator'
        runMultisigTzbtcContract mbMultisig $ RemoveOperator (#operator .! operator)
      CmdPause mbMultisig -> runMultisigTzbtcContract mbMultisig $ Pause ()
      CmdUnpause mbMultisig -> runMultisigTzbtcContract mbMultisig $ Unpause ()
      CmdSetRedeemAddress redeem' mbMultisig -> do
        redeem <- addrOrAliasToAddr redeem'
        runMultisigTzbtcContract mbMultisig $ SetRedeemAddress (#redeem .! redeem)
      CmdTransferOwnership newOwner' mbMultisig -> do
        newOwner <- addrOrAliasToAddr newOwner'
        runMultisigTzbtcContract mbMultisig $ TransferOwnership (#newOwner .! newOwner)
      CmdAcceptOwnership p -> runTzbtcContract $ AcceptOwnership p
      CmdStartMigrateTo manager' mbMultisig -> do
        manager <- addrOrAliasToAddr manager'
        runMultisigTzbtcContract mbMultisig $
          StartMigrateTo (#migrationManager .! manager)
      CmdStartMigrateFrom manager' mbMultisig -> do
        manager <- addrOrAliasToAddr manager'
        runMultisigTzbtcContract mbMultisig $
          StartMigrateFrom (#migrationManager .! manager)
      CmdMigrate p -> runTzbtcContract $ Migrate p
      CmdGetOpDescription packageFilePath -> do
        pkg <- getPackageFromFile packageFilePath
        case pkg of
          Left err -> putStrLn err
          Right package -> putStrLn (pretty package :: Text)
      CmdGetPackageDescription packageFilePath -> do
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
            Right signedPackage -> writePackageToFile signedPackage  packageFilePath
            Left err -> putStrLn err
      CmdCallMultisig packagesFilePaths -> do
        pkgs <- fmap sequence $ mapM getPackageFromFile packagesFilePaths
        case pkgs of
          Left err -> putStrLn err
          Right packages -> runMultisigContract packages
  where
    runMultisigTzbtcContract :: (Maybe FilePath) -> Parameter -> IO ()
    runMultisigTzbtcContract mbMultisig param =
      case mbMultisig of
        Just fp -> createMultisigPackage fp param
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
