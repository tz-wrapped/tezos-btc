{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Main
  ( main
  ) where

import Data.Version (showVersion)
import Data.Vinyl.Derived (Label)
import Fmt (pretty)
import Options.Applicative
  (execParser, footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)

import Lorentz.Macro (View(..))
import Michelson.Typed.Haskell.Value (ContractAddr(..))
import Paths_tzbtc (version)
import Tezos.Address (Address, formatAddress)
import Util.Named ((.!))
import Util.TypeLits (symbolValT')

import Client.IO
import Client.Parser
import Lorentz.Contracts.TZBTC
import Util.MultiSig

main :: IO ()
main = do
  ClientArgs cmd dryRunFlag <- execParser programInfo
  case dryRunFlag of
    True -> pass
    False -> case cmd of
      CmdConfig editFlag partialConfig ->
        runConfigEdit editFlag partialConfig
      CmdSetupClient config -> setupClient config
      CmdMint to' value mbMultisig -> do
        to <- addrOrAliasToAddr to'
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ Mint (#to .! to, #value .! value)
      CmdBurn burnParams mbMultisig ->
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ Burn burnParams
      CmdTransfer from' to' value -> do
        [from, to] <- mapM addrOrAliasToAddr [from', to']
        runTzbtcContract $
          fromFlatParameter $ Transfer (#from .! from, #to .! to, #value .! value)
      CmdApprove spender' value -> do
        spender <- addrOrAliasToAddr spender'
        runTzbtcContract $
          fromFlatParameter $ Approve (#spender .! spender, #value .! value)
      CmdGetAllowance (owner', spender') mbCallback' ->
        case mbCallback' of
          Just callback' -> do
            [owner, spender, callback] <- mapM addrOrAliasToAddr [owner', spender', callback']
            runTzbtcContract $ fromFlatParameter $ GetAllowance $
              View (#owner .! owner, #spender .! spender) (ContractAddr callback)
          Nothing -> do
            [owner, spender] <- mapM addrOrAliasToAddr [owner', spender']
            allowance <- getAllowance owner spender
            putTextLn $ "Allowance: " <> show allowance
      CmdGetBalance owner' mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            [owner, callback] <- mapM addrOrAliasToAddr [owner', callback']
            runTzbtcContract $
              fromFlatParameter $ GetBalance $ View owner (ContractAddr callback)
          Nothing -> do
            owner <- addrOrAliasToAddr owner'
            balance <- getBalance owner
            putTextLn $ "Balance: " <> show balance
      CmdAddOperator operator' mbMultisig -> do
        operator <- addrOrAliasToAddr operator'
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ AddOperator (#operator .! operator)
      CmdRemoveOperator operator' mbMultisig -> do
        operator <- addrOrAliasToAddr operator'
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ RemoveOperator (#operator .! operator)
      CmdPause mbMultisig -> runMultisigTzbtcContract mbMultisig $
        fromFlatParameter $ Pause ()
      CmdUnpause mbMultisig -> runMultisigTzbtcContract mbMultisig $
        fromFlatParameter $ Unpause ()
      CmdSetRedeemAddress redeem' mbMultisig -> do
        redeem <- addrOrAliasToAddr redeem'
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ SetRedeemAddress (#redeem .! redeem)
      CmdTransferOwnership newOwner' mbMultisig -> do
        newOwner <- addrOrAliasToAddr newOwner'
        runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ TransferOwnership (#newOwner .! newOwner)
      CmdAcceptOwnership p -> runTzbtcContract $
        fromFlatParameter $ AcceptOwnership p
      CmdGetTotalSupply mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            callback <- addrOrAliasToAddr callback'
            runTzbtcContract $
              fromFlatParameter $ GetTotalSupply $ View () (ContractAddr callback)
          Nothing ->
            printFieldFromStorage @Natural #totalSupply "Total supply: " show
      CmdGetTotalMinted mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            callback <- addrOrAliasToAddr callback'
            runTzbtcContract $
              fromFlatParameter $ GetTotalMinted $ View () (ContractAddr callback)
          Nothing ->
            printFieldFromStorage @Natural #totalMinted "Total minted: " show
      CmdGetTotalBurned mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            callback <- addrOrAliasToAddr callback'
            runTzbtcContract $
              fromFlatParameter $ GetTotalBurned $ View () (ContractAddr callback)
          Nothing ->
            printFieldFromStorage @Natural #totalBurned "Total burned: " show
      CmdGetAdministrator mbCallback' -> do
        case mbCallback' of
          Just callback' -> do
            callback <- addrOrAliasToAddr callback'
            runTzbtcContract $
              fromFlatParameter $ GetAdministrator $ View () (ContractAddr callback)
          Nothing ->
            printFieldFromStorage @Address #admin "Admininstator: " formatAddress
      CmdGetOpDescription packageFilePath -> do
        pkg <- getPackageFromFile packageFilePath
        case pkg of
          Left err -> putStrLn err
          Right package -> putTextLn $ pretty package
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
    runMultisigTzbtcContract :: (Maybe FilePath) -> Parameter a -> IO ()
    runMultisigTzbtcContract mbMultisig param =
      case mbMultisig of
        Just fp -> case toSafeParam param of
          Just subParam -> createMultisigPackage fp subParam
          _ -> putTextLn "Unable to call multisig for View entrypoints"
        Nothing -> runTzbtcContract param
    printFieldFromStorage
      :: forall t name. HasStoreTemplateField t name
      => Label name -> Text -> (t -> Text) -> IO ()
    printFieldFromStorage _ prefix formatter = do
      mbField <- getFieldFromTzbtcUStore @name @t
      case mbField of
        Just field' -> putTextLn $ prefix <> formatter field'
        Nothing -> putStrLn $ "Field " <>
          symbolValT' @name <> " not found in the contract storage"
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
