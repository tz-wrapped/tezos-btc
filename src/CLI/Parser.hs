{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module CLI.Parser
  ( CmdLnArgs (..)
  , addressArgument
  , addressOption
  , argParser
  , mkCommandParser
  , mTextOption
  ) where

import Options.Applicative (command, help, hsubparser, info, long, progDesc, switch)
import qualified Options.Applicative as Opt

import Lorentz ((:!))
import Lorentz.Contracts.Metadata
import Morley.CLI
import Tezos.Address (Address)
import Util.CLI
import Util.Named

-- | Represents the Cmd line commands with inputs/arguments.
data CmdLnArgs
  = CmdPrintInitialStorage Address
  | CmdPrintContract Bool (Maybe FilePath)
  | CmdPrintMultisigContract Bool Bool (Maybe FilePath)
  | CmdPrintDoc (Maybe FilePath)
  | CmdParseParameter Text
  | CmdMigrate
      ("version" :! Natural)
      ("redeemAddress" :! Address)
      ("tokenMetadata" :! TokenMetadata)
      ("output" :! Maybe FilePath)

argParser :: Opt.Parser CmdLnArgs
argParser = hsubparser $
  printCmd <> printMultisigCmd
  <> printInitialStorageCmd <> printDoc
  <> parseParameterCmd <> migrateCmd
  where
    singleLineSwitch = onelineOption
    printCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    printCmd =
      (mkCommandParser
        "printContract"
        (CmdPrintContract <$> singleLineSwitch <*> outputOption)
        "Print token contract")
    printMultisigCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    printMultisigCmd =
      (mkCommandParser
        "printMultisigContract"
        (CmdPrintMultisigContract <$> singleLineSwitch <*> customErrorsFlag <*> outputOption)
        "Print token contract")
      where
        customErrorsFlag = switch
          (long "use-custom-errors" <>
           help "By default the multisig contract fails with 'unit' in all error cases.\n\
                \This flag will deploy the custom version of multisig\n\
                \contract with human-readable string errors.")
    printInitialStorageCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    printInitialStorageCmd =
      (mkCommandParser
         "printInitialStorage"
         (CmdPrintInitialStorage
            <$> addressOption Nothing
            (#name .! "owner-address") (#help .! "Owner's address")
         )
         "Print initial contract storage with the given owner and \
         \redeem addresses")
    printDoc :: Opt.Mod Opt.CommandFields CmdLnArgs
    printDoc =
      (mkCommandParser
        "printContractDoc"
        (CmdPrintDoc <$> outputOption)
        "Print tzbtc contract documentation")
    parseParameterCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    parseParameterCmd =
      (mkCommandParser
          "parseContractParameter"
          (CmdParseParameter <$> Opt.strArgument mempty)
          "Parse contract parameter to Lorentz representation")
    migrateCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    migrateCmd =
      (mkCommandParser
          "migrate"
          (CmdMigrate
            <$> namedParser Nothing "Target version"
            <*> namedParser Nothing "Redeem address"
            <*> fmap (#tokenMetadata .!) parseSingleTokenMetadata
            <*> (#output <.!> outputOption))
          "Print migration scripts.")

mkCommandParser
  :: String
  -> Opt.Parser a
  -> String
  -> Opt.Mod Opt.CommandFields a
mkCommandParser commandName parser desc =
  command commandName $ info parser $ progDesc desc

addressArgument :: String -> Opt.Parser Address
addressArgument hInfo = mkCLArgumentParser Nothing (#help .! hInfo)
