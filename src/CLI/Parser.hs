{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

{-# LANGUAGE ApplicativeDo #-}

module CLI.Parser
  ( CmdLnArgs (..)
  , addressArgument
  , addressOption
  , argParser
  , mkCommandParser
  , mTextOption
  , parseSingleTokenMetadata
  ) where

import Named (Name(..), arg)
import Options.Applicative (command, help, hsubparser, info, long, progDesc, short, switch)
import qualified Options.Applicative as Opt

import Lorentz.Contracts.Metadata
import Michelson.Text
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
  | CmdTestScenario ("verbosity" :! Word) ("dryRun" :! Bool)
  | CmdMigrate
      ("version" :! Natural)
      ("redeemAddress" :! Address)
      ("tokenMetadata" :! TokenMetadata)
      ("output" :! Maybe FilePath)

argParser :: Opt.Parser CmdLnArgs
argParser = hsubparser $
  printCmd <> printMultisigCmd
  <> printInitialStorageCmd <> printDoc
  <> parseParameterCmd <> testScenarioCmd <> migrateCmd
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
    testScenarioCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    testScenarioCmd =
      (mkCommandParser
          "testScenario"
          (CmdTestScenario
             <$> (#verbosity <.!> genericLength <$> many verbositySwitch)
             <*> (#dryRun <.!> dryRunSwitch))
          "Do smoke tests")
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
    verbositySwitch =
      Opt.flag' ()
                (short 'v' <>
                 long "verbose" <>
                 help "Increase verbosity (pass several times to increase further)")
    dryRunSwitch =
      switch (long "dry-run" <>
              help "Don't run tests over a real network.")

mkCommandParser
  :: String
  -> Opt.Parser a
  -> String
  -> Opt.Mod Opt.CommandFields a
mkCommandParser commandName parser desc =
  command commandName $ info parser $ progDesc desc

addressArgument :: String -> Opt.Parser Address
addressArgument hInfo = mkCLArgumentParser Nothing (#help .! hInfo)

-- | Parse `TokenMetadata` for a single token, with no extras
parseSingleTokenMetadata :: Opt.Parser TokenMetadata
parseSingleTokenMetadata = do
  token_id <-
    pure singleTokenTokenId
  symbol <-
    arg (Name @"token-symbol") <$> namedParser (Just [mt|TZBTC|])
      "Token symbol, as described in TZIP-12."
  name <-
    arg (Name @"token-name") <$> namedParser (Just [mt|Tezos BTC|])
      "Token name, as in TZIP-12."
  decimals <-
    arg (Name @"token-decimals") <$> namedParser (Just 0)
      "Number of decimals token uses, as in TZIP-12."
  extras <-
    pure mempty
  return TokenMetadata{..}
