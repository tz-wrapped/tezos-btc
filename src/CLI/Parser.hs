{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

{-# LANGUAGE ApplicativeDo #-}

module CLI.Parser
  ( CmdLnArgs (..)
  , VersionArg (..)
  , MigrationArgs (..)
  , argParser
  , parseSingleTokenMetadata
  ) where

import Fmt (Buildable, pretty)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Options.Applicative (ReadM, help, hsubparser, long, metavar, switch)
import Options.Applicative qualified as Opt

import Lorentz.Contracts.Metadata
import Morley.CLI
import Morley.Michelson.Text
import Morley.Tezos.Address
import Morley.Util.CLI
import Morley.Util.Named

import CLI.L1AddressOrAlias ()

-- | Represents the Cmd line commands with inputs/arguments.
data CmdLnArgs
  = CmdPrintInitialStorage L1Address
  | CmdPrintContract Bool (Maybe FilePath)
  | CmdPrintMultisigContract Bool Bool (Maybe FilePath)
  | CmdPrintDoc VersionArg (Maybe FilePath)
  | CmdParseParameter VersionArg Text
  | CmdMigrate ("output" :! Maybe FilePath) MigrationArgs

data VersionArg
  = V0
  | V1
  | V2
  deriving stock (Show)

data MigrationArgs
  = MigrateV1
      ("redeemAddress" :! L1Address)
      ("tokenMetadata" :! TokenMetadata)
  | MigrateV2
      ("redeemAddress" :! L1Address)
      ("tokenMetadata" :! TokenMetadata)
  | MigrateV2FromV1

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
        "Print token contract (V0 version)")
    printMultisigCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    printMultisigCmd =
      (mkCommandParser
        "printMultisigContract"
        (CmdPrintMultisigContract <$> singleLineSwitch <*> customErrorsFlag <*> outputOption)
        "Print multisig contract")
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
            <$> mkCLOptionParser Nothing
            (#name :! "owner-address") (#help :! "Owner's address")
         )
         "Print initial contract storage with the given owner and \
         \redeem addresses")
    printDoc :: Opt.Mod Opt.CommandFields CmdLnArgs
    printDoc =
      (mkCommandParser
        "printContractDoc"
        (CmdPrintDoc <$> versionOption <*> outputOption)
        "Print tzbtc contract documentation")
    parseParameterCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    parseParameterCmd =
      (mkCommandParser
          "parseContractParameter"
          (CmdParseParameter <$> versionOption <*> Opt.strArgument mempty)
          "Parse contract parameter to Lorentz representation")
    migrateCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    migrateCmd =
      mkCommandParser
          "migrate"
          (CmdMigrate
            <$> (#output <:!> outputOption)
            <*> (hsubparser
                  (mconcat
                    [ migrateV1Cmd
                    , migrateV2Cmd
                    , migrateV2FromV1Cmd
                    ])
                  <|> migrateV1Parser
                )
          )
          "Print migration scripts. When version is unspecified, v1 is used."
    migrateV1Parser =
      MigrateV1
          <$> biNamedParser @_ @L1Address Nothing "Redeem address"
          <*> fmap (#tokenMetadata :!) parseSingleTokenMetadata
    migrateV1Cmd :: Opt.Mod Opt.CommandFields MigrationArgs
    migrateV1Cmd =
      mkCommandParser
        "v1"
        (MigrateV1
          <$> biNamedParser @_ @L1Address Nothing "Redeem address"
          <*> fmap (#tokenMetadata :!) parseSingleTokenMetadata
        )
        "Migration from V0 to V1."
    migrateV2Cmd :: Opt.Mod Opt.CommandFields MigrationArgs
    migrateV2Cmd =
      mkCommandParser
        "v2"
        (MigrateV2
          <$> biNamedParser @_ @L1Address Nothing "Redeem address"
          <*> fmap (#tokenMetadata :!) parseSingleTokenMetadata
        )
        "Migration from V0 to V2."
    migrateV2FromV1Cmd :: Opt.Mod Opt.CommandFields MigrationArgs
    migrateV2FromV1Cmd =
      mkCommandParser
        "v1-to-v2"
        (pure MigrateV2FromV1)
        "Migration from V1 to V2."
    versionOption =
      Opt.option versionReadM
        (long "version" <>
         help "Contract version." <>
         metavar "NUMBER" <>
         Opt.value V1 <>
         Opt.showDefaultWith (\_ -> "1"))

-- | For value named in camelCase, parse the respective CLI option
-- if provided in camelCase (legacy) or in spinal-case.
biNamedParser
  :: forall name a. (Buildable a, HasCLReader a, KnownSymbol name)
  => Maybe a -> String -> Opt.Parser (name :! a)
biNamedParser defValue hInfo = asum
  [ namedParser defValue hInfo
  , Opt.option (fromLabel @name <:!> getReader) $
      mconcat
      [ long name
      , metavar (getMetavar @a)
      , help (hInfo <> " " <> deprecationNote)
      , maybeAddDefault pretty (fromLabel @name <:!> defValue)
      ]
  ]
  where
    name = symbolVal (Proxy @name)
    deprecationNote = "(this option is deprecated, use spinal-case form instead)"

versionReadM :: ReadM VersionArg
versionReadM = eitherReader $ \case
  "0" -> pure V0
  "1" -> pure V1
  "2" -> pure V2
  other -> Left $ "Unknown version identifier " <> pretty other

-- | Parse `TokenMetadata` for a single token, with no extras
parseSingleTokenMetadata :: Opt.Parser TokenMetadata
parseSingleTokenMetadata = do
  let tmTokenId = singleTokenTokenId
      tmExtras = mempty
  tmSymbol <-
    arg (fromLabel @"token-symbol") <$> namedParser (Just [mt|TZBTC|])
      "Token symbol, as described in TZIP-12."
  tmName <-
    arg (fromLabel @"token-name") <$> namedParser (Just [mt|Tezos BTC|])
      "Token name, as in TZIP-12."
  tmDecimals <-
    arg (fromLabel @"token-decimals") <$> namedParser (Just 0)
      "Number of decimals token uses, as in TZIP-12."
  return TokenMetadata{..}
