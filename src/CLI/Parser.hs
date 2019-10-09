{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module CLI.Parser
  ( CmdLnArgs(..)
  , TestScenarioOptions(..)
  , HasParser(..)
  , addressArgument
  , addressOption
  , argParser
  , mkCommandParser
  , namedAddressOption
  , nullableAddressOption
  ) where

import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Data.Char (toUpper)
import Fmt (pretty)
import Named (NamedF(..), Name(..), (!))
import Options.Applicative
  (argument, auto, command, eitherReader, help, hsubparser, info, long, metavar, option, progDesc,
  showDefaultWith, str, switch, value)
import qualified Options.Applicative as Opt

import Lorentz ((:!))
import Tezos.Address (Address, parseAddress)
import Util.Named

-- | Represents the Cmd line commands with inputs/arguments.
data CmdLnArgs
  = CmdPrintInitialStorage Address Address
  | CmdPrintContract Bool (Maybe FilePath)
  | CmdPrintAgentContract Bool (Maybe FilePath)
  | CmdPrintProxyContract Bool (Maybe FilePath)
  | CmdPrintDoc (Maybe FilePath)
  | CmdParseParameter Text
  | CmdTestScenario TestScenarioOptions

data TestScenarioOptions = TestScenarioOptions
  { tsoMaster :: !Address
  , tsoOutput :: !(Maybe FilePath)
  , tsoAddresses :: ![Address]
  }

argParser :: Opt.Parser CmdLnArgs
argParser = hsubparser $
  printCmd
  <> printAgentCmd <> printProxyCmd
  <> printInitialStorageCmd <> printDoc
  <> parseParameterCmd <> testScenarioCmd
  where
    singleLineSwitch =
            switch (long "oneline" <> help "Single line output")
    printCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    printCmd =
      (mkCommandParser
        "printContract"
        (CmdPrintContract <$> singleLineSwitch <*> outputOption)
        "Print token contract")
    printAgentCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    printAgentCmd =
      (mkCommandParser
        "printAgentContract"
        (CmdPrintAgentContract <$> singleLineSwitch <*> outputOption)
        "Print migration agent contract")
    printProxyCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    printProxyCmd =
      (mkCommandParser
        "printProxyContract"
        (CmdPrintProxyContract <$> singleLineSwitch <*> outputOption)
        "Print proxy contract")
    printInitialStorageCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    printInitialStorageCmd =
      (mkCommandParser
         "printInitialStorage"
         (CmdPrintInitialStorage
            <$> namedAddressOption Nothing "admin-address" "Administrator's address"
            <*> namedAddressOption Nothing "redeem-address" "Redeem address")
         "Print initial contract storage with the given administrator and \
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
          (CmdTestScenario <$> testScenarioOptions)
          "Print parameters for smoke tests")

mkCommandParser
  :: String
  -> Opt.Parser a
  -> String
  -> Opt.Mod Opt.CommandFields a
mkCommandParser commandName parser desc =
  command commandName $ info parser $ progDesc desc

-- Maybe add default value and make sure it will be shown in help message.
maybeAddDefault :: Opt.HasValue f => (a -> String) -> Maybe a -> Opt.Mod f a
maybeAddDefault printer = maybe mempty addDefault
  where
    addDefault v = value v <> showDefaultWith printer

-- The following, HasReader/HasParser typeclasses are used to generate
-- parsers for a named fields with options name and metavars derived from
-- the name of the field itself.
--
-- | Supporting typeclass for HasParser.
class HasReader a where
  getReader :: Opt.ReadM a

instance HasReader Natural where
  getReader = auto

instance HasReader Int where
  getReader = auto

instance HasReader Text where
  getReader = str

instance HasReader Address where
  getReader = eitherReader parseAddrDo

-- | Typeclass used to define general instance for named fields
class HasParser a where
  getParser :: String -> Opt.Parser a

instance
  (HasReader a, KnownSymbol name) =>
    HasParser ((name :: Symbol) :! a)  where
  getParser hInfo =
    let
      name = (symbolVal (Proxy @name))
    in option ((Name @name) <.!> getReader) $
         mconcat [ long name , metavar (toUpper <$> name), help hInfo ]

testScenarioOptions :: Opt.Parser TestScenarioOptions
testScenarioOptions = TestScenarioOptions <$>
  addressArgument "Owner's address" <*>
  outputOption <*>
  (many $ addressOption Nothing "Other owned addresses")

addressOption :: Maybe Address -> String -> Opt.Parser Address
addressOption defAddress hInfo =
  option (eitherReader parseAddrDo) $
  mconcat
    [ metavar "ADDRESS"
    , long "address"
    , help hInfo
    , maybeAddDefault pretty defAddress
    ]

namedAddressOption :: Maybe Address -> String -> String -> Opt.Parser Address
namedAddressOption defAddress name hInfo = option (eitherReader parseAddrDo) $
  mconcat
    [ metavar "ADDRESS"
    , long name
    , help hInfo
    , maybeAddDefault pretty defAddress
    ]

-- A nullable option that will accept an explicit null value
nullableOption
  :: ("name" :! String)
  -> ("meta" :! String)
  -> ("hinfo" :! String)
  -> (String -> Either String a) -> Opt.Parser (Maybe a)
nullableOption (Arg name) (Arg meta) (Arg hinfo) e =
  option (eitherReader (parseNullable e)) $
  mconcat [long name, metavar meta, help hinfo]
  where
    parseNullable :: (String -> Either String a) -> String -> Either String (Maybe a)
    parseNullable _ "null" = Right Nothing
    parseNullable fn a = case fn a of
      Right x -> Right (Just x)
      Left x -> Left x

nullableAddressOption :: ("name" :! String) -> ("hinfo" :! String) -> Opt.Parser (Maybe Address)
nullableAddressOption name hinfo = (nullableOption name ! #meta "ADDRESS") hinfo parseAddrDo

addressArgument :: String -> Opt.Parser Address
addressArgument hInfo =
  argument (eitherReader parseAddrDo) $
  mconcat
    [ metavar "ADDRESS", help hInfo
    ]

outputOption :: Opt.Parser (Maybe FilePath)
outputOption = Opt.optional $ Opt.strOption $ mconcat
  [ Opt.short 'o'
  , Opt.long "output"
  , Opt.metavar "FILEPATH"
  , Opt.help "Output file"
  ]

parseAddrDo :: String -> Either String Address
parseAddrDo addr =
  either (Left . mappend "Failed to parse address: " . pretty) Right $
  parseAddress $ toText addr
