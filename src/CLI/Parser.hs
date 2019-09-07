{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module CLI.Parser
  ( CmdLnArgs(..)
  , TestScenarioOptions(..)
  , argParser
  ) where

import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Data.Char (toUpper)
import Fmt (pretty)
import Named (Name(..))
import Options.Applicative
  (argument, auto, command, eitherReader, help, hsubparser, info, long, metavar, option, progDesc,
  showDefaultWith, switch, value)
import qualified Options.Applicative as Opt

import Lorentz ((:!), ContractAddr(..), View(..))
import Lorentz.Contracts.TZBTC.Types
import Tezos.Address
import Util.Named

-- | Represents the Cmd line commands with inputs/arguments.
data CmdLnArgs
  = CmdMint MintParams
  | CmdMintForMigrations MintForMigrationParams
  | CmdBurn BurnParams
  | CmdTransfer TransferParams
  | CmdApprove ApproveParams
  | CmdGetAllowance (View GetAllowanceParams Natural)
  | CmdGetBalance (View GetBalanceParams Natural)
  | CmdAddOperator OperatorParams
  | CmdRemoveOperator OperatorParams
  | CmdPause
  | CmdUnpause
  | CmdSetRedeemAddress SetRedeemAddressParams
  | CmdTransferOwnership TransferOwnershipParams
  | CmdAcceptOwnership AcceptOwnershipParams
  | CmdStartMigrateTo StartMigrateToParams
  | CmdStartMigrateFrom StartMigrateFromParams
  | CmdMigrate MigrateParams
  | CmdPrintInitialStorage Address Address
  | CmdPrintContract Bool (Maybe FilePath)
  | CmdPrintAgentContract Bool (Maybe FilePath)
  | CmdParseParameter Text
  | CmdTestScenario TestScenarioOptions
  | CmdTest

data TestScenarioOptions = TestScenarioOptions
  { tsoMaster :: !Address
  , tsoOutput :: !(Maybe FilePath)
  , tsoAddresses :: ![Address]
  }

argParser :: Opt.Parser CmdLnArgs
argParser = hsubparser $
  mintCmd <> mintForMigrationsCmd <> burnCmd <> transferCmd <> approveCmd
  <> getAllowanceCmd <> getBalanceCmd <> addOperatorCmd
  <> removeOperatorCmd <> pauseCmd <> unpauseCmd
  <> setRedeemAddressCmd <> transferOwnershipCmd
  <> startMigrateFromCmd <> startMigrateToCmd
  <> migrateCmd <> printCmd
  <> printAgentCmd <> printInitialStorageCmd
  <> parseParameterCmd <> testScenarioCmd <> testCmd
  where
    mkCommandParser ::
         String
      -> Opt.Parser CmdLnArgs
      -> String
      -> Opt.Mod Opt.CommandFields CmdLnArgs
    mkCommandParser commandName parser desc =
      command commandName $ info parser $ progDesc desc
    printCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    printCmd =
      let singleLineSwitch =
            switch (long "oneline" <> help "Single line output")
       in (mkCommandParser
             "printContract"
             (CmdPrintContract <$> singleLineSwitch <*> outputOption)
             "Print token contract")
    printAgentCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    printAgentCmd =
      let singleLineSwitch =
            switch (long "oneline" <> help "Single line output")
       in (mkCommandParser
             "printAgentContract"
             (CmdPrintAgentContract <$> singleLineSwitch <*> outputOption)
             "Print migration agent contract")
    printInitialStorageCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    printInitialStorageCmd =
      (mkCommandParser
         "printInitialStorage"
         (CmdPrintInitialStorage
            <$> addressArgument "Administrator's address"
            <*> addressArgument "Redeem address")
         "Print initial contract storage")
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
    testCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    testCmd =
      (mkCommandParser
          "test"
          (pure CmdTest)
          "Print parameters for smoke tests")
    mintCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    mintCmd =
      (mkCommandParser
         "mint"
         (CmdMint <$> mintParamParser)
         "Mint tokens for an account")
    mintForMigrationsCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    mintForMigrationsCmd =
      (mkCommandParser
         "mintForMigrations"
         (CmdMintForMigrations <$> mintForMigrationsParamParser)
         "Mint tokens for an account during migration")
    burnCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    burnCmd =
      (mkCommandParser
         "burn"
         (CmdBurn <$> burnParamsParser)
         "Burn tokens from an account")
    transferCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    transferCmd =
      (mkCommandParser
         "transfer"
         (CmdTransfer <$> transferParamParser)
         "Transfer tokens from one account to another")
    approveCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    approveCmd =
      (mkCommandParser
         "approve"
         (CmdApprove <$> approveParamsParser)
         "Approve transfer of tokens from one account to another")
    getAllowanceCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    getAllowanceCmd =
      (mkCommandParser
         "getAllowance"
         (CmdGetAllowance <$> getAllowanceParamsParser)
         "Get allowance for an account")
    getBalanceCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    getBalanceCmd =
      (mkCommandParser
         "getBalance"
         (CmdGetBalance <$> getBalanceParamsParser)
         "Get balance for an account")
    addOperatorCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    addOperatorCmd =
      (mkCommandParser
         "addOperator"
         (CmdAddOperator <$> operatorParamsParser)
         "Add an operator")
    removeOperatorCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    removeOperatorCmd =
      (mkCommandParser
         "removeOperator"
         (CmdRemoveOperator <$> operatorParamsParser)
         "Remove an operator")
    pauseCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    pauseCmd =
      (mkCommandParser
         "pause"
         (pure CmdPause)
         "Pause the contract")
    unpauseCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    unpauseCmd =
      (mkCommandParser
         "unpause"
         (pure CmdUnpause)
         "Unpause the contract")
    setRedeemAddressCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    setRedeemAddressCmd =
      (mkCommandParser
         "setRedeemAddress"
         (CmdSetRedeemAddress <$> setRedeemAddressParamsParser)
         "Set redeem address")
    transferOwnershipCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    transferOwnershipCmd =
      (mkCommandParser
         "transferOwnership"
         (CmdTransferOwnership <$> transferOwnershipParamsParser)
         "Transfer ownership")
    startMigrateFromCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    startMigrateFromCmd =
      (mkCommandParser
         "startMigrateFrom"
         (CmdStartMigrateFrom <$> startMigrateFromParamsParser)
         "Start contract migration")
    startMigrateToCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    startMigrateToCmd =
      (mkCommandParser
         "startMigrateTo"
         (CmdStartMigrateTo <$> startMigrateToParamsParser)
         "Start contract migration")
    migrateCmd :: Opt.Mod Opt.CommandFields CmdLnArgs
    migrateCmd =
      (mkCommandParser
         "migrate"
         (pure $ CmdMigrate ())
         "Migrate contract")

mintParamParser :: Opt.Parser MintParams
mintParamParser =
  (,) <$> (getParser "Address to mint to")
       <*> (getParser "Amount to mint")

mintForMigrationsParamParser :: Opt.Parser MintParams
mintForMigrationsParamParser =
  (,) <$> (getParser "Address to mint to")
       <*> (getParser "Amount to mint")

burnParamsParser :: Opt.Parser BurnParams
burnParamsParser = getParser "Amount to burn"

approveParamsParser :: Opt.Parser ApproveParams
approveParamsParser =
  (,) <$> (getParser "Address of the spender")
       <*> (getParser "Amount to approve")

transferParamParser :: Opt.Parser TransferParams
transferParamParser =
  (,,) <$> (getParser "Address to transfer from")
       <*> (getParser "Address to transfer to")
       <*> (getParser "Amount to transfer")

getAllowanceParamsParser :: Opt.Parser (View GetAllowanceParams Natural)
getAllowanceParamsParser = let
  iParam =
    (,) <$> (getParser "Address of the owner")
        <*> (getParser "Address of spender")
  contractParam = callBackAddressOption
  in View <$> iParam <*> contractParam

getBalanceParamsParser :: Opt.Parser (View GetBalanceParams Natural)
getBalanceParamsParser = let
  iParam = addressOption Nothing "Address of the owner"
  in View <$> iParam <*> callBackAddressOption

operatorParamsParser :: Opt.Parser OperatorParams
operatorParamsParser = getParser "Address of the operator"

setRedeemAddressParamsParser :: Opt.Parser SetRedeemAddressParams
setRedeemAddressParamsParser = #redeem <.!> addressArgument "Redeem address"

transferOwnershipParamsParser :: Opt.Parser TransferOwnershipParams
transferOwnershipParamsParser = #newOwner
  <.!> addressArgument "Address of the new owner"

startMigrateFromParamsParser :: Opt.Parser StartMigrateFromParams
startMigrateFromParamsParser = #migrationManager <.!>
  (ContractAddr <$> addressArgument "Source contract address")

startMigrateToParamsParser :: Opt.Parser StartMigrateToParams
startMigrateToParamsParser = #migrationManager <.!>
  (ContractAddr <$> addressArgument "Manager contract address")

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


callBackAddressOption :: Opt.Parser (ContractAddr a)
callBackAddressOption = ContractAddr <$> caddr
  where
    caddr = option (eitherReader parseAddrDo) $
      mconcat
        [ metavar "CALLBACK-ADDRESS"
        , long "callback"
        , help "Callback address"
        ]

parseAddrDo :: String -> Either String Address
parseAddrDo addr =
  either (Left . mappend "Failed to parse address: " . pretty) Right $
  parseAddress $ toText addr
