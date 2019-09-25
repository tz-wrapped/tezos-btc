{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Parser
  ( ClientArgs(..)
  , clientArgParser
  , parseSignatureFromOutput
  ) where

import Data.Char (isAlpha, isDigit)
import Fmt (pretty)
import Options.Applicative
  (argument, auto, eitherReader, help, long, metavar, option,
  showDefaultWith, str, value)
import qualified Options.Applicative as Opt
import qualified Text.Megaparsec as P
  (Parsec, customFailure, many, parse, satisfy)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (symbol)
import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent(..))

import Lorentz (ContractAddr(..), View(..))
import Tezos.Crypto (PublicKey, Signature, parsePublicKey, parseSignature)
import Tezos.Address (Address, parseAddress)
import Util.Named

import CLI.Parser
import Client.Types
import Lorentz.Contracts.TZBTC.Types

data ClientArgs
  = CmdMint MintParams (Maybe FilePath)
  | CmdBurn BurnParams (Maybe FilePath)
  | CmdTransfer TransferParams
  | CmdApprove ApproveParams
  | CmdGetAllowance (View GetAllowanceParams Natural)
  | CmdGetBalance (View GetBalanceParams Natural)
  | CmdAddOperator OperatorParams (Maybe FilePath)
  | CmdRemoveOperator OperatorParams (Maybe FilePath)
  | CmdPause (Maybe FilePath)
  | CmdUnpause (Maybe FilePath)
  | CmdSetRedeemAddress SetRedeemAddressParams (Maybe FilePath)
  | CmdTransferOwnership TransferOwnershipParams (Maybe FilePath)
  | CmdAcceptOwnership AcceptOwnershipParams
  | CmdStartMigrateTo StartMigrateToParams (Maybe FilePath)
  | CmdStartMigrateFrom StartMigrateFromParams (Maybe FilePath)
  | CmdMigrate MigrateParams
  | CmdSetupClient ClientConfig
  | CmdGetOpDescription FilePath
  | CmdGetPackageDescription FilePath
  | CmdGetBytesToSign FilePath
  | CmdAddSignature PublicKey Signature FilePath
  | CmdCallMultisig (NonEmpty FilePath)

clientArgParser :: Opt.Parser ClientArgs
clientArgParser = Opt.hsubparser $
  mintCmd <> burnCmd <> transferCmd <> approveCmd
  <> getAllowanceCmd <> getBalanceCmd <> addOperatorCmd
  <> removeOperatorCmd <> pauseCmd <> unpauseCmd
  <> setRedeemAddressCmd <> transferOwnershipCmd <> acceptOwnershipCmd
  <> startMigrateFromCmd <> startMigrateToCmd
  <> migrateCmd <> setupUserCmd <> getOpDescriptionCmd
  <> getPackageDescriptionCmd <> getBytesToSignCmd
  <> addSignatureCmd <> callMultisigCmd
  where
    multisigOption =
      Opt.optional $ Opt.strOption $ mconcat
      [ long "multisig"
      , metavar "FILEPATH"
      , help "Create package for multisig transaction and write it to the given file"
      ]
    setupUserCmd :: Opt.Mod Opt.CommandFields ClientArgs
    setupUserCmd = (mkCommandParser
                    "setupClient"
                    (CmdSetupClient <$>
                     (ClientConfig <$>
                      urlArgument "Node url" <*>
                      intArgument "Node port" <*>
                      namedAddressOption Nothing "contract-address"
                      "Contract's address" <*>
                      namedAddressOption Nothing "multisig-address" "Multisig contract address" <*>
                      namedAddressOption Nothing "user-address" "User's address" <*>
                      (option str $ mconcat
                       [ long "alias"
                       , metavar "ADDRESS_ALIAS"
                       , help "tezos-client alias"
                       ])
                      <*> tezosClientFilePathOption
                     ))
                    ("Setup client using node url, node port, contract address, \
                     \user address, user address alias and \
                     \filepath to the tezos-client executable"))
    mintCmd :: Opt.Mod Opt.CommandFields ClientArgs
    mintCmd =
      (mkCommandParser
         "mint"
         (CmdMint <$> mintParamParser <*> multisigOption)
         "Mint tokens for an account")
    burnCmd :: Opt.Mod Opt.CommandFields ClientArgs
    burnCmd =
      (mkCommandParser
         "burn"
         (CmdBurn <$> burnParamsParser <*> multisigOption)
         "Burn tokens from an account")
    transferCmd :: Opt.Mod Opt.CommandFields ClientArgs
    transferCmd =
      (mkCommandParser
         "transfer"
         (CmdTransfer <$> transferParamParser)
         "Transfer tokens from one account to another")
    approveCmd :: Opt.Mod Opt.CommandFields ClientArgs
    approveCmd =
      (mkCommandParser
         "approve"
         (CmdApprove <$> approveParamsParser)
         "Approve transfer of tokens from one account to another")
    getAllowanceCmd :: Opt.Mod Opt.CommandFields ClientArgs
    getAllowanceCmd =
      (mkCommandParser
         "getAllowance"
         (CmdGetAllowance <$> getAllowanceParamsParser)
         "Get allowance for an account")
    getBalanceCmd :: Opt.Mod Opt.CommandFields ClientArgs
    getBalanceCmd =
      (mkCommandParser
         "getBalance"
         (CmdGetBalance <$> getBalanceParamsParser)
         "Get balance for an account")
    addOperatorCmd :: Opt.Mod Opt.CommandFields ClientArgs
    addOperatorCmd =
      (mkCommandParser
         "addOperator"
         (CmdAddOperator <$> operatorParamsParser <*> multisigOption)
         "Add an operator")
    removeOperatorCmd :: Opt.Mod Opt.CommandFields ClientArgs
    removeOperatorCmd =
      (mkCommandParser
         "removeOperator"
         (CmdRemoveOperator <$> operatorParamsParser <*> multisigOption)
         "Remove an operator")
    pauseCmd :: Opt.Mod Opt.CommandFields ClientArgs
    pauseCmd =
      (mkCommandParser
         "pause"
         (CmdPause <$> multisigOption)
         "Pause the contract")
    unpauseCmd :: Opt.Mod Opt.CommandFields ClientArgs
    unpauseCmd =
      (mkCommandParser
         "unpause"
         (CmdUnpause <$> multisigOption)
         "Unpause the contract")
    setRedeemAddressCmd :: Opt.Mod Opt.CommandFields ClientArgs
    setRedeemAddressCmd =
      (mkCommandParser
         "setRedeemAddress"
         (CmdSetRedeemAddress <$> setRedeemAddressParamsParser <*> multisigOption)
         "Set redeem address")
    transferOwnershipCmd :: Opt.Mod Opt.CommandFields ClientArgs
    transferOwnershipCmd =
      (mkCommandParser
         "transferOwnership"
         (CmdTransferOwnership <$> transferOwnershipParamsParser <*> multisigOption)
         "Transfer ownership")
    acceptOwnershipCmd :: Opt.Mod Opt.CommandFields ClientArgs
    acceptOwnershipCmd =
      (mkCommandParser
         "acceptOwnership"
         (pure $ CmdAcceptOwnership ())
         "Accept ownership")
    startMigrateFromCmd :: Opt.Mod Opt.CommandFields ClientArgs
    startMigrateFromCmd =
      (mkCommandParser
         "startMigrateFrom"
         (CmdStartMigrateFrom <$> startMigrateFromParamsParser <*> multisigOption)
         "Start contract migration")
    startMigrateToCmd :: Opt.Mod Opt.CommandFields ClientArgs
    startMigrateToCmd =
      (mkCommandParser
         "startMigrateTo"
         (CmdStartMigrateTo <$> startMigrateToParamsParser <*> multisigOption)
         "Start contract migration")
    migrateCmd :: Opt.Mod Opt.CommandFields ClientArgs
    migrateCmd =
      (mkCommandParser
         "migrate"
         (pure $ CmdMigrate ())
         "Migrate contract")

    getOpDescriptionCmd :: Opt.Mod Opt.CommandFields ClientArgs
    getOpDescriptionCmd =
      mkCommandParser
      "getOpDescription"
      (CmdGetOpDescription <$> namedFilePathOption "package" "Package filepath")
      "Get operation description from given multisig package"

    getPackageDescriptionCmd :: Opt.Mod Opt.CommandFields ClientArgs
    getPackageDescriptionCmd =
      mkCommandParser
      "getPackageDescription"
      (CmdGetPackageDescription <$> namedFilePathOption "package" "Package filepath")
      "Get human-readable description for given multisig package"

    getBytesToSignCmd :: Opt.Mod Opt.CommandFields ClientArgs
    getBytesToSignCmd =
      mkCommandParser
      "getBytesToSign"
      (CmdGetBytesToSign <$> namedFilePathOption "package" "Package filepath")
      "Get bytes that need to be signed from given multisig package"

    addSignatureCmd :: Opt.Mod Opt.CommandFields ClientArgs
    addSignatureCmd =
      mkCommandParser
      "addSignature"
      (CmdAddSignature <$> publicKeyOption <*> signatureOption <*>
       namedFilePathOption "package" "Package filepath"
      )
      "Add signature assosiated with the given public key to the given package"

    callMultisigCmd :: Opt.Mod Opt.CommandFields ClientArgs
    callMultisigCmd =
      mkCommandParser
      "callMultisig"
      (CmdCallMultisig <$>
       nonEmptyParser (namedFilePathOption "package" "Package filepath")
      )
      "Call multisig contract with the given packages"

mintParamParser :: Opt.Parser MintParams
mintParamParser =
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
  (addressArgument "Source contract address")

startMigrateToParamsParser :: Opt.Parser StartMigrateToParams
startMigrateToParamsParser = #migrationManager <.!>
  (addressArgument "Manager contract address")

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

urlArgument :: String -> Opt.Parser Text
urlArgument hInfo = argument str $
  mconcat [metavar "URL", help hInfo]

signatureOption :: Opt.Parser Signature
signatureOption = option (eitherReader parseSignatureDo) $ mconcat
  [ long "signature", metavar "SIGNATURE"]

parseSignatureDo :: String -> Either String Signature
parseSignatureDo sig =
  either (Left . mappend "Failed to parse signature: " . pretty) Right $
  parseSignature $ toText sig

publicKeyOption :: Opt.Parser PublicKey
publicKeyOption = option (eitherReader parsePublicKeyDo) $ mconcat
  [ long "public-key", metavar "PUBLIC KEY"]

parsePublicKeyDo :: String -> Either String PublicKey
parsePublicKeyDo pk =
  either (Left . mappend "Failed to parse signature: " . pretty) Right $
  parsePublicKey $ toText pk

intArgument :: String -> Opt.Parser Int
intArgument hInfo = argument auto $
  mconcat [metavar "PORT", help hInfo]

tezosClientFilePathOption :: Opt.Parser FilePath
tezosClientFilePathOption = option str $
  mconcat [ long "tezos-client", metavar "FILEPATH", help "tezos-client executable"
          , value "tezos-client", showDefaultWith (<> " from $PATH")
          ]

namedFilePathOption :: String -> String -> Opt.Parser FilePath
namedFilePathOption name hInfo = option str $
  mconcat [long name, metavar "FILEPATH", help hInfo]

nonEmptyParser :: Opt.Parser a -> Opt.Parser (NonEmpty a)
nonEmptyParser p = (:|) <$> p <*> many p

-- Tezos-client sign bytes output parser
data OutputParseError = OutputParseError Text
  deriving stock (Eq, Show, Ord)

instance ShowErrorComponent OutputParseError where
  showErrorComponent (OutputParseError err) = toString $
    "Failed to parse signature: " <> err

type Parser = P.Parsec OutputParseError Text

tezosClientSignatureParser :: Parser Signature
tezosClientSignatureParser = do
  void $ symbol space "Signature:"
  rawSignature <- P.many (P.satisfy isBase58Char)
  case parseSignature (fromString rawSignature) of
    Left err -> P.customFailure $ OutputParseError $ pretty err
    Right sign -> return sign
  where
    isBase58Char :: Char -> Bool
    isBase58Char c =
      (isDigit c && c /= '0') || (isAlpha c && c /= 'O' && c /= 'I' && c /= 'l')

parseSignatureFromOutput ::
  Text -> Either (ParseErrorBundle Text OutputParseError) Signature
parseSignatureFromOutput output = P.parse tezosClientSignatureParser "" output
