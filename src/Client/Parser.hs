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

import Tezos.Crypto (PublicKey, Signature, parsePublicKey, parseSignature)

import CLI.Parser
import Client.Types

data ClientArgs
  = CmdTransaction CmdLnArgs
  | CmdSetupClient ClientConfig
  | CmdGetOpDescription FilePath
  | CmdGetPackageDescription FilePath
  | CmdGetBytesToSign FilePath
  | CmdAddSignature PublicKey Signature FilePath
  | CmdCallMultisig (NonEmpty FilePath) [PublicKey]

clientArgParser :: Opt.Parser ClientArgs
clientArgParser =
  (Opt.hsubparser $
   setupUserCmd <> getOpDescriptionCmd <> getPackageDescriptionCmd <>
   getBytesToSignCmd <> addSignatureCmd <> callMultisigCmd
  ) <|> CmdTransaction <$> argParser
  where
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
       nonEmptyParser (namedFilePathOption "package" "Package filepath") <*>
       many publicKeyOption)
      "Call multisig contract with the given packages"

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
