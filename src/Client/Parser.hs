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
  (argument, auto, help, long, metavar, option, showDefaultWith, str, value)
import qualified Options.Applicative as Opt
import qualified Text.Megaparsec as P
  (Parsec, customFailure, many, parse, satisfy)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (symbol)
import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent(..))

import Tezos.Crypto (Signature, parseSignature)

import CLI.Parser
import Client.Types

data ClientArgs
  = CmdTransaction CmdLnArgs
  | CmdSetupClient ClientConfig

clientArgParser :: Opt.Parser ClientArgs
clientArgParser =
  (Opt.hsubparser setupUserCmd) <|>
  CmdTransaction <$> argParser
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

urlArgument :: String -> Opt.Parser Text
urlArgument hInfo = argument str $
  mconcat [metavar "URL", help hInfo]

intArgument :: String -> Opt.Parser Int
intArgument hInfo = argument auto $
  mconcat [metavar "PORT", help hInfo]

tezosClientFilePathOption :: Opt.Parser FilePath
tezosClientFilePathOption = option str $
  mconcat [ long "tezos-client", metavar "FILEPATH", help "tezos-client executable"
          , value "tezos-client", showDefaultWith (<> " from $PATH")
          ]

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
