{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Parser
  ( ClientArgs(..)
  , clientArgParser
  ) where

import Fmt (pretty)
import Options.Applicative
  (argument, auto, eitherReader, help, metavar, str)
import qualified Options.Applicative as Opt

import Tezos.Crypto (Signature, parseSignature)

import CLI.Parser
import Client.Types

data ClientArgs
  = CmdTransaction CmdLnArgs
  | CmdSetupClient ClientConfigPartial
  | CmdInjectOperation Text Signature

clientArgParser :: Opt.Parser ClientArgs
clientArgParser =
  (Opt.hsubparser $ setupUserCmd <> injectOperationCmd) <|>
  CmdTransaction <$> argParser
  where
    setupUserCmd :: Opt.Mod Opt.CommandFields ClientArgs
    setupUserCmd = (mkCommandParser
                    "setupClient"
                    (CmdSetupClient <$>
                     (ClientConfig <$>
                      (partialParser $ urlArgument "Node url") <*>
                      (partialParser $ intArgument "Node port") <*>
                      (partialParser $ addressArgument "Contract's address") <*>
                      (partialParser $ addressArgument "User's address") <*>
                      (partialParser $ argument str $ mconcat
                       [metavar "ADDRESS_ALIAS", help "tezos-client alias"])
                      <*> (partialParser $ filePathArgument "TZBTC-client executable")
                      <*> (partialParser $ filePathArgument "tezos-client executable")
                     ))
                     ("Setup client using node url, node port, contract address, user " <>
                      "address and user address alias"))
    injectOperationCmd :: Opt.Mod Opt.CommandFields ClientArgs
    injectOperationCmd = (mkCommandParser
                          "injectOperation"
                          (CmdInjectOperation <$>
                           unsignedOperation <*>
                           signatureArgument)
                          ("Inject given unsigned operation by signing it with given signature"))

urlArgument :: String -> Opt.Parser Text
urlArgument hInfo = argument str $
  mconcat [metavar "URL", help hInfo]

unsignedOperation :: Opt.Parser Text
unsignedOperation = argument str $
  mconcat [metavar "UNSIGNED_OPERATION"]

signatureArgument :: Opt.Parser Signature
signatureArgument = argument (eitherReader parseSignatureDo) $ mconcat
  [ metavar "SIGNATURE"]

parseSignatureDo :: String -> Either String Signature
parseSignatureDo sig =
  either (Left . mappend "Failed to parse signature: " . pretty) Right $
  parseSignature $ toText sig

intArgument :: String -> Opt.Parser Int
intArgument hInfo = argument auto $
  mconcat [metavar "PORT", help hInfo]

filePathArgument :: String -> Opt.Parser FilePath
filePathArgument hInfo = argument str $
  mconcat [metavar "FILEPATH", help hInfo]
