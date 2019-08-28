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

import Tezos.Crypto (SecretKey, parseSecretKey)

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
                      addressArgument "Contract's address" <*>
                      addressArgument "User's address" <*>
                      secretKeyArgument "User's secret key"
                     ))
                     ("Setup client using node url, node port, contract address, user " <>
                      "address user secret key"))

secretKeyArgument :: String -> Opt.Parser SecretKey
secretKeyArgument hInfo =
  argument (eitherReader parseSecretKeyDo) $ mconcat
  [ metavar "SECRET-KEY", help hInfo]

parseSecretKeyDo :: String -> Either String SecretKey
parseSecretKeyDo sk =
  either (Left . mappend "Failed to parse secret-key: " . pretty) Right $
  parseSecretKey $ toText sk

urlArgument :: String -> Opt.Parser Text
urlArgument hInfo = argument str $
  mconcat [metavar "URL", help hInfo]

intArgument :: String -> Opt.Parser Int
intArgument hInfo = argument auto $
  mconcat [metavar "PORT", help hInfo]
