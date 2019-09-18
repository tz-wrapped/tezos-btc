{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Parser
  ( ClientArgs(..)
  , clientArgParser
  ) where

import Options.Applicative
  (argument, auto, help, metavar, str)
import qualified Options.Applicative as Opt

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
                      (argument str $ mconcat
                       [metavar "ADDRESS_ALIAS", help "tezos-client alias"])
                      <*> filePathArgument "tezos-client executable"
                     ))
                     ("Setup client using node url, node port, contract address, user " <>
                      "address and user address alias"))

urlArgument :: String -> Opt.Parser Text
urlArgument hInfo = argument str $
  mconcat [metavar "URL", help hInfo]

intArgument :: String -> Opt.Parser Int
intArgument hInfo = argument auto $
  mconcat [metavar "PORT", help hInfo]

filePathArgument :: String -> Opt.Parser FilePath
filePathArgument hInfo = argument str $
  mconcat [metavar "FILEPATH", help hInfo]
