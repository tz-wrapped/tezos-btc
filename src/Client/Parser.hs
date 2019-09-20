{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Parser
  ( ClientArgs(..)
  , clientArgParser
  ) where

import Options.Applicative
  (argument, auto, help, long, metavar, option, str)
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
                      namedAddressOption Nothing "contract-address"
                      "Contract's address" <*>
                      namedAddressOption Nothing "user-address" "User's address" <*>
                      (option str $ mconcat
                       [ long "alias"
                       , metavar "ADDRESS_ALIAS"
                       , help "tezos-client alias"
                       ])
                      <*> namedFilePathOption "tezos-client" "tezos-client executable"
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

namedFilePathOption :: String -> String -> Opt.Parser FilePath
namedFilePathOption name hInfo = option str $
  mconcat [long name, metavar "FILEPATH", help hInfo]
