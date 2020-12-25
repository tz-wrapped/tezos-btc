{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Main
  ( main
  ) where

import Data.Version (showVersion)
import Named (arg)
import Options.Applicative
  (execParser, footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)

import Morley.Client.Init (mkMorleyClientEnv)
import Paths_tzbtc (version)

import Client.Env
import Client.Main
import Client.Parser
import Client.Types

main :: IO ()
main = do
  ClientArgs morleyClientConfig cmd
    (arg #userOverride -> maybeuser)
    (arg #multisigOverride -> maybemsig)
    (arg #contractOverride -> maybecontract)
    (arg #fee -> maybefees)
    dryRunFlag <- execParser programInfo
  unless dryRunFlag $ do
    let appEnv = AppEnv
          { aeConfigOverride = ConfigOverride
            { coTzbtcUser = maybeuser
            , coTzbtcMultisig = maybemsig
            , coTzbtcContract = maybecontract
            }
          , aeFees = maybefees
        }
    morleyClientEnv <- mkMorleyClientEnv morleyClientConfig
    runAppM (TzbtcClientEnv appEnv morleyClientEnv) $ mainProgram cmd
  where
    programInfo =
      info (helper <*> versionOption <*> clientArgParser) $
      mconcat
        [ fullDesc
        , progDesc
            "TZBTC - Wrapped bitcoin on tezos blockchain"
        , header "TZBTC Client"
        , footerDoc $ usageDoc
        ]
    versionOption =
      infoOption
        ("tzbtc-" <> showVersion version)
        (long "version" <> help "Show version.")
    usageDoc :: Maybe Doc
    usageDoc =
      Just $ mconcat
      [ "You can use help for specific COMMAND", linebreak
      , "EXAMPLE:", linebreak
      , "  tzbtc-client mint --help", linebreak
      , "USAGE EXAMPLE:", linebreak
      , "  tzbtc-client mint --to tz1U1h1YzBJixXmaTgpwDpZnbrYHX3fMSpvb --value 100500", linebreak
      , linebreak
      , "  This command will perform transaction insertion", linebreak
      , "  to the chain.", linebreak
      , "  Operation hash is returned as a result.", linebreak
      ]
