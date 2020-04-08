{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Main
  ( main
  ) where

import Client.Main

main :: IO ()
main = do
  env <- mkInitEnv
  runAppM env mainProgram
