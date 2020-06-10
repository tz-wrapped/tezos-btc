{- SPDX-FileCopyrightText: 2020 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Main
  ( main
  ) where

import Morley.Nettest

import Nettest.TZBTC

main :: IO ()
main = do
  config <- getClientConfig (Just "TZBTC")
  runNettestViaIntegrational $ tzbtcScenario True
  runNettestClient config $ tzbtcScenario True
  runNettestTzbtcClient config $ tzbtcScenario False
