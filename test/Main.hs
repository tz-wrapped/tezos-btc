{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Main
  ( main
  ) where

import Test.Tasty (defaultMain)
import Tree (tests)

main :: IO ()
main = tests >>= defaultMain
