{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Main
  ( main
  ) where

import Cleveland.Ingredients (ourIngredients)
import Morley.Nettest.Tasty (nettestMainWithIngredients)

import Tree (tests)

main :: IO ()
main = tests >>= nettestMainWithIngredients ourIngredients
