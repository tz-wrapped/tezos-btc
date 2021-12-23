{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Main
  ( main
  ) where

import Test.Cleveland.Ingredients (ourIngredients)
import Test.Cleveland.Tasty (clevelandMainWithIngredients)

import Tree (tests)

main :: IO ()
main = tests >>= clevelandMainWithIngredients ourIngredients
