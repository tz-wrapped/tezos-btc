{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Main
  ( main
  ) where

import Test.Tasty (defaultMainWithIngredients)

import Cleveland.Ingredients (ourIngredients)

import Tree (tests)

main :: IO ()
main = tests >>= defaultMainWithIngredients ourIngredients
