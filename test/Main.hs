{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Main
  ( main
  ) where

import Test.Tasty (defaultMain)
import Tree (tests)

main :: IO ()
main = tests >>= defaultMain
