{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}

{-# LANGUAGE NoImplicitPrelude #-}

module Test.Dummy
  ( test_dummy
  ) where

import Universum

import Test.HUnit ((@?=))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)

import Lib

{-# ANN test_dummy ("HLint: ignore Evaluate" :: Text) #-}

test_dummy :: [TestTree]
test_dummy = [ testCase "always true" $ do
    (2 :: Integer) + (2 :: Integer) @?= (4 :: Integer)
    assertBool "the list is empty" $ null []
    someFunc
    ("foo" :: String) @?= ("foo" :: String)]
