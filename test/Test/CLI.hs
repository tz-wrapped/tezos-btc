{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.CLI
  ( test_toConfigFilled
  , test_jsonEncodingOfPartialValues
  ) where

import qualified Universum.Unsafe as Unsafe (fromJust)

import Data.Aeson (encode, decode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Tezos.Address
import Client.Types

contractAddress :: Address
contractAddress = unsafeParseAddress "KT19rTTBPeG1JAvrECgoQ8LJj1mJrN7gsdaH"

test_toConfigFilled :: TestTree
test_toConfigFilled = testGroup "Building ClientConfig from ConfigPartial works as expected"
  [ testCase "Handle values correctly with placeholders" $
    (toConfigFilled partialConfigBad) @?= Nothing
  , testCase "Handle correctly when all required values are available" $
    assertBool "Returns completed config when all required options are available"
      (isJust $ toConfigFilled partialConfigGood)
  ]
  where
    partialConfigGood :: ClientConfigPartial
    partialConfigGood = ClientConfig
      { ccNodeAddress = Available "localhost"
      , ccNodePort = Available 9000
      , ccContractAddress = Available contractAddress
      , ccMultisigAddress = Available (Just contractAddress)
      , ccUserAlias = Available "alice"
      , ccTezosClientExecutable = Available "/bin/tezos-client"
      }
    partialConfigBad :: ClientConfigPartial
    partialConfigBad = ClientConfig
      { ccNodeAddress = Available "localhost"
      , ccNodePort = Available 9000
      , ccContractAddress = Available contractAddress
      , ccMultisigAddress = Available (Just contractAddress)
      , ccUserAlias = Available "-- some text"
      , ccTezosClientExecutable = Available "/bin/tezos-client"
      }

test_jsonEncodingOfPartialValues :: TestTree
test_jsonEncodingOfPartialValues =
  testGroup "Incomplete values are replaced with placeholders in proper format"
    [ testCase "Generates placeholder values correctly" $
        assertBool "-- prefix is included" $
          (isPrefixOf "-- " $  Unsafe.fromJust $ decode $ encode partialValue )]
  where
    partialValue = Unavilable :: Partial "fieldname" (Maybe Text)
