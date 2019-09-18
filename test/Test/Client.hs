{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.Client
  ( test_paramToExpression
  ) where

import Data.ByteString (cons)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Tezos.Binary (encode)

import Michelson.Interpret.Unpack (UnpackError(..), dummyUnpackEnv, unpackValue')
import Michelson.Typed.Haskell.Value (IsoValue(..))
import Lorentz.Test.Integrational (genesisAddress1)
import Util.Named ((.!))

import Client.Util (paramToExpression)
import Lorentz.Contracts.TZBTC (Parameter(..))

test_paramToExpression :: TestTree
test_paramToExpression = testGroup "Test converting Parameter to Micheline expression"
  [ testCase "Pause" $
    parameterRoundTrip (Pause ()) @?= Right (Pause ())
  , testCase "Mint" $
    parameterRoundTrip (Mint (#to .! genesisAddress1, #value .! 10)) @?=
    Right (Mint (#to .! genesisAddress1, #value .! 10))
  , testCase "RemoveOperator" $
    parameterRoundTrip (RemoveOperator (#operator .! genesisAddress1)) @?=
    Right (RemoveOperator (#operator .! genesisAddress1))
  ]

parameterRoundTrip :: Parameter -> Either UnpackError Parameter
parameterRoundTrip = fmap fromVal . unpackValue' dummyUnpackEnv .
  cons 0x05 . encode . paramToExpression
