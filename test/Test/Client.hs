{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.Client
  ( test_addressParser
  , test_paramToExpression
  , test_signatureParser
  ) where

import Data.ByteString (cons)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=), (@?))
import Tezos.Binary (encode)

import Michelson.Interpret.Unpack (UnpackError(..), dummyUnpackEnv, unpackValue')
import Michelson.Typed.Haskell.Value (IsoValue(..))
import Lorentz.Test.Integrational (genesisAddress1)
import Util.Named ((.!))

import Client.Parser (parseAddressFromOutput, parseSignatureFromOutput)
import Client.Util (paramToExpression)
import Lorentz.Contracts.TZBTC.Types (SafeParameter(..))

(@??) :: (Show a, HasCallStack) => a -> (a -> Bool) -> Assertion
(@??) val predicate =
  predicate val @?
  ("Predicate does not hold for value " <> show val)

test_paramToExpression :: TestTree
test_paramToExpression = testGroup "Test converting Parameter to Micheline expression"
  [ testCase "Pause" $
    parameterRoundTrip (Pause ()) @?=
    Right (Pause ())
  , testCase "Mint" $
    parameterRoundTrip (Mint (#to .! genesisAddress1, #value .! 10)) @?=
    Right (Mint (#to .! genesisAddress1, #value .! 10))
  , testCase "RemoveOperator" $
    parameterRoundTrip (RemoveOperator (#operator .! genesisAddress1)) @?=
    Right (RemoveOperator (#operator .! genesisAddress1))
  ]

parameterRoundTrip :: SafeParameter a -> Either UnpackError (SafeParameter a)
parameterRoundTrip = fmap fromVal . unpackValue' dummyUnpackEnv .
  cons 0x05 . encode . paramToExpression

test_signatureParser :: TestTree
test_signatureParser = testGroup "Test parsing tezos-client sign output"
  [ testCase "valid output" $
    parseSignatureFromOutput
    ("Signature: edsigtjEws8LR5dRnd2Ve3kNN1Kd9bw79bmLs3SY71Lm4oGd2owg\
    \w4SKo38ygid2NHZwbrqZTK1PpxjGUZNpnccKmCNHAPmKitD" :: Text) @?? isRight
  , testCase "valid output with some noise after singnature" $
    parseSignatureFromOutput
    ("Signature: edsigtjEws8LR5dRnd2Ve3kNN1Kd9bw79bmLs3SY71Lm4oGd2owg\
    \w4SKo38ygid2NHZwbrqZTK1PpxjGUZNpnccKmCNHAPmKitD some noise" :: Text) @?? isRight
  , testCase "invalid `Signature:` prefix" $
    parseSignatureFromOutput
    ("NotSignature: edsigtjEws8LR5dRnd2Ve3kNN1Kd9bw79bmLs3SY71Lm4oGd2owg\
    \w4SKo38ygid2NHZwbrqZTK1PpxjGUZNpnccKmCNHAPmKitD" :: Text) @?? isLeft
  , testCase "invalid signature" $
    parseSignatureFromOutput
    ("Signature: edsigtjEws8LR5dRnd2Ve3kNN1Kd9bw79bmLsnoise3SY71Lm4oGd2owg\
    \w4SKo38ygid2NHZwbrqZTK1PpxjGnoiseUZNpnccKmCNHAPmKitD" :: Text) @?? isLeft
  ]

test_addressParser :: TestTree
test_addressParser = testGroup "Test parsing tezos-client output"
  [ testCase "valid output" $
    parseAddressFromOutput
    ("Hash: tz1faCC6Fm9gorxDaa2TUmQsYXeCJyv6rFGJ\n\
     \Public Key: edpkuG18TvSJX8uPNyXetnpjDXqw92AhiTjE51y9cQ21pbGYzL3FYs" :: Text)
    @?? isRight
  , testCase "invalid address" $
    parseAddressFromOutput
    ("Hash: tz1fmda6Fm9gorxDaa2TUmQsYXeCJyv6rFGJ\n\
     \Public Key: edpkuG18TvSJX8uPNyXetnpjDXqw92AhiTjE51y9cQ21pbGYzL3FYs" :: Text)
    @?? isLeft
  , testCase "invalid address prefix" $
    parseAddressFromOutput
    ("Not really Hash: tz1faCC6Fm9gorxDaa2TUmQsYXeCJyv6rFGJ\n\
     \Public Key: edpkuG18TvSJX8uPNyXetnpjDXqw92AhiTjE51y9cQ21pbGYzL3FYs" :: Text)
    @?? isLeft
  , testCase "invalid public key" $
    parseAddressFromOutput
    ("Hash: tz1faCC6Fm9gorxDaa2TUmQsYXeCJyv6rFGJ\n\
     \Public Key: edmdaG18TvSJX8uPNyXetnpjDXqw92AhiTjE51y9cQ21pbGYzL3FYs" :: Text)
    @?? isLeft
  , testCase "invalid public key prefix" $
    parseAddressFromOutput
    ("Hash: tz1faCC6Fm9gorxDaa2TUmQsYXeCJyv6rFGJ\n\
     \Not Public Key: edpkuG18TvSJX8uPNyXetnpjDXqw92AhiTjE51y9cQ21pbGYzL3FYs" :: Text)
    @?? isLeft
  , testCase "no newline between address and public key" $
    parseAddressFromOutput
    ("Hash: tz1faCC6Fm9gorxDaa2TUmQsYXeCJyv6rFGJ \
     \Public Key: edpkuG18TvSJX8uPNyXetnpjDXqw92AhiTjE51y9cQ21pbGYzL3FYs" :: Text)
    @?? isLeft
  ]
