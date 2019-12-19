{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.Client
  ( test_addressParser
  , test_convertTypeToExpression
  , test_nicePackedValueToExpression
  , test_signatureParser
  ) where

import Data.ByteString (cons)
import Data.Sequence (fromList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?), (@?=))
import Tezos.Common.Binary (encode)
import Tezos.V005.Micheline
  (Annotation(..), Expression(..), MichelinePrimAp(..), MichelinePrimitive(..))

import Lorentz.Test.Integrational (genesisAddress1)
import Michelson.Interpret.Unpack (UnpackError(..), unpackValue')
import Michelson.Typed.Haskell.Value (IsoValue(..))
import Michelson.Untyped.Annotation (ann, noAnn)
import Michelson.Untyped.Type (CT(..), T(..), Type(..))
import Util.Named ((.!))

import Client.Parser (parseAddressFromOutput, parseSignatureFromOutput)
import Client.Util (convertTypeToExpression, nicePackedValueToExpression, typeToExpression)
import Lorentz.Contracts.TZBTC.Preprocess (upgradeParameters)
import Lorentz.Contracts.TZBTC.Types
import Lorentz.Contracts.TZBTC.V0 (TZBTCv0)

import Test.TZBTC (dummyOriginationParameters)

(@??) :: (Show a, HasCallStack) => a -> (a -> Bool) -> Assertion
(@??) val predicate =
  predicate val @?
  ("Predicate does not hold for value " <> show val)

test_nicePackedValueToExpression :: TestTree
test_nicePackedValueToExpression = testGroup "Test converting Parameter to Micheline expression"
  [ testCase "Pause" $
    parameterRoundTrip (Pause ()) @?=
    Right (Pause ())
  , testCase "Mint" $
    parameterRoundTrip (Mint (#to .! genesisAddress1, #value .! 10)) @?=
    Right (Mint (#to .! genesisAddress1, #value .! 10))
  , testCase "RemoveOperator" $
    parameterRoundTrip (RemoveOperator (#operator .! genesisAddress1)) @?=
    Right (RemoveOperator (#operator .! genesisAddress1))
  , testCase "Upgrade" $
    parameterRoundTrip upgradeParam @?= Right upgradeParam
  ]
  where
    upgradeParam =
      let
        ownerAddr = genesisAddress1
        origParams = dummyOriginationParameters ownerAddr ownerAddr mempty
      in
        Upgrade @TZBTCv0 $ upgradeParameters origParams

parameterRoundTrip
  :: SafeParameter TZBTCv0
  -> Either UnpackError (SafeParameter TZBTCv0)
parameterRoundTrip = fmap fromVal . unpackValue' .
  cons 0x05 . encode . nicePackedValueToExpression

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

test_convertTypeToExpression :: TestTree
test_convertTypeToExpression = testGroup "Test converting Untyped.Type to Expression"
  [ testCase "Same JSON for Haskell type and Untyped.Type without annotations" $
    typeToExpression @(Either Integer ((), Bool)) @?=
    convertTypeToExpression
    (Type (TOr noAnn noAnn
           (Type (Tc CInt) noAnn)
           (Type (TPair noAnn noAnn
                   (Type TUnit noAnn)
                   (Type (Tc CBool) noAnn)
                  ) noAnn)
         ) noAnn
    )
  , testCase "Annotations on types are preserved in the JSON" $
    convertTypeToExpression (Type (TOr (ann "kek") (ann "bek")
                                   (Type (Tc CInt) noAnn)
                                   (Type (Tc CNat) noAnn)
                                  ) (ann "lol")) @?=
    Expression_Prim (
      MichelinePrimAp
        (MichelinePrimitive "or")
        (fromList [ Expression_Prim
                    (MichelinePrimAp (MichelinePrimitive "int")
                     (fromList []) (fromList [Annotation_Field "kek"])
                    )
                  , Expression_Prim
                    (MichelinePrimAp (MichelinePrimitive "nat")
                     (fromList []) (fromList [Annotation_Field "bek"]))])
        (fromList [Annotation_Type "lol"])
      )
  ]
