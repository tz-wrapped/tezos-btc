{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Test.Client
  ( test_addressParser
  , test_nicePackedValueToExpression
  , test_signatureParser
  , test_tezosClientFloatingPointRoundTrip
  ) where

import Control.Lens (_Just)
import Data.ByteString (cons)
import Numeric (showFFloat)
import Test.HUnit (Assertion, (@?), (@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Positive(..), testProperty)

import Lorentz.Base (mapLorentzInstr, optimizeLorentz)
import Lorentz.Constraints (NiceFullPackedValue, nicePackedValueEvi, niceUnpackedValueEvi, withDict)
import Lorentz.Test.Integrational (genesisAddress1)
import Michelson.Interpret.Unpack (UnpackError(..), unpackValue')
import Michelson.Typed.Haskell.Value (IsoValue(..))
import Morley.Micheline (encodeExpression)
import Util.Named (namedL, (.!))

import Client.IO.TezosClient (toTezString)
import Client.Parser
  (parseAddressFromOutput, parseBurncapErrorFromOutput, parseSignatureFromOutput,
  parseSimulationResultFromOutput, toMuTez)
import Client.Types (SimulationResult(..))
import Client.Util (nicePackedValueToExpression)
import Lorentz.Contracts.TZBTC.Preprocess (upgradeParameters)
import Lorentz.Contracts.TZBTC.Types
import Lorentz.Contracts.TZBTC.V0 (TZBTCv0)

import Test.TZBTC (dummyV1Parameters)

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
    optimizeUpgradeParams <$> valueRoundTrip upgradeParam @?=
    Right upgradeParam
  ]
  where
    upgradeParam =
      let
        ownerAddr = genesisAddress1
        origParams = dummyV1Parameters ownerAddr mempty
      in
        upgradeParameters origParams

    optimizeUpgradeParams
      :: OneShotUpgradeParameters TZBTCv0
      -> OneShotUpgradeParameters TZBTCv0
    optimizeUpgradeParams =
      _5 . namedL #newPermCode . _Just %~ mapLorentzInstr optimizeLorentz

test_tezosClientFloatingPointRoundTrip :: TestTree
test_tezosClientFloatingPointRoundTrip =
  testGroup "Test floating point values parsed from tezos-clinet output retains precision"
    [ testProperty "Burn-fee retains precision" burnFeeRoundTrip
    , testProperty "Baker-fee retains precision" bakerFeeRoundTrip
    ]

parameterRoundTrip
  :: SafeParameter TZBTCv0
  -> Either UnpackError (SafeParameter TZBTCv0)
parameterRoundTrip = valueRoundTrip

valueRoundTrip
  :: forall a.
     (NiceFullPackedValue a)
  => a -> Either UnpackError a
valueRoundTrip =
  withDict (nicePackedValueEvi @a) $
  withDict (niceUnpackedValueEvi @a) $
    fmap fromVal . unpackValue' .
    cons 0x05 . encodeExpression . nicePackedValueToExpression

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

burnFeeRoundTrip :: Positive Double -> Bool
burnFeeRoundTrip d = let
  inpString = showFFloat (Just 3) (getPositive d) ""
  in case parseBurncapErrorFromOutput ("The operation will burn " <> (toText inpString)) of
    Right bc -> (toTezString $ toMuTez bc) == inpString
    Left _ -> False

bakerFeeRoundTrip :: Positive Double -> Bool
bakerFeeRoundTrip d = let
  inpString = showFFloat (Just 3) (getPositive d) ""
  in case parseSimulationResultFromOutput ("Fee to the baker: " <> (toText inpString) <> "\n") of
    Right sr -> (toTezString $ srComputedFees sr) == inpString
    Left _ -> False
