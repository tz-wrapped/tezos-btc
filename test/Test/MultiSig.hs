{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.MultiSig (test_multisig) where

import qualified Data.Set as Set
import qualified Data.Text as T (drop)
import Test.Tasty (TestTree, testGroup)
import Test.Hspec (Expectation)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Lorentz hiding (SomeContract)
import Lorentz.Contracts.TZBTC as TZBTC
import Lorentz.Contracts.TZBTC.MultiSig as MSig
import Lorentz.Test.Integrational
import Michelson.Runtime (prepareContract)
import Michelson.Test (originate)
import Michelson.Typed.Convert
import qualified Michelson.Untyped as U
import Text.Hex (decodeHex)
import Tezos.Crypto
import Tezos.Address
import Util.MultiSig as MSig
import Util.Named

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

unsafeParsePublicKey :: Text -> PublicKey
unsafeParsePublicKey x = either (error . show) id $ parsePublicKey x

unsafeParseSecretKey :: Text -> SecretKey
unsafeParseSecretKey x = either (error . show) id $ parseSecretKey x

addSignature_ :: ByteString -> (Text, Text) -> Either String Package
addSignature_ e (pk, sig) = do
  case parsePublicKey pk of
    Right pubk -> case parseSignature sig of
      Right sig_ -> do
        f <- decodePackage e :: Either String Package
        addSignature f (pubk, sig_)
      _ -> Left "Error"
    _ -> Left "Error"

withMultiSigContract
  :: Natural
  -> Natural
  -> [PublicKey]
  -> (Address -> IntegrationalScenario)
  -> Expectation
withMultiSigContract counter thresh pkList callback = do
  m <- prepareContract (Just "contracts/MultiSigGeneric.tz")
  integrationalTestExpectation $ do
    msig <- originate m "Multisig Contract"
      (untypeValue $ toVal (MSig.mkStorage counter thresh pkList)) (toMutez 0)
    callback msig

sign_ :: SecretKey -> Text -> Signature
sign_ sk bs = case decodeHex (T.drop 2 bs) of
  Just dbs -> sign sk dbs
  Nothing -> error "Error with making signatures"

test_multisig :: TestTree
test_multisig = testGroup "TZBTC contract multi-sig functionality test"
  [ testCase "Test call to multisig to add an operator by admin works" $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Define initial storage for main contract and expected storage
        -- after the call
        let
          initStorage = TZBTC.mkStorage msig genesisAddress3 mempty mempty
          expectedStorage =
            TZBTC.mkStorage msig genesisAddress3 mempty (Set.fromList [(operatorAddress)])
        -- Originate main contract with admin set to multisig
        tzbtc <- lOriginate tzbtcContract "TZBTC Contract" initStorage (toMutez 1000)
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTC.AddOperator (#operator .! operatorAddress)
          package = MSig.mkPackageFromTzbtcParam msig 0 tzbtc tzbtcParam
          bytesToSign = getBytesToSign package
          encodedPackage = MSig.encodePackage package
        -- Signing the bytes
          alicePackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (alicePKRaw, formatSignature $ sign_ aliceSK bytesToSign)
          carlosPackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (carlosPKRaw, formatSignature $ sign_ carlosSK bytesToSign)
          --Make multisig param
          (msaddr, mparam) = fromRight_ "Making multisig parameter failed" $
            MSig.mkMultiSigParam masterPKList ((alicePackage) :| [carlosPackage])
        -- Finally call the multisig contract
        lCall msaddr mparam
        validate . Right $
          lExpectStorageConst tzbtc expectedStorage

  , testCase "Test call to multisig to add an operator by fails with one signature less" $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Define initial storage for main contract
        let
          initStorage = TZBTC.mkStorage msig genesisAddress3 mempty mempty
        -- Originate main contract with admin set to multisig
        tzbtc <- lOriginate tzbtcContract "TZBTC Contract" initStorage (toMutez 1000)
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTC.AddOperator (#operator .! operatorAddress)
          package = MSig.mkPackageFromTzbtcParam msig 0 tzbtc tzbtcParam
          bytesToSign = getBytesToSign package
          encodedPackage = MSig.encodePackage package
          -- Signing the bytes
          alicePackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (alicePKRaw, formatSignature $ sign_ aliceSK bytesToSign)
          --Make multisig param. We use only one signature instead of
          --the require threshold of two signatures.
          (_, mparam) = fromRight_ "Making multisig parameter failed" $
            MSig.mkMultiSigParam masterPKList ((alicePackage) :| [])
        -- Finally call the multisig contract
        lCall (ContractAddr msig) mparam
        validate . Left $
          lExpectMichelsonFailed (const True) (ContractAddr msig)
  , testCase "Test call to multisig to add an operator by fails for bad signatures" $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Define initial storage for main contract and expected storage
        -- after the call
        let
          initStorage = TZBTC.mkStorage msig genesisAddress3 mempty mempty
        -- Originate main contract with admin set to multisig
        tzbtc <- lOriginate tzbtcContract "TZBTC Contract" initStorage (toMutez 1000)
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTC.AddOperator (#operator .! operatorAddress)
          package = MSig.mkPackageFromTzbtcParam msig 0 tzbtc tzbtcParam
          bytesToSign = getBytesToSign package
          encodedPackage = MSig.encodePackage package
          -- Signing the bytes
          alicePackage = fromRight_ "Adding signature failed" $
            -- Make a bad signature. Use Alice's public key but Bob's secret.
            addSignature_ encodedPackage
            (alicePKRaw, formatSignature $ sign_ bobSK bytesToSign)
          carlosPackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (carlosPKRaw, formatSignature $ sign_ carlosSK bytesToSign)
          --Make multisig param
          (msaddr, mparam) = fromRight_ "Making multisig parameter failed" $
            MSig.mkMultiSigParam masterPKList ((alicePackage) :| [carlosPackage])
        -- Finally call the multisig contract
        lCall msaddr mparam
        validate . Left $
          lExpectMichelsonFailed (const True) (ContractAddr msig)
  , testCase "Test replay attack prevention counter" $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Define initial storage for main contract and expected storage
        -- after the call
        let
          initStorage = TZBTC.mkStorage msig genesisAddress3 mempty mempty
        -- Originate main contract with admin set to multisig
        tzbtc <- lOriginate tzbtcContract "TZBTC Contract" initStorage (toMutez 1000)
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTC.AddOperator (#operator .! operatorAddress)
          package = MSig.mkPackageFromTzbtcParam msig 0 tzbtc tzbtcParam
          bytesToSign = getBytesToSign package
          encodedPackage = MSig.encodePackage package
          -- Signing the bytes
          alicePackage = fromRight_ "Adding signature failed" $
            --Use Alice's public key but bob's secret.
            addSignature_ encodedPackage
            (alicePKRaw, formatSignature $ sign_ aliceSK bytesToSign)
          carlosPackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (carlosPKRaw, formatSignature $ sign_ carlosSK bytesToSign)
          --Make multisig param
          (msaddr, mparam) = fromRight_ "Making multisig parameter failed" $
            MSig.mkMultiSigParam masterPKList ((alicePackage) :| [carlosPackage])
        -- Finally call the multisig contract
        lCall msaddr mparam
        -- Now call again with the same param, this should fail.
        lCall (ContractAddr msig) mparam
        validate . Left $
          lExpectMichelsonFailed (const True) (ContractAddr msig)
  , testCase "Test signed bundle created for one msig contract does not work on other" $ do

      a <- prepareContract (Just "contracts/MultiSigGeneric.tz")
      -- Add some nop instructions to the contract so that we can
      -- originate a duplicate.
      let mClone = a { U.code = (U.PrimEx (U.DUP U.noAnn)):(U.PrimEx U.DROP):(U.code a) }
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Define initial storage for main contract and expected storage
        -- after the call
        let
          initStorage = TZBTC.mkStorage msig genesisAddress3 mempty mempty
          expectedStorage =
            TZBTC.mkStorage msig genesisAddress3 mempty (Set.fromList [(operatorAddress)])
        -- Originate main contract with admin set to multisig
        tzbtc <- lOriginate tzbtcContract "TZBTC Contract" initStorage (toMutez 1000)
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTC.AddOperator (#operator .! operatorAddress)
          -- Here we make the multi-sig pacakge for msig address.
          -- But will call the cloned multi-sig using it.
          package = MSig.mkPackageFromTzbtcParam msig 0 tzbtc tzbtcParam
          bytesToSign = getBytesToSign package
          encodedPackage = MSig.encodePackage package
          -- Signing the bytes
          alicePackage = fromRight_ "Adding signature failed" $
            --Use Alice's public key but bob's secret.
            addSignature_ encodedPackage
            (alicePKRaw, formatSignature $ sign_ aliceSK bytesToSign)
          carlosPackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (carlosPKRaw, formatSignature $ sign_ carlosSK bytesToSign)
          --Make multisig param
          (msaddr, mparam) = fromRight_ "Making multisig parameter failed" $
            MSig.mkMultiSigParam masterPKList ((alicePackage) :| [carlosPackage])

        -- Call the actual contract with the bundle. Should work as
        -- expected.
        lCall msaddr mparam
        validate . Right $
          lExpectStorageConst tzbtc expectedStorage
                                                                --
        -- Now make a clone of the multisig contract that only differs in
        -- some noop instruction at the top

        mSigClone <- originate mClone "Multisig Contract"
          (untypeValue $ toVal (MSig.mkStorage 0 2 masterPKList)) (toMutez 0)

        -- Call the clone with the bundle created for the real multisig
        -- contract.
        lCall (ContractAddr mSigClone) mparam
        -- It should fail
        validate . Left $
          lExpectMichelsonFailed (const True) (ContractAddr mSigClone)

  , testCase "Test mkMultiSigParam function arranges the signatures in the order of public keys" $ do
      let
        msig = unsafeParseAddress "KT19rTTBPeG1JAvrECgoQ8LJj1mJrN7gsdaH"
        tzbtc = ContractAddr $ unsafeParseAddress "KT1XXJWcjrwfcPL4n3vjmwCBsvkazDt8scYY"

        tzbtcParam = TZBTC.AddOperator (#operator .! operatorAddress)
        package = MSig.mkPackageFromTzbtcParam msig 0 tzbtc tzbtcParam
        bytesToSign = getBytesToSign package
        encodedPackage = MSig.encodePackage package
        -- Signing the bytes
        aliceSig = sign_ aliceSK bytesToSign
        carlosSig = sign_ carlosSK bytesToSign
        alicePackage = fromRight_ "Adding signature failed" $
          --Use Alice's public key but bob's secret.
          addSignature_ encodedPackage (alicePKRaw, formatSignature aliceSig)
        carlosPackage = fromRight_ "Adding signature failed" $
          addSignature_ encodedPackage (carlosPKRaw, formatSignature carlosSig)
        --Make multisig param, but extract the signature list
        mparam = fromRight_ "Making multisig parameter failed" $
          MSig.mkMultiSigParam masterPKList ((carlosPackage) :| [alicePackage])
      case mparam of
        (_, MSig.ParameterMain (_, sigList)) -> assertEqual
          "The signatures in multi-sig parameter is in the expected order"
          [Just aliceSig, Nothing, Just carlosSig]
          sigList
        _ -> error "Unexpected multisig param"
      let
        mparam_ = fromRight_ "Making multisig parameter failed" $
          MSig.mkMultiSigParam masterPKList ((alicePackage) :| [carlosPackage])
      case mparam_ of
        (_, MSig.ParameterMain (_, sigList)) -> assertEqual
          "The signatures in multi-sig parameter is in the expected order"
          [Just aliceSig, Nothing, Just carlosSig]
          sigList
        _ -> error "Unexpected multisig param"

  , testCase "Test user is not allowed to sign a bad package" $ do
      let
        msig = unsafeParseAddress "KT19rTTBPeG1JAvrECgoQ8LJj1mJrN7gsdaH"
        tzbtc = ContractAddr $ unsafeParseAddress "KT1XXJWcjrwfcPL4n3vjmwCBsvkazDt8scYY"

        tzbtcParam = TZBTC.AddOperator (#operator .! operatorAddress)
        tzbtcParamBadParam = TZBTC.RemoveOperator (#operator .! operatorAddress)
        package = MSig.mkPackageFromTzbtcParam msig 0 tzbtc tzbtcParam
        package2 = MSig.mkPackageFromTzbtcParam msig 0 tzbtc tzbtcParamBadParam

        -- replace operation with bad operation
        badPackage = package { pkToSign = pkToSign package2 } :: Package
        bytesToSign = getBytesToSign package
        aliceSig = sign_ aliceSK bytesToSign
      assertBool "User was able to sign bad package" (isLeft $ addSignature badPackage (alicePK, aliceSig))
  ]

  where
    fromRight_ er e = fromRight (error er) e
    operatorAddress :: Address
    operatorAddress = genesisAddress5

    alicePKRaw = "edpkvKTdd6kd4JpMk9fFdmay4zdboWUHRkax5hthJrtDHQ6Gstyn5b"
    bobPKRaw = "edpktrycE9jd2jtFnv5ndie9P9842wZzToPXhBrWxZrLorKbJXEZSf"
    carlosPKRaw = "edpkvFrVEBQWQfc4z1rE2g6TFsZJjpTeLwcEBy9icc7VqRXfvg9TD1"

    alicePK = unsafeParsePublicKey alicePKRaw
    bobPK = unsafeParsePublicKey bobPKRaw
    carlosPK = unsafeParsePublicKey carlosPKRaw

    aliceSK = unsafeParseSecretKey "edsk4PPtShemob8MbUacQ4pUQfcq1iLwTwpWE37M97e3H9sZqT4wLd"
    bobSK = unsafeParseSecretKey "edsk4S74qyLz74CZyMr2qKCKKmnb8a94yX5pCp5LjutbBkSdhixQyT"
    carlosSK = unsafeParseSecretKey "edsk2jKGAnpoBMMqmyRQdwmZSjW9qiTozrWkPkLpbeNToNSsNUCxwY"

    masterPKList = [alicePK, bobPK, carlosPK]
