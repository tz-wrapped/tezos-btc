{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.MultiSig (test_multisig) where

import qualified Data.Set as Set
import qualified Data.Text as T (drop)
import Test.Hspec (Expectation)
import Test.HUnit (assertEqual)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz
import Lorentz.Contracts.Multisig
import Lorentz.Contracts.Test.ManagedLedger (OriginationParams(..))
import Lorentz.Contracts.TZBTC as TZBTC
import qualified Lorentz.Contracts.TZBTC.Types as TZBTCTypes (SafeParameter(..))
import Lorentz.Test.Integrational
import Test.TZBTC (checkField, originateTzbtcV1ContractRaw)
import Text.Hex (decodeHex)
import Tezos.Address
import Tezos.Core (dummyChainId)
import Tezos.Crypto
import qualified Tezos.Crypto.Ed25519 as Ed25519
import Util.MultiSig as MSig
import Util.Named

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

deriving stock instance Eq (TSignature a)

addSignature_ :: ByteString -> (PublicKey, Sign) -> Either String Package
addSignature_ e (pk, sig) = do
  f <- decodePackage e :: Either String Package
  addSignature f (pk, sig)

withMultiSigContract_
  :: Natural
  -> Natural
  -> [PublicKey]
  -> (TAddress MSigParameter -> IntegrationalScenario)
  -> Expectation
withMultiSigContract_ counter thresh pkList callback =
  integrationalTestExpectation $ do
    msig <- lOriginate (tzbtcMultisigContract @'CustomErrors) "Multisig Contract"
      (mkStorage counter thresh pkList) (toMutez 0)
    callback (toTAddress msig)

withMultiSigContract
  :: Natural
  -> Natural
  -> [PublicKey]
  -> (TAddress MSigParameter -> IntegrationalScenario)
  -> Expectation
withMultiSigContract counter threshold masterPKList =
  withMultiSigContract_
    counter threshold masterPKList

sign_ :: Ed25519.SecretKey -> Text -> Sign
sign_ sk bs = case decodeHex (T.drop 2 bs) of
  Just dbs -> TSignature . SignatureEd25519 $ Ed25519.sign sk dbs
  Nothing -> error "Error with making signatures"

originateTzbtc
  :: TAddress MSigParameter
  -> IntegrationalScenarioM (TAddress (TZBTC.Parameter SomeTZBTCVersion))
originateTzbtc msig =
  originateTzbtcV1ContractRaw genesisAddress3 $ OriginationParams
    { opAdmin = toAddress msig
    , opBalances = mempty
    }

testChainId :: ChainId
testChainId = dummyChainId

test_multisig :: TestTree
test_multisig = testGroup "TZBTC contract multi-sig functionality test"
  [ testCase "Test call to multisig to add an operator works" $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Originate main contract with owner set to multisig
        tzbtc <- originateTzbtc msig
        let
          -- Make the multi-sig call that adds an operator
          tzbtcParam = TZBTCTypes.AddOperator (#operator .! operatorAddress)
          package = MSig.mkPackage msig testChainId 0 tzbtc tzbtcParam
          bytesToSign = getBytesToSign package
          encodedPackage = MSig.encodePackage package
          -- Signing the bytes
          alicePackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (alicePK, sign_ aliceSK bytesToSign)
          carlosPackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (carlosPK, sign_ carlosSK bytesToSign)
          --Make multisig param
          (msaddr, mparam) = fromRight_ "Making multisig parameter failed" $
            MSig.mkMultiSigParam masterPKList ((alicePackage) :| [carlosPackage])
        -- Finally call the multisig contract
        lCallEP msaddr (Call @"MainParameter") mparam
        lExpectStorageUpdate tzbtc $
          (checkField (operators . stCustom)
            (Set.member operatorAddress) "New operator not found")

  , testCase "Test call to multisig to add an operator fails with one signature less" $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Originate main contract with owner set to multisig
        tzbtc <- originateTzbtc msig
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTCTypes.AddOperator (#operator .! operatorAddress)
          package = MSig.mkPackage msig testChainId 0 tzbtc tzbtcParam
          bytesToSign = getBytesToSign package
          encodedPackage = MSig.encodePackage package
          -- Signing the bytes
          alicePackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (alicePK, sign_ aliceSK bytesToSign)
          --Make multisig param. We use only one signature instead of
          --the require threshold of two signatures.
          (_, mparam) = fromRight_ "Making multisig parameter failed" $
            MSig.mkMultiSigParam masterPKList ((alicePackage) :| [])
        -- Finally call the multisig contract
        err <- expectError $ lCallEP msig (Call @"MainParameter") mparam
        lExpectMichelsonFailed (const True) msig err
  , testCase "Test call to multisig to add an operator fails for bad signatures" $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Originate main contract with owner set to multisig
        tzbtc <- originateTzbtc msig
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTCTypes.AddOperator (#operator .! operatorAddress)
          package = MSig.mkPackage msig testChainId 0 tzbtc tzbtcParam
          bytesToSign = getBytesToSign package
          encodedPackage = MSig.encodePackage package
          -- Signing the bytes
          alicePackage = fromRight_ "Adding signature failed" $
            -- Make a bad signature. Use Alice's public key but Bob's secret.
            addSignature_ encodedPackage
            (alicePK, sign_ bobSK bytesToSign)
          carlosPackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (carlosPK, sign_ carlosSK bytesToSign)
          --Make multisig param
          (msaddr, mparam) = fromRight_ "Making multisig parameter failed" $
            MSig.mkMultiSigParam masterPKList ((alicePackage) :| [carlosPackage])
        -- Finally call the multisig contract
        err <- expectError $ lCallEP msaddr (Call @"MainParameter") mparam
        lExpectMichelsonFailed (const True) msig err
  , testCase "Test replay attack prevention counter" $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Originate main contract with owner set to multisig
        tzbtc <- originateTzbtc msig
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTCTypes.AddOperator (#operator .! operatorAddress)
          package = MSig.mkPackage msig testChainId 0 tzbtc tzbtcParam
          bytesToSign = getBytesToSign package
          encodedPackage = MSig.encodePackage package
          -- Signing the bytes
          alicePackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (alicePK, sign_ aliceSK bytesToSign)
          carlosPackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (carlosPK, sign_ carlosSK bytesToSign)
          --Make multisig param
          (msaddr, mparam) = fromRight_ "Making multisig parameter failed" $
            MSig.mkMultiSigParam masterPKList ((alicePackage) :| [carlosPackage])
        -- Finally call the multisig contract
        lCallEP msaddr (Call @"MainParameter") mparam
        -- Now call again with the same param, this should fail.
        err <- expectError $ lCallEP msig (Call @"MainParameter") mparam
        lExpectMichelsonFailed (const True) msig err
  , testCase "Test signed bundle created for one msig contract does not work on other" $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Originate another multisig, with a different initial balance
        mClone <- lOriginate (tzbtcMultisigContract @'CustomErrors) "Multisig Contract Clone"
          (mkStorage 0 2 masterPKList) (toMutez 1) -- Use a different initial balance
           -- so 'contract already originated' error is not triggered.
        -- Originate main contract with owner set to multisig
        tzbtc <- originateTzbtc msig
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTCTypes.AddOperator (#operator .! operatorAddress)
          -- Here we make the multi-sig pacakge for `msig` address.
          -- But will call the cloned multi-sig using it.
          package = MSig.mkPackage msig testChainId 0 tzbtc tzbtcParam
          bytesToSign = getBytesToSign package
          encodedPackage = MSig.encodePackage package
          -- Signing the bytes
          alicePackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (alicePK, sign_ aliceSK bytesToSign)
          carlosPackage = fromRight_ "Adding signature failed" $
            addSignature_ encodedPackage
            (carlosPK, sign_ carlosSK bytesToSign)
          --Make multisig param
          (msaddr, mparam) = fromRight_ "Making multisig parameter failed" $
            MSig.mkMultiSigParam masterPKList ((alicePackage) :| [carlosPackage])

        -- Call the actual contract with the bundle. Should work as
        -- expected.
        lCallEP msaddr (Call @"MainParameter") mparam
        lExpectStorageUpdate tzbtc $
          (checkField (operators . stCustom)
            (Set.member operatorAddress) "New operator not found")

        -- Call the clone with the bundle created for the real multisig
        -- contract.
        err <- expectError $ lCallEP mClone (Call @"MainParameter") mparam
        -- It should fail.
        lExpectMichelsonFailed (const True) mClone err

  , testCase "Test mkMultiSigParam function arranges the signatures in the order of public keys" $ do
      let
        msig  = TAddress @MSigParameter $
                unsafeParseAddress "KT19rTTBPeG1JAvrECgoQ8LJj1mJrN7gsdaH"
        tzbtc = TAddress @(TZBTC.Parameter SomeTZBTCVersion) $
                unsafeParseAddress "KT1XXJWcjrwfcPL4n3vjmwCBsvkazDt8scYY"

        tzbtcParam = TZBTCTypes.AddOperator (#operator .! operatorAddress)
        package = MSig.mkPackage @(TAddress MSigParameter) msig testChainId 0 tzbtc tzbtcParam
        bytesToSign = getBytesToSign package
        encodedPackage = MSig.encodePackage package
        -- Signing the bytes
        aliceSig = sign_ aliceSK bytesToSign
        carlosSig = sign_ carlosSK bytesToSign
        alicePackage = fromRight_ "Adding signature failed" $
          addSignature_ encodedPackage (alicePK, aliceSig)
        carlosPackage = fromRight_ "Adding signature failed" $
          addSignature_ encodedPackage (carlosPK, carlosSig)
        --Make multisig param.
        mparam = fromRight_ "Making multisig parameter failed" $
          MSig.mkMultiSigParam masterPKList ((carlosPackage) :| [alicePackage])
      case mparam of
        (_, (_, sigList)) -> assertEqual
          "The signatures in multi-sig parameter is in the expected order"
          [Just aliceSig, Nothing, Just carlosSig]
          sigList
      -- Now specify packages in different order while creating multisig param
      -- and ensure that the order of signatures remins valid in the generated
      -- parameter.
      let
        mparam_ = fromRight_ "Making multisig parameter failed" $
          MSig.mkMultiSigParam masterPKList ((alicePackage) :| [carlosPackage])
      case mparam_ of
        (_, (_, sigList)) -> assertEqual
          "The signatures in multi-sig parameter is in the expected order"
          [Just aliceSig, Nothing, Just carlosSig]
          sigList
  ]

  where
    fromRight_ er e = fromRight (error er) e
    operatorAddress :: Address
    operatorAddress = genesisAddress5

    aliceSK = Ed25519.detSecretKey "aa"
    bobSK = Ed25519.detSecretKey "bbb"
    carlosSK = Ed25519.detSecretKey "cccc"

    alicePK = PublicKeyEd25519 . Ed25519.toPublic $ aliceSK
    bobPK = PublicKeyEd25519 . Ed25519.toPublic $ bobSK
    carlosPK = PublicKeyEd25519 . Ed25519.toPublic $ carlosSK

    masterPKList = [alicePK, bobPK, carlosPK]
