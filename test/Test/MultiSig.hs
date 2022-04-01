{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.MultiSig (test_multisig) where

import Data.Set qualified as Set
import Data.Text qualified as T (drop)
import Test.HUnit (assertEqual)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz hiding (assert, chainId)
import Lorentz.Contracts.Multisig
import Lorentz.Contracts.TZBTC as TZBTC
import Lorentz.Contracts.TZBTC.Types qualified as TZBTCTypes (SafeParameter(..))
import Lorentz.Contracts.Test.ManagedLedger (OriginationParams(..))
import Morley.Tezos.Address
import Morley.Tezos.Core (dummyChainId)
import Morley.Tezos.Crypto
import Morley.Tezos.Crypto.Ed25519 qualified as Ed25519
import Morley.Util.Named
import Test.Cleveland
import Test.TZBTC (TestTZBTCVersion(..), coerceContractHandler, originateTzbtcV1ContractRaw)
import Text.Hex (decodeHex)
import Util.MultiSig as MSig

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

deriving stock instance Eq (TSignature a)

addSignature_ :: ByteString -> (PublicKey, Sign) -> Either String Package
addSignature_ e (pk, sig) = do
  f <- decodePackage e :: Either String Package
  addSignature f (pk, sig)

withMultiSigContract_
  :: (HasCallStack, MonadCleveland caps m)
  => Natural
  -> Natural
  -> [PublicKey]
  -> (ContractHandle MSigParameter MSigStorage () -> m ())
  -> m ()
withMultiSigContract_ counter thresh pkList callback = do
  msig <- originateSimple "Multisig Contract" (mkStorage counter thresh pkList)
    (tzbtcMultisigContract @'CustomErrors)
  callback msig

withMultiSigContract
  :: (HasCallStack, MonadCleveland caps m)
  => Natural
  -> Natural
  -> [PublicKey]
  -> (ContractHandle MSigParameter MSigStorage () -> m ())
  -> m ()
withMultiSigContract counter threshold masterPKList =
  withMultiSigContract_
    counter threshold masterPKList

sign_ :: Ed25519.SecretKey -> Text -> Sign
sign_ sk bs = case decodeHex (T.drop 2 bs) of
  Just dbs -> TSignature . SignatureEd25519 $ Ed25519.sign sk dbs
  Nothing -> error "Error with making signatures"

test_multisig :: TestTree
test_multisig = testGroup "TZBTC contract multi-sig functionality test"
  [ testScenario "Test call to multisig to add an operator works" $ scenario $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Originate main contract with owner set to multisig
        tzbtc <- originateTzbtc msig
        testChainId <- getChainId
        operatorAddress <- newFreshAddress "operator"
        let
          -- Make the multi-sig call that adds an operator
          tzbtcParam = TZBTCTypes.AddOperator (#operator :! operatorAddress)
          package = MSig.mkPackage msig testChainId 1 (toTAddress $ coerceContractHandler tzbtc) tzbtcParam
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
          (_, mparam) = fromRight_ "Making multisig parameter failed" $
            MSig.mkMultiSigParam masterPKList ((alicePackage) :| [carlosPackage])
        -- Finally call the multisig contract
        call msig (Call @"MainParameter") mparam
        st <- getStorage tzbtc
        checkField (getOperators @TZBTCv1) (Set.member operatorAddress) st
          "New operator not found"

  , testScenario "Test call to multisig to add an operator fails with one signature less" $ scenario $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Originate main contract with owner set to multisig
        tzbtc <- originateTzbtc msig
        testChainId <- getChainId
        operatorAddress <- newFreshAddress "operator"
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTCTypes.AddOperator (#operator :! operatorAddress)
          package = MSig.mkPackage msig testChainId 1 (toTAddress $ coerceContractHandler tzbtc) tzbtcParam
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
        expectCustomError_ #insufficientSignatures $ call msig (Call @"MainParameter") mparam
  , testScenario "Test call to multisig to add an operator fails for bad signatures" $ scenario $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Originate main contract with owner set to multisig
        tzbtc <- originateTzbtc msig
        testChainId <- getChainId
        operatorAddress <- newFreshAddress "operator"
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTCTypes.AddOperator (#operator :! operatorAddress)
          package = MSig.mkPackage msig testChainId 1 (toTAddress $ coerceContractHandler tzbtc) tzbtcParam
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
        expectCustomError_ #invalidSignature $ call msaddr (Call @"MainParameter") mparam
  , testScenario "Test replay attack prevention counter" $ scenario $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Originate main contract with owner set to multisig
        tzbtc <- originateTzbtc msig
        testChainId <- getChainId
        operatorAddress <- newFreshAddress "operator"
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTCTypes.AddOperator (#operator :! operatorAddress)
          package = MSig.mkPackage msig testChainId 1 (toTAddress $ coerceContractHandler tzbtc) tzbtcParam
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
        call msaddr (Call @"MainParameter") mparam
        -- Now call again with the same param, this should fail.
        expectCustomError_ #counterDoesntMatch $
          call msaddr (Call @"MainParameter") mparam
  , testScenario "Test signed bundle created for one msig contract does not work on other" $ scenario $ do
      -- Originate multisig with threshold 2 and a master pk list of
      -- three public keys
      withMultiSigContract 0 2 masterPKList $ \msig -> do
        -- Originate another multisig, with a different initial balance
        mClone <- originateSimple "Multisig Contract Clone" (mkStorage 1 2 masterPKList) $
          tzbtcMultisigContract @'CustomErrors
        -- Originate main contract with owner set to multisig
        tzbtc <- originateTzbtc msig
        testChainId <- getChainId
        operatorAddress <- newFreshAddress "operator"
        -- Make the multi-sig call that adds an operator
        let
          tzbtcParam = TZBTCTypes.AddOperator (#operator :! operatorAddress)
          -- Here we make the multi-sig pacakge for `msig` address.
          -- But will call the cloned multi-sig using it.
          package = MSig.mkPackage msig testChainId 1 (toTAddress $ coerceContractHandler tzbtc) tzbtcParam
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
        call msaddr (Call @"MainParameter") mparam
        st <- getStorage tzbtc
        checkField (getOperators @TZBTCv1) (Set.member operatorAddress) st
          "New operator not found"

        -- Call the clone with the bundle created for the real multisig
        -- contract.
        expectCustomError_ #invalidSignature $ call mClone (Call @"MainParameter") mparam

  , testCase "Test mkMultiSigParam function arranges the signatures in the order of public keys" $ do
      let
        msig  = TAddress @MSigParameter [ta|KT19rTTBPeG1JAvrECgoQ8LJj1mJrN7gsdaH|]
        tzbtc = TAddress @(TZBTC.Parameter SomeTZBTCVersion)
          [ta|KT1XXJWcjrwfcPL4n3vjmwCBsvkazDt8scYY|]
        operatorAddress = [ta|tz1fdBQ4jHa2xVNHte8pVFHvCokidE4MvDxm|]

        tzbtcParam = TZBTCTypes.AddOperator (#operator :! operatorAddress)
        package = MSig.mkPackage @(TAddress MSigParameter ()) msig dummyChainId 0 (toTAddress tzbtc) tzbtcParam
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

    aliceSK = Ed25519.detSecretKey "aa"
    bobSK = Ed25519.detSecretKey "bbb"
    carlosSK = Ed25519.detSecretKey "cccc"

    alicePK = PublicKeyEd25519 . Ed25519.toPublic $ aliceSK
    bobPK = PublicKeyEd25519 . Ed25519.toPublic $ bobSK
    carlosPK = PublicKeyEd25519 . Ed25519.toPublic $ carlosSK

    masterPKList = [alicePK, bobPK, carlosPK]

    originateTzbtc
      :: (HasCallStack, MonadCleveland caps m)
      => ContractHandle MSigParameter MSigStorage ()
      -> m (ContractHandle (TZBTC.Parameter TZBTCv1) (TZBTC.Storage TZBTCv1) ())
    originateTzbtc msig = do
      admin <- newAddress auto
      chainId <- getChainId
      redeem <- newFreshAddress "redeem"
      tzbtc <- originateTzbtcV1ContractRaw redeem $ OriginationParams
        { opAdmin = admin
        , opBalances = mempty
        }
      withSender admin $
        call tzbtc CallDefault $ fromFlatParameter $ TransferOwnership (#newOwner :! toAddress msig)
      let
        tzbtcParam = TZBTCTypes.AcceptOwnership ()
        package = MSig.mkPackage msig chainId 0 (toTAddress $ coerceContractHandler tzbtc) tzbtcParam
        bytesToSign = getBytesToSign package
        encodedPackage = MSig.encodePackage package
        alicePackage = fromRight_ "Adding signature failed" $
          addSignature_ encodedPackage
          (alicePK, sign_ aliceSK bytesToSign)
        bobPackage = fromRight_ "Adding signature failed" $
          addSignature_ encodedPackage
          (bobPK, sign_ bobSK bytesToSign)
        --Make multisig param
        (_, mparam) = fromRight_ "Making multisig parameter failed" $
          MSig.mkMultiSigParam masterPKList ((alicePackage) :| [bobPackage])
      call msig (Call @"MainParameter") mparam
      return tzbtc
