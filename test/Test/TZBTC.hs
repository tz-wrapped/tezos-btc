{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

-- We have to test some deprecated behaviour.
-- For V1 this is fine, for V2 this will be fixed.
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.TZBTC
  ( test_interface
  , test_acceptOwnership
  , test_burn
  , test_mint
  , test_approvableLedgerV1
  , test_approvableLedgerV2
  , test_pause
  , test_unpause
  , test_setRedeemAddress
  , test_transferOwnership
  , test_bookkeeping
  , test_addOperator
  , test_removeOperator
  , test_get_meta
  , test_documentation

  -- * Utilities
  , TestTZBTCVersion (checkField, getOperators)
  , coerceContractHandler
  , dummyV1Parameters
  , originateTzbtcV1ContractRaw
  ) where

import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text.IO.Utf8 as Utf8
import Test.HUnit (assertFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz hiding (assert)
import Lorentz.Contracts.Metadata
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import qualified Lorentz.Contracts.Test.ApprovableLedger as AL
import Lorentz.Contracts.Test.ManagedLedger (OriginationParams(..), originateManagedLedger)
import qualified Lorentz.Contracts.Test.ManagedLedger as ML
import Lorentz.Contracts.Upgradeable.Common
  (EpwUpgradeParameters(..), KnownContractVersion, VerPermanent, VerUStoreTemplate, emptyPermanentImpl)
import Lorentz.Test
  (contractConsumer, expectContractEntrypoints, runDocTests, testLorentzDoc)
import Lorentz.UStore
import Lorentz.UStore.Migration
import Michelson.Runtime (parseExpandContract)
import Michelson.Test.Unit (matchContractEntrypoints, mkEntrypointsMap)
import Michelson.Typed.Convert (convertContract)
import Morley.Nettest
import Morley.Nettest.Tasty
import qualified Michelson.Untyped as U
import Tezos.Address (ta)
import Util.Named

import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.Common.Types (Operators)
import qualified Lorentz.Contracts.TZBTC.V1 as V1
import qualified Lorentz.Contracts.TZBTC.V2 as V2
import Test.Smoke (TestUpgrade(..), testUpgradeToV1, testUpgradeToV2)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

class KnownContractVersion ver => TestTZBTCVersion ver where
  checkField
    :: ( Generic (VerUStoreTemplate ver)
       , UStoreTraversable DecomposeUStoreTW (VerUStoreTemplate ver)
       )
    => (VerUStoreTemplate ver -> UStoreField a)
    -> (a -> Bool)
    -> (Storage ver -> Bool)
  checkField ef cf = checkStorage_ (\st -> cf $ unUStoreField $ ef st)

  checkStorage_
    :: ( Generic (VerUStoreTemplate ver)
       , UStoreTraversable DecomposeUStoreTW (VerUStoreTemplate ver)
       )
    => (VerUStoreTemplate ver -> Bool)
    -> Storage ver -> Bool
  checkStorage_ fn st =
    case ustoreDecomposeFull $ dataMap st of
      Right template -> fn template
      Left _ -> False

  getOperators :: VerUStoreTemplate ver -> UStoreField Operators
  getNewOwner :: VerUStoreTemplate ver -> UStoreField (Maybe Address)
  getPaused :: VerUStoreTemplate ver -> UStoreField Bool

instance TestTZBTCVersion TZBTCv1 where
  getOperators = operators . stCustom
  getNewOwner = newOwner . stCustom
  getPaused = paused . stCustom

instance TestTZBTCVersion V2.TZBTCv2 where
  getOperators = V2.operators . stCustom
  getNewOwner = V2.newOwner . stCustom
  getPaused = V2.paused . stCustom

type TestableTZBTCVersion ver =
  ( TestTZBTCVersion ver, Generic (VerUStoreTemplate ver), Typeable ver
  , VerPermanent ver ~ Empty, UStoreTraversable DecomposeUStoreTW (VerUStoreTemplate ver)
  )

dummyV1Parameters :: Address -> Map Address Natural -> V1Parameters
dummyV1Parameters redeem balances = V1Parameters
  { v1RedeemAddress = redeem
  , v1TokenMetadata = defaultTZBTCMetadata
  , v1Balances = balances
  }

dummyV2Parameters :: Address -> Map Address Natural -> V2Parameters
dummyV2Parameters = dummyV1Parameters


redeemAddress_ :: Address
redeemAddress_ = [ta|tz1Mdd7rL6jGFAWCMUqatQ64K9pTpe8kazfy|]

coerceContractHandler
  :: (Typeable ver2, VerPermanent ver2 ~ Empty)
  => ContractHandler (Parameter ver1) (Storage ver1) -> ContractHandler (Parameter ver2) (Storage ver2)
coerceContractHandler c = ContractHandler { chContractName = chContractName c, chAddress = chAddress c }

originateTzbtcV1ContractRaw
  :: (HasCallStack, MonadNettest caps base m)
  => Address -> OriginationParams
  -> m (ContractHandler (Parameter TZBTCv1) (Storage TZBTCv1))
originateTzbtcV1ContractRaw redeem op = do
  let owner = ML.opAdmin op
      balances = ML.opBalances op
  c <- originateSimple "TZBTC Contract" (mkEmptyStorageV0 owner) tzbtcContract
  unTestUpgrade testUpgradeToV1 owner redeem balances c
  pure $ coerceContractHandler c

originateTzbtcV1Contract
  :: (HasCallStack, MonadNettest caps base m)
  => Address -> m (ContractHandler (Parameter TZBTCv1) (Storage TZBTCv1))
originateTzbtcV1Contract admin = do
  originateTzbtcV1ContractRaw redeemAddress_ $ OriginationParams
    { opAdmin = admin
    , opBalances = M.fromList [(redeemAddress_, initialSupply)]
    }

originateTzbtcV2ContractRaw
  :: (HasCallStack, MonadNettest caps base m)
  => Address -> OriginationParams
  -> m (ContractHandler (Parameter V2.TZBTCv2) (Storage V2.TZBTCv2))
originateTzbtcV2ContractRaw redeem op = do
  let owner = ML.opAdmin op
      balances = ML.opBalances op
  c <- originateSimple "TZBTC Conctact" (mkEmptyStorageV0 owner) tzbtcContract
  unTestUpgrade testUpgradeToV2 owner redeem balances c
  pure $ coerceContractHandler c

originateTzbtcV2Contract
  :: (HasCallStack, MonadNettest caps base m)
  => Address -> m (ContractHandler (Parameter V2.TZBTCv2) (Storage V2.TZBTCv2))
originateTzbtcV2Contract admin = do
  originateTzbtcV2ContractRaw redeemAddress_ $ OriginationParams
    { opAdmin = admin
    , opBalances = M.fromList [(redeemAddress_, initialSupply)]
    }

-- Some constants

initialSupply :: Natural
initialSupply = 500

-- We have the header of the actual tzbtc contract (parameter, storage types with
-- some nop code) in ./test/resources/tzbtc-parameter-entrypoints-ref.tz.
-- We parse it, extract the paramter type from the parsed contract. Then we use
-- the 'mkEntrypointsMap' function to extract the field and type annotations from
-- the relavant parts of the parameter type. This is compared against the
-- auto derivied entrypoints of the contract. They should match or else the
-- test fails. If the tzbtc parameter has to be changed, then this test should
-- be fixed by editing the `tzbtc-parameter-entrypoints-ref.tz` file.
entrypointsRef :: IO (Map EpName U.Ty)
entrypointsRef = mkEntrypointsMap <$> tzbtcParameterType
  where
    tzbtcParameterType :: IO U.ParameterType
    tzbtcParameterType = do
      code <- Utf8.readFile "./test/resources/tzbtc-parameter-entrypoints-ref.mtz"
      case parseExpandContract Nothing code of
        Right c -> pure $ U.contractParameter c
        Left e -> error ("Error in parsing reference contract paramter:" <> show e)

-- Tests

testContract
  :: String
  -> ( forall m caps base ver. (MonadNettest caps base m, TestableTZBTCVersion ver)
       => (Address -> m (ContractHandler (Parameter ver) (Storage ver))) -> Proxy ver -> m ()
     )
  -> TestTree
testContract name testSuite =
  testGroup name $
    [ nettestScenarioCaps "V1 contract" $ testSuite originateTzbtcV1Contract Proxy
    , nettestScenarioCaps "V2 contract" $ testSuite originateTzbtcV2Contract Proxy
    ]

testContractEmulated
  :: String
  -> ( forall m caps base ver. (MonadEmulated caps base m, TestableTZBTCVersion ver)
       => (Address -> m (ContractHandler (Parameter ver) (Storage ver))) -> Proxy ver -> m ()
     )
  -> TestTree
testContractEmulated name testSuite =
  testGroup name $
    [ nettestScenarioOnEmulatorCaps "V1 contract" $ testSuite originateTzbtcV1Contract Proxy
    , nettestScenarioOnEmulatorCaps "V2 contract" $ testSuite originateTzbtcV2Contract Proxy
    ]

test_interface :: TestTree
test_interface = testGroup "TZBTC consistency test"
  [ testCase
      "Has an approvable ledger interface that satisfies FA1.2 specification" $ do
        expectContractEntrypoints @AL.Parameter tzbtcContract

  , testCase
      "Has the expected interface of TZBTC contract" $ do
        reference <- entrypointsRef
        let untypedTzbtc = convertContract $ cMichelsonContract tzbtcContract
        case matchContractEntrypoints untypedTzbtc reference of
          Right _ -> pass
          Left missing -> do
            assertFailure $ "Some entrypoints were not found:" <> (show missing)
  ]

test_addOperator :: TestTree
test_addOperator = testGroup "TZBTC contract `addOperator` test"
  [ testContract
      "Call to `addOperator` from random address gets denied with `senderIsNotOwner` error." $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        bob <- newAddress auto
        operator <- newAddress auto
        withSender bob $
          expectCustomError_ #senderIsNotOwner $ call c CallDefault $
          fromFlatParameter $ AddOperator (#operator .! operator)
  , testContractEmulated
      "Call to `addOperator` from owner adds operator to the set." $
      \originateContract (_ :: Proxy ver) -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        operator <- newAddress auto
        withSender admin $
          call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)
        st <- getFullStorage c
        assert (checkField (getOperators @ver) (Set.member operator) st)
          "New operator not found"
  ]

test_removeOperator :: TestTree
test_removeOperator = testGroup "TZBTC contract `addOperator` test"
  [ testContract
      "Call to `removeOperator` from random address gets denied with `senderIsNotOwner` error." $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        bob <- newAddress auto
        operator <- newAddress auto
        withSender bob $
          expectCustomError_ #senderIsNotOwner $
          call c CallDefault $ fromFlatParameter $ RemoveOperator (#operator .! operator)

  , testContractEmulated
      "Call to `removeOperator` from owner removes operator from the set." $
      \originateContract (_ :: Proxy ver) -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        operator <- newAddress auto
        withSender admin $ call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)
        st1 <- getFullStorage c
        assert (checkField (getOperators @ver) (Set.member operator) st1)
          "New operator not found"
        withSender admin $ call c CallDefault $ fromFlatParameter $ RemoveOperator (#operator .! operator)
        st2 <- getFullStorage c
        assert (checkField (getOperators @ver) (Prelude.not . Set.member operator) st2)
          "Unexpectedly found operator"
  ]

test_transferOwnership :: TestTree
test_transferOwnership = testGroup "TZBTC contract `transferOwnership` test"
  [ testContract
      "Call to `transferOwnership` from random address gets denied with `senderIsNotOwner` error." $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        bob <- newAddress auto
        replaceAddress <- newAddress "replaceAddress"
        withSender bob $ expectCustomError_ #senderIsNotOwner $
          call c CallDefault $ fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress)
  , testContractEmulated
      "Call to `transferOwnership` from owner address gets denied with `senderIsNotOwner` error." $
      \originateContract (_ :: Proxy ver) -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        replaceAddress <- newAddress auto
        withSender admin $
          call c CallDefault $ fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress)
        st <- getFullStorage c
        assert (checkField (getNewOwner @ver) (== Just replaceAddress) st)
          "Expected `newOwner` not found"
  ]

test_acceptOwnership :: TestTree
test_acceptOwnership = testGroup "TZBTC contract `acceptOwnership` test"
  [ testContract
      "Call to `acceptOwnership` to non-transfering contract gets denied with `notInTransferOwnershipMode` error " $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        bob <- newAddress auto
        withSender bob $ expectCustomError_ #notInTransferOwnershipMode $
          call c CallDefault $ fromFlatParameter $ AcceptOwnership ()
  , testContract
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        bob <- newAddress auto
        replaceAddress <- newAddress auto
        withSender admin $
          call c CallDefault $ fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress)
        withSender bob $ expectCustomError_ #senderIsNotNewOwner $
          call c CallDefault $ fromFlatParameter $ AcceptOwnership ()
  , testContract
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        replaceAddress <- newAddress auto
        withSender admin $
          call c CallDefault $ fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress)
        withSender replaceAddress $ do
          call c CallDefault $ fromFlatParameter $ AcceptOwnership ()
        consumer <- originateSimple "consumer" [] contractConsumer
        call c CallDefault $ fromFlatParameter $ GetOwner $ mkView () consumer
        consumerStorage <- getStorage consumer
        assert (consumerStorage == [replaceAddress]) "Unexpected owner found"
  , testContract
      "Call to `acceptOwnership` to transferring contract from current address gets denied with `senderIsNotNewOwner` error." $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        replaceAddress <- newAddress auto
        withSender admin $
          call c CallDefault $ fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress)
        expectCustomError_ #senderIsNotNewOwner $
          call c CallDefault $ fromFlatParameter $ AcceptOwnership ()
  ]

test_setRedeemAddress :: TestTree
test_setRedeemAddress = testGroup "TZBTC contract `setRedeemAddress` test"
  [ testContract
      "Call to `setRedeemAddress` from random address is denied with `SenderIsNotOwner` error" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        replaceAddress <- newAddress auto
        bob <- newAddress auto
        withSender bob $ expectCustomError_ #senderIsNotOwner $
          call c CallDefault $ fromFlatParameter $ SetRedeemAddress (#redeem .! replaceAddress)
  , testContract
      "Call to `setRedeemAddress` sets redeem address correctly" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        replaceAddress <- newAddress auto
        withSender admin $
          call c CallDefault $ fromFlatParameter $ SetRedeemAddress (#redeem .! replaceAddress)
        consumer <- originateSimple "consumer" [] contractConsumer
        call c CallDefault $ fromFlatParameter $ GetRedeemAddress $ mkView () consumer
        consumerStorage <- getStorage $ consumer
        assert (consumerStorage == [replaceAddress]) "Updated redeem address doesn't match"
  ]

test_burn :: TestTree
test_burn = testGroup "TZBTC contract `burn` test"
  [ testContract
      "Call to `burn` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        bob <- newAddress auto
        withSender bob $ expectCustomError_ #senderIsNotOperator $
          call c CallDefault $ fromFlatParameter $ Burn (#value .! 100)
  , testContract
      "Call to `burn` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        withSender admin $ expectCustomError_ #senderIsNotOperator $
          call c CallDefault $ fromFlatParameter $ Burn (#value .! 100)
  , testContract
      "Call to `burn` from operator subtracts from redeemAddress and update bookkeeping fields" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        operator <- newAddress "operator"
        consumer <- originateSimple "consumer" [] contractConsumer

        -- Add an operator
        withSender admin $
          call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)

        call c CallDefault $ fromFlatParameter $ GetBalance (mkView (#owner .! redeemAddress_) consumer)

        withSender operator $
          call c CallDefault $ fromFlatParameter $ Burn (#value .! 130)

        call c CallDefault $ fromFlatParameter $ GetBalance (mkView (#owner .! redeemAddress_) consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalBurned (mkView () consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalSupply (mkView () consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalMinted (mkView () consumer)
        consumerStorage <- getStorage consumer
        let expectedStorage =
              [ initialSupply
              , initialSupply - 130
              , 130
              , initialSupply - 130
              , initialSupply
              ]
        assert (consumerStorage == expectedStorage) "Consumer storage doesn't match"
  ]

test_mint :: TestTree
test_mint = testGroup "TZBTC contract `mint` test"
  [ testContract
      "Call to `mint` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        alice <- newAddress auto
        bob <- newAddress auto
        withSender bob $ expectCustomError_ #senderIsNotOperator $
          call c CallDefault $ fromFlatParameter $ Mint (#to .! alice, #value .! 100)
  , testContract
      "Call to `mint` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        alice <- newAddress auto
        withSender admin $ expectCustomError_ #senderIsNotOperator $
          call c CallDefault $ fromFlatParameter $ Mint (#to .! alice, #value .! 100)
  , testContract
      "Call to `mint` from operator adds to account and update bookkeeping fields" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        operator <- newAddress "operator"
        alice <- newAddress auto
        consumer <- originateSimple "consumer" [] contractConsumer

        -- Add an operator
        withSender admin $
          call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)

        withSender operator $
          call c CallDefault (fromFlatParameter $ Mint (#to .! alice, #value .! 130))

        call c CallDefault $ fromFlatParameter $ GetBalance (mkView (#owner .! alice) consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalBurned (mkView () consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalSupply (mkView () consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalMinted (mkView () consumer)
        consumerStorage <- getStorage consumer
        let expectedStorage =
              [ initialSupply + 130
              , initialSupply + 130
              , 0
              , 130
              ]
        assert (consumerStorage == expectedStorage) "Consumer storage doesn't match"
  ]

test_approvableLedgerV1 :: TestTree
test_approvableLedgerV1 = testGroup "TZBTC contract approvable ledger tests"
  -- Our code does not comply with the recent approvable ledger spec in some
  -- minor points, so we are testing a set of pieces that cover the old behaviour
  -- of ManagedLedger .
  [ AL.alEmptyInitBalanceTest @(Parameter TZBTCv1) @(Storage TZBTCv1) alOriginate
  , AL.alTransferTest @(Parameter TZBTCv1) @(Storage TZBTCv1) alOriginate
  , AL.alTransferSelfAlwaysTest @(Parameter TZBTCv1) @(Storage TZBTCv1) alOriginate
  , AL.alAllowTest @(Parameter TZBTCv1) @(Storage TZBTCv1) alOriginate
  ]
  where
    alOriginate
      :: (MonadNettest caps base m)
      => Address -> AL.AlSettings -> m (ContractHandler (Parameter TZBTCv1) (Storage TZBTCv1))
    alOriginate = originateManagedLedger $ \op  -> do
      let owner = ML.opAdmin op
      c <- originateSimple "TZBTC Contract" (mkEmptyStorageV0 owner) tzbtcContract
      let
        opTZBTC = dummyV1Parameters redeemAddress_ (ML.opBalances op)
        upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
          { upMigrationScripts =
            Identity $
            manualConcatMigrationScripts (migrationScriptsV1 opTZBTC)
          , upNewCode = tzbtcContractRouterV1
          , upNewPermCode = emptyPermanentImpl
          }
      withSender owner $
        call c CallDefault $ fromFlatParameter $ Upgrade upgradeParams
      pure $ coerceContractHandler c

test_approvableLedgerV2 :: TestTree
test_approvableLedgerV2 = testGroup "TZBTC contract approvable ledger tests"
  [ AL.approvableLedgerGenericTest @(Parameter SomeTZBTCVersion) @(Storage SomeTZBTCVersion) $
    originateManagedLedger $ \op -> do
      let owner = ML.opAdmin op
      c <- originateSimple "TZBTC Contract" (mkEmptyStorageV0 owner) tzbtcContract
      let
        opTZBTC = dummyV2Parameters redeemAddress_ (ML.opBalances op)
        upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
          { upMigrationScripts =
            Identity $
            manualConcatMigrationScripts (migrationScriptsV2 opTZBTC)
          , upNewCode = tzbtcContractRouterV2
          , upNewPermCode = emptyPermanentImpl
          }
      withSender owner $
        call c CallDefault $ fromFlatParameter $ Upgrade upgradeParams
      pure $ coerceContractHandler c
  ]

test_pause :: TestTree
test_pause = testGroup "TZBTC contract `pause` test"
  [ testContract
      "Call to `pause` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        bob <- newAddress auto
        withSender bob $ expectCustomError_ #senderIsNotOperator $
          call c CallDefault $ fromFlatParameter $ Pause ()
  , testContract
      "Call to `pause` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        withSender admin $ expectCustomError_ #senderIsNotOperator $
          call c CallDefault $ fromFlatParameter $ Pause ()
  , testContractEmulated
      "Call to `pause` from operator sets the paused status" $
      \originateContract (_ :: Proxy ver) -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        operator <- newAddress "operator"
        -- Add an operator
        withSender admin $ call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)

        -- Call pause
        withSender operator $
          call c CallDefault $ fromFlatParameter $ Pause ()

        st <- getFullStorage c
        assert (checkField (getPaused @ver) id st) "Unexpected Paused status"
  ]

test_unpause :: TestTree
test_unpause = testGroup "TZBTC contract `unpause` test"
  [ testContract
      "Call to `unpause` from random address is denied with `SenderIsNotOwner` error" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        bob <- newAddress "bob"
        withSender bob $ expectCustomError_ #senderIsNotOwner $
          call c CallDefault $ fromFlatParameter $ Unpause ()
  , testContract
      "Call to `unpause` from operator address is denied with `SenderIsNotOwner` error" $
      \originateContract _ -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        operator <- newAddress "operator"
        -- Add an operator
        withSender admin $ call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)
        withSender operator $ expectCustomError_ #senderIsNotOwner $
          call c CallDefault $ fromFlatParameter $ Unpause ()
  , testContractEmulated
      "Call to `unpause` from owner unsets the paused status" $
      \originateContract (_ :: Proxy ver) -> do
        admin <- newAddress "admin"
        c <- originateContract admin
        operator <- newAddress "operator"
        -- Add an operator
        withSender admin $ call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)
        -- Pause the contract
        withSender operator $ call c CallDefault $ fromFlatParameter $ Pause ()
        st1 <- getFullStorage c

        assert (checkField (getPaused @ver) id st1) "Unexpected Paused status"

        -- Call unpause
        withSender admin $ call c CallDefault $ fromFlatParameter $ Unpause ()
        st2 <- getFullStorage c

        assert (checkField (getPaused @ver) Prelude.not st2) "Unexpected Paused status"
  ]

test_bookkeeping :: TestTree
test_bookkeeping = testGroup "TZBTC contract bookkeeping views test"
  [ testContract
      "calling book keeping views returns expected result" $
      \originateContract _ -> do
          admin <- newAddress "admin"
          v1 <- originateContract admin
          operator <- newAddress "operator"
          alice <- newAddress "alice"
          -- Add an operator
          withSender admin $ call v1 CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)
          withSender operator $ do
            -- Mint and burn some tokens
            call v1 CallDefault (fromFlatParameter $ Mint (#to .! alice, #value .! 130))
            call v1 CallDefault (fromFlatParameter $ Burn (#value .! 20))

          consumer <- originateSimple "consumer" [] contractConsumer

          call v1 CallDefault $ fromFlatParameter $ GetTotalSupply (mkView () consumer)
          call v1 CallDefault $ fromFlatParameter $ GetTotalMinted (mkView () consumer)
          call v1 CallDefault $ fromFlatParameter $ GetTotalBurned (mkView () consumer)
          -- Check expectations
          natConsumerStorage <- getStorage consumer
          assert (natConsumerStorage == [20, 630, 610 :: Natural]) "Unexpected nat consumer storage"

          -- Check redeem address getter (we need another consumer
          -- because the type is different).
          withSender admin $ call v1 CallDefault (fromFlatParameter $ SetRedeemAddress (#redeem .! operator))
          consumerAddr <- originateSimple "consumerAddr" [] contractConsumer
          call v1 CallDefault $ fromFlatParameter $ GetRedeemAddress (mkView () consumerAddr)
          addrConsumerStorage <- getStorage consumerAddr
          assert (addrConsumerStorage == [operator]) "Unexpected address consumer storage"
  ]

test_get_meta :: TestTree
test_get_meta =
  testContractEmulated "Can get TZBTC metadata" $
    \originateContract _ -> do
      admin <- newAddress "admin"
      v1 <- originateContract admin
      consumer <- originateSimple "consumer" [] contractConsumer
      call v1 CallDefault $ fromFlatParameter $ GetTokenMetadata (mkView [singleTokenTokenId] consumer)
      consumerStorage <- getFullStorage consumer
      assert (consumerStorage == [[defaultTZBTCMetadata]]) "Unexpected metadata"

test_documentation :: [TestTree]
test_documentation =
  [ testGroup "V1" $
      runDocTests testLorentzDoc V1.tzbtcDoc
  , testGroup "V2" $
      runDocTests testLorentzDoc V2.tzbtcDoc
  ]
