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
  , checkField
  , dummyV1Parameters
  , originateTzbtcV1ContractRaw
  ) where

import Data.Coerce (coerce)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text.IO.Utf8 as Utf8
import Test.HUnit (assertFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.HUnit (testCase)

import Lorentz hiding (assert)
import Lorentz.Contracts.Metadata
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import qualified Lorentz.Contracts.Test.ApprovableLedger as AL
import Lorentz.Contracts.Test.ManagedLedger (OriginationParams(..), originateManagedLedger)
import qualified Lorentz.Contracts.Test.ManagedLedger as ML
import Lorentz.Contracts.Upgradeable.Common (EpwUpgradeParameters(..), emptyPermanentImpl)
import Lorentz.Test
  (contractConsumer, expectContractEntrypoints, genesisAddress1, lCallDef, lOriginate,
   runDocTests, testLorentzDoc)
import qualified Lorentz.Test as L (withSender)
import Lorentz.UStore
import Lorentz.UStore.Migration
import Michelson.Runtime (parseExpandContract)
import Michelson.Test.Unit (matchContractEntrypoints, mkEntrypointsMap)
import Michelson.Typed.Convert (convertContract)
import Morley.Nettest
import Morley.Nettest.Tasty
import qualified Michelson.Untyped as U
import Util.Named

import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.V1 as V1
import qualified Lorentz.Contracts.TZBTC.V2 as V2
import Test.Smoke (TestUpgrade(..), testUpgradeToV1, testUpgradeToV2)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Convert sane parameter to parameter of this contract.
-- Helpers to check storage state.
checkStorage_
  :: (StoreTemplateV1 -> Bool)
  -> TZBTCStorage -> Bool
checkStorage_ fn st =
  case ustoreDecomposeFull $ dataMap st of
    Right template -> fn template
    Left _ -> False

checkField
  :: (StoreTemplateV1 -> UStoreField a)
  -> (a -> Bool)
  -> (TZBTCStorage -> Bool)
checkField ef cf = checkStorage_ (\st -> cf $ unUStoreField $ ef st)

dummyV1Parameters :: Address -> Map Address Natural -> V1Parameters
dummyV1Parameters redeem balances = V1Parameters
  { v1RedeemAddress = redeem
  , v1TokenMetadata = defaultTZBTCMetadata
  , v1Balances = balances
  }

dummyV2Parameters :: Address -> Map Address Natural -> V2Parameters
dummyV2Parameters = dummyV1Parameters


redeemAddress_ :: Address
redeemAddress_ = genesisAddress1

originateTzbtcV1ContractRaw
  :: (HasCallStack, MonadNettest caps base m)
  => Address -> OriginationParams -> m (TAddress (Parameter SomeTZBTCVersion))
originateTzbtcV1ContractRaw redeem op = do
  let owner = ML.opAdmin op
      balances = ML.opBalances op
  c <- originateSimple "TZBTC Contract" (mkEmptyStorageV0 owner) tzbtcContract
  unTestUpgrade testUpgradeToV1 owner redeem balances c
  pure $ coerce c

originateTzbtcV1Contract
  :: (HasCallStack, MonadNettest caps base m)
  => m (TAddress (Parameter SomeTZBTCVersion))
originateTzbtcV1Contract = do
  nettestAddr <- resolveNettestAddress
  originateTzbtcV1ContractRaw redeemAddress_ $ OriginationParams
    { opAdmin = nettestAddr
    , opBalances = M.fromList [(redeemAddress_, initialSupply)]
    }

originateTzbtcV2ContractRaw
  :: (HasCallStack, MonadNettest caps base m)
  => Address -> OriginationParams -> m (TAddress (Parameter SomeTZBTCVersion))
originateTzbtcV2ContractRaw redeem op = do
  let owner = ML.opAdmin op
      balances = ML.opBalances op
  c <- originateSimple "TZBTC Conctact" (mkEmptyStorageV0 owner) tzbtcContract
  unTestUpgrade testUpgradeToV2 owner redeem balances c
  pure $ coerce c

originateTzbtcV2Contract
  :: (HasCallStack, MonadNettest caps base m)
  => m (TAddress (Parameter SomeTZBTCVersion))
originateTzbtcV2Contract = do
  nettestAddr <- resolveNettestAddress
  originateTzbtcV2ContractRaw redeemAddress_ $ OriginationParams
    { opAdmin = nettestAddr
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
  -> (forall m caps base. (MonadNettest caps base m) => m (TAddress (Parameter SomeTZBTCVersion)) -> m ())
  -> TestTree
testContract name testSuite =
  testGroup name $
    [ nettestScenarioCaps "V1 contract" $ testSuite originateTzbtcV1Contract
    , nettestScenarioCaps "V2 contract" $ testSuite originateTzbtcV2Contract
    ]

testContractEmulated
  :: String
  -> (forall m caps base. (MonadEmulated caps base m) => m (TAddress (Parameter SomeTZBTCVersion)) -> m ())
  -> TestTree
testContractEmulated name testSuite =
  testGroup name $
    [ nettestScenarioOnEmulatorCaps "V1 contract" $ testSuite originateTzbtcV1Contract
    , nettestScenarioOnEmulatorCaps "V2 contract" $ testSuite originateTzbtcV2Contract
    ]

test_interface :: TestTree
test_interface = testGroup "TZBTC consistency test"
  [ testCase
      "Has an approvable ledger interface that satisfies FA1.2 specification" $ do
        expectContractEntrypoints @AL.Parameter tzbtcContract

  , testCase
      "Has the expected interface of TZBTC contract" $ do
        reference <- entrypointsRef
        let untypedTzbtc = convertContract . compileLorentzContract $ tzbtcContract
        case matchContractEntrypoints untypedTzbtc reference of
          Right _ -> pass
          Left missing -> do
            assertFailure $ "Some entrypoints were not found:" <> (show missing)
  ]

test_addOperator :: TestTree
test_addOperator = testGroup "TZBTC contract `addOperator` test"
  [ testContract
      "Call to `addOperator` from random address gets denied with `senderIsNotOwner` error." $
      \originateContract -> do
        c <- originateContract
        bob <- AddressResolved <$> newAddress "bob"
        operator <- newAddress "operator"
        withSender bob $
          expectCustomError_ #senderIsNotOwner c $ call c CallDefault $
          fromFlatParameter $ AddOperator (#operator .! operator)
  , testContractEmulated
      "Call to `addOperator` from owner adds operator to the set." $
      \originateContract -> do
        c <- originateContract
        operator <- newAddress "operator"
        call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)
        st <- getStorage' $ AddressResolved $ toAddress c
        assert (checkField (operators . stCustom) (Set.member operator) $ fromVal st)
          "New operator not found"
  ]

test_removeOperator :: TestTree
test_removeOperator = testGroup "TZBTC contract `addOperator` test"
  [ testContract
      "Call to `removeOperator` from random address gets denied with `senderIsNotOwner` error." $
      \originateContract -> do
        c <- originateContract
        bob <- AddressResolved <$> newAddress "bob"
        operator <- newAddress "operator"
        withSender bob $
          expectCustomError_ #senderIsNotOwner c $
          call c CallDefault $ fromFlatParameter $ RemoveOperator (#operator .! operator)

  , testContractEmulated
      "Call to `removeOperator` from owner removes operator from the set." $
      \originateContract -> do
        c <- originateContract
        operator <- newAddress "operator"
        call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)
        st1 <- getStorage' $ AddressResolved $ toAddress c
        assert (checkField (operators . stCustom) (Set.member operator) $ fromVal st1)
          "New operator not found"
        call c CallDefault $ fromFlatParameter $ RemoveOperator (#operator .! operator)
        st2 <- getStorage' $ AddressResolved $ toAddress c
        assert (checkField (operators . stCustom) (Prelude.not . Set.member operator) $ fromVal st2)
          "Unexpectedly found operator"
  ]

test_transferOwnership :: TestTree
test_transferOwnership = testGroup "TZBTC contract `transferOwnership` test"
  [ testContract
      "Call to `transferOwnership` from random address gets denied with `senderIsNotOwner` error." $
      \originateContract -> do
        c <- originateContract
        bob <- AddressResolved <$> newAddress "bob"
        replaceAddress <- newAddress "replaceAddress"
        withSender bob $ expectCustomError_ #senderIsNotOwner c $
          call c CallDefault $ fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress)
  , testContractEmulated
      "Call to `transferOwnership` from owner address gets denied with `senderIsNotOwner` error." $
      \originateContract -> do
        c <- originateContract
        replaceAddress <- newAddress "replaceAddress"
        call c CallDefault $ fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress)
        st <- getStorage' $ AddressResolved $ toAddress c
        assert (checkField (newOwner . stCustom) (== Just replaceAddress) $ fromVal st)
          "Expected `newOwner` not found"
  ]

test_acceptOwnership :: TestTree
test_acceptOwnership = testGroup "TZBTC contract `acceptOwnership` test"
  [ testContract
      "Call to `acceptOwnership` to non-transfering contract gets denied with `notInTransferOwnershipMode` error " $
      \originateContract -> do
        c <- originateContract
        bob <- AddressResolved <$> newAddress "bob"
        withSender bob $ expectCustomError_ #notInTransferOwnershipMode c $
          call c CallDefault $ fromFlatParameter $ AcceptOwnership ()
  , testContract
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      \originateContract -> do
        c <- originateContract
        bob <- AddressResolved <$> newAddress "bob"
        replaceAddress <- newAddress "replaceAddress"
        call c CallDefault $ fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress)
        withSender bob $ expectCustomError_ #senderIsNotNewOwner c $
          call c CallDefault $ fromFlatParameter $ AcceptOwnership ()
  , testContract
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      \originateContract -> do
        c <- originateContract
        replaceAddress <- newAddress "replaceAddress"
        call c CallDefault $ fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress)
        withSender (AddressResolved replaceAddress) $ do
          call c CallDefault $ fromFlatParameter $ AcceptOwnership ()
        consumer <- originateSimple "consumer" [] contractConsumer
        call c CallDefault $ fromFlatParameter $ GetOwner $ mkView () consumer
        checkStorage (AddressResolved $ toAddress consumer) $ toVal [replaceAddress]
  , testContract
      "Call to `acceptOwnership` to transferring contract from current address gets denied with `senderIsNotNewOwner` error." $
      \originateContract -> do
        c <- originateContract
        replaceAddress <- newAddress "replaceAddress"
        call c CallDefault $ fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress)
        expectCustomError_ #senderIsNotNewOwner c $
          call c CallDefault $ fromFlatParameter $ AcceptOwnership ()
  ]

test_setRedeemAddress :: TestTree
test_setRedeemAddress = testGroup "TZBTC contract `setRedeemAddress` test"
  [ testContract
      "Call to `setRedeemAddress` from random address is denied with `SenderIsNotOwner` error" $
      \originateContract -> do
        c <- originateContract
        replaceAddress <- newAddress "replaceAddress"
        bob <- AddressResolved <$> newAddress "bob"
        withSender bob $ expectCustomError_ #senderIsNotOwner c $
          call c CallDefault $ fromFlatParameter $ SetRedeemAddress (#redeem .! replaceAddress)
  , testContract
      "Call to `setRedeemAddress` sets redeem address correctly" $
      \originateContract -> do
        c <- originateContract
        replaceAddress <- newAddress "replaceAddress"
        call c CallDefault $ fromFlatParameter $ SetRedeemAddress (#redeem .! replaceAddress)
        consumer <- originateSimple "consumer" [] contractConsumer
        call c CallDefault $ fromFlatParameter $ GetRedeemAddress $ mkView () consumer
        checkStorage (AddressResolved $ toAddress consumer) $ toVal [replaceAddress]
  ]

test_burn :: TestTree
test_burn = testGroup "TZBTC contract `burn` test"
  [ testContract
      "Call to `burn` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract -> do
        c <- originateContract
        bob <- AddressResolved <$> newAddress "bob"
        withSender bob $ expectCustomError_ #senderIsNotOperator c $
          call c CallDefault $ fromFlatParameter $ Burn (#value .! 100)
  , testContract
      "Call to `burn` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract -> do
        c <- originateContract
        expectCustomError_ #senderIsNotOperator c $
          call c CallDefault $ fromFlatParameter $ Burn (#value .! 100)
  , testContract
      "Call to `burn` from operator subtracts from redeemAddress and update bookkeeping fields" $
      \originateContract -> do
        c <- originateContract
        operator <- newAddress "operator"
        consumer <- originateSimple "consumer" [] contractConsumer

        -- Add an operator
        call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)

        call c CallDefault $ fromFlatParameter $ GetBalance (mkView (#owner .! redeemAddress_) consumer)

        withSender (AddressResolved operator) $
          call c CallDefault $ fromFlatParameter $ Burn (#value .! 130)

        call c CallDefault $ fromFlatParameter $ GetBalance (mkView (#owner .! redeemAddress_) consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalBurned (mkView () consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalSupply (mkView () consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalMinted (mkView () consumer)
        checkStorage (AddressResolved $ toAddress consumer) $ toVal
          [ initialSupply
          , initialSupply - 130
          , 130
          , initialSupply - 130
          , initialSupply
          ]
  ]

test_mint :: TestTree
test_mint = testGroup "TZBTC contract `mint` test"
  [ testContract
      "Call to `mint` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract -> do
        c <- originateContract
        alice <- newAddress "alice"
        bob <- AddressResolved <$> newAddress "bob"
        withSender bob $ expectCustomError_ #senderIsNotOperator c $
          call c CallDefault $ fromFlatParameter $ Mint (#to .! alice, #value .! 100)
  , testContract
      "Call to `mint` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract -> do
        c <- originateContract
        alice <- newAddress "alice"
        expectCustomError_ #senderIsNotOperator c $
          call c CallDefault $ fromFlatParameter $ Mint (#to .! alice, #value .! 100)
  , testContract
      "Call to `mint` from operator adds to account and update bookkeeping fields" $
      \originateContract -> do
        c <- originateContract
        operator <- newAddress "operator"
        alice <- newAddress "alice"
        consumer <- originateSimple "consumer" [] contractConsumer

        -- Add an operator
        call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)

        withSender (AddressResolved operator) $
          call c CallDefault (fromFlatParameter $ Mint (#to .! alice, #value .! 130))

        call c CallDefault $ fromFlatParameter $ GetBalance (mkView (#owner .! alice) consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalBurned (mkView () consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalSupply (mkView () consumer)
        call c CallDefault $ fromFlatParameter $ GetTotalMinted (mkView () consumer)
        checkStorage (AddressResolved $ toAddress consumer) $ toVal
          [ initialSupply + 130
          , initialSupply + 130
          , 0
          , 130
          ]
  ]

test_approvableLedgerV1 :: IO TestTree
test_approvableLedgerV1 = testSpec "TZBTC contract approvable ledger tests" $ do
  -- Our code does not comply with the recent approvable ledger spec in some
  -- minor points, so we are testing a set of pieces that cover the old behaviour
  -- of ManagedLedger .
  AL.alEmptyInitBalanceSpec alOriginate
  AL.alTransferSpec alOriginate
  AL.alTransferSelfAlwaysSpec alOriginate
  AL.alAllowSpec alOriginate
  where
    alOriginate = originateManagedLedger @(Parameter SomeTZBTCVersion) $ \op -> do
      let owner = ML.opAdmin op
      c <- lOriginate tzbtcContract "TZBTC Contract"
        (mkEmptyStorageV0 owner) (toMutez 1000)
      let
        opTZBTC = dummyV1Parameters redeemAddress_ (ML.opBalances op)
        upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
          { upMigrationScripts =
            Identity $
            manualConcatMigrationScripts (migrationScriptsV1 opTZBTC)
          , upNewCode = tzbtcContractRouterV1
          , upNewPermCode = emptyPermanentImpl
          }
      L.withSender owner $ lCallDef c (fromFlatParameter $ Upgrade upgradeParams)
      pure $ coerce c

test_approvableLedgerV2 :: IO TestTree
test_approvableLedgerV2 = testSpec "TZBTC contract approvable ledger tests" $ do
  AL.approvableLedgerSpec $ originateManagedLedger @(Parameter SomeTZBTCVersion) $ \op -> do
    let owner = ML.opAdmin op
    c <- lOriginate tzbtcContract "TZBTC Contract"
      (mkEmptyStorageV0 owner) (toMutez 1000)
    let
      opTZBTC = dummyV2Parameters redeemAddress_ (ML.opBalances op)
      upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
        { upMigrationScripts =
          Identity $
          manualConcatMigrationScripts (migrationScriptsV2 opTZBTC)
        , upNewCode = tzbtcContractRouterV2
        , upNewPermCode = emptyPermanentImpl
        }
    L.withSender owner $ lCallDef c (fromFlatParameter $ Upgrade upgradeParams)
    pure $ coerce c

test_pause :: TestTree
test_pause = testGroup "TZBTC contract `pause` test"
  [ testContract
      "Call to `pause` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract -> do
        c <- originateContract
        bob <- AddressResolved <$> newAddress "bob"
        withSender bob $ expectCustomError_ #senderIsNotOperator c $
          call c CallDefault $ fromFlatParameter $ Pause ()
  , testContract
      "Call to `pause` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract -> do
        c <- originateContract
        expectCustomError_ #senderIsNotOperator c $
          call c CallDefault $ fromFlatParameter $ Pause ()
  , testContractEmulated
      "Call to `pause` from operator sets the paused status" $
      \originateContract -> do
        c <- originateContract
        operator <- newAddress "operator"
        -- Add an operator
        call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)

        -- Call pause
        withSender (AddressResolved operator) $
          call c CallDefault $ fromFlatParameter $ Pause ()

        st <- getStorage' $ AddressResolved $ toAddress c
        assert (checkField (paused . stCustom) id $ fromVal st) "Unexpected Paused status"
  ]

test_unpause :: TestTree
test_unpause = testGroup "TZBTC contract `unpause` test"
  [ testContract
      "Call to `unpause` from random address is denied with `SenderIsNotOwner` error" $
      \originateContract -> do
        c <- originateContract
        bob <- AddressResolved <$> newAddress "bob"
        withSender bob $ expectCustomError_ #senderIsNotOwner c $
          call c CallDefault $ fromFlatParameter $ Unpause ()
  , testContract
      "Call to `unpause` from operator address is denied with `SenderIsNotOwner` error" $
      \originateContract -> do
        c <- originateContract
        operator <- newAddress "operator"
        -- Add an operator
        call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)
        withSender (AddressResolved operator) $ expectCustomError_ #senderIsNotOwner c $
          call c CallDefault $ fromFlatParameter $ Unpause ()
  , testContractEmulated
      "Call to `unpause` from owner unsets the paused status" $
      \originateContract -> do
        c <- originateContract
        operator <- newAddress "operator"
        -- Add an operator
        call c CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)
        -- Pause the contract
        withSender (AddressResolved operator) $ call c CallDefault $ fromFlatParameter $ Pause ()
        st1 <- getStorage' $ AddressResolved $ toAddress c

        assert (checkField (paused . stCustom) id $ fromVal st1) "Unexpected Paused status"

        -- Call unpause
        call c CallDefault $ fromFlatParameter $ Unpause ()
        st2 <- getStorage' $ AddressResolved $ toAddress c

        assert (checkField (paused . stCustom) Prelude.not $ fromVal st2) "Unexpected Paused status"
  ]

test_bookkeeping :: TestTree
test_bookkeeping = testGroup "TZBTC contract bookkeeping views test"
  [ testContract
      "calling book keeping views returns expected result" $
      \originateContract -> do
          v1 <- originateContract
          operator <- newAddress "operator"
          alice <- newAddress "alice"
          -- Add an operator
          call v1 CallDefault $ fromFlatParameter $ AddOperator (#operator .! operator)
          withSender (AddressResolved operator) $ do
            -- Mint and burn some tokens
            call v1 CallDefault (fromFlatParameter $ Mint (#to .! alice, #value .! 130))
            call v1 CallDefault (fromFlatParameter $ Burn (#value .! 20))

          consumer <- originateSimple "consumer" [] contractConsumer

          call v1 CallDefault $ fromFlatParameter $ GetTotalSupply (mkView () consumer)
          call v1 CallDefault $ fromFlatParameter $ GetTotalMinted (mkView () consumer)
          call v1 CallDefault $ fromFlatParameter $ GetTotalBurned (mkView () consumer)
          -- Check expectations
          checkStorage (AddressResolved $ toAddress consumer) $ toVal [20, 630, 610 :: Natural]

          -- Check redeem address getter (we need another consumer
          -- because the type is different).
          call v1 CallDefault (fromFlatParameter $ SetRedeemAddress (#redeem .! operator))
          consumerAddr <- originateSimple "consumerAddr" [] contractConsumer
          call v1 CallDefault $ fromFlatParameter $ GetRedeemAddress (mkView () consumerAddr)
          checkStorage (AddressResolved $ toAddress consumerAddr) $ toVal [operator]
  ]

test_get_meta :: TestTree
test_get_meta =
  testContract "Can get TZBTC metadata" $
    \originateContract -> do
      v1 <- originateContract
      consumer <- originateSimple "consumer" [] contractConsumer
      call v1 CallDefault $ fromFlatParameter $ GetTokenMetadata (mkView [singleTokenTokenId] consumer)
      checkStorage (AddressResolved $ toAddress consumer) $ toVal [[defaultTZBTCMetadata]]

test_documentation :: [TestTree]
test_documentation =
  [ testGroup "V1" $
      runDocTests testLorentzDoc V1.tzbtcDoc
  , testGroup "V2" $
      runDocTests testLorentzDoc V2.tzbtcDoc
  ]
