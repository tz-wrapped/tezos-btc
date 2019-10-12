{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.TZBTC
  ( test_acceptOwnership
  , test_burn
  , test_migration
  , test_mint
  , test_pause
  , test_setProxy
  , test_setRedeemAddress
  , test_transferOwnership
  , test_unpause_
  , test_bookkeeping
  , test_proxyCheck

  , test_addOperator
  , test_removeOperator
  ) where

import Fmt (pretty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertBool, assertFailure, testCase)
import qualified Data.Map as Map
import Data.Set
import Data.Singletons (SingI(..))
import qualified Data.Set as Set
import Named (arg)

import Lorentz
import Lorentz.Contracts.Consumer
import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.Agent as Agent
import Lorentz.Contracts.TZBTC.Types
import Lorentz.Test.Integrational
import Lorentz.UStore.Types (UStore(..))
import Michelson.Interpret (ContractEnv(..), MichelsonFailed(..))
import Michelson.Interpret.Unpack (dummyUnpackEnv)
import Michelson.Test
  ( ContractPropValidator, contractProp, dummyContractEnv)
import Michelson.Text (mt)
import Michelson.Typed (Instr, InstrWrapC, AppendCtorField, GetCtorField, ToTs, Value, Value'(..))
import Michelson.Typed.Scope (checkOpPresence, OpPresence(..))
import Util.Named

import qualified Lorentz.Contracts.TZBTC.V0 as V0
import qualified Lorentz.Contracts.TZBTC.V1 as V1

-- A helper to check storage state.
checkStorage
  :: (V1.StoreTemplate -> Either ValidationError ())
  -> V0.Storage -> Either ValidationError ()
checkStorage fn st =
  case ustoreDecomposeFull dummyUnpackEnv $ UStore $ V0.dataMap st of
    Right template -> fn template
    Left err -> error err

checkField
  :: (V1.StoreTemplate -> UStoreField a)
  -> (a -> Bool)
  -> Text
  -> (V0.Storage -> Either ValidationError ())
checkField ef cf ms = checkStorage (\st ->
  if cf $ unUStoreField $ ef st
  then Right ()
  else Left $ CustomValidationError ms)

v0EmptyStorage = V0.mkEmptyStorage adminAddress

tzbtcV1Contract :: IntegrationalScenarioM (ContractAddr (V0.Parameter '[]))
tzbtcV1Contract = do
  c <- lOriginate V0.tzbtcContract "TZBTC Contract" v0EmptyStorage (toMutez 1000)
  let
    op = V1.originationParams adminAddress redeemAddress_ initialSupply
    upgradeParams =
      ( #newVersion .! 1
      , #migrationScript .! (Prelude.foldl' (#) nop $ V1.migrationScripts op)
      , #newCode V1.tzbtcContractCode
      )
  withSender adminAddress $ lCall c (V0.Upgrade  upgradeParams)
  pure c

---- New Tests

-- Add/Remove Operator

test_addOperator :: TestTree
test_addOperator = testGroup "TZBTC contract `addOperator` test"
  [ testCase
      "Call to `addOperator` from random address gets denied with `senderIsNotAdmin` error." $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender bob $ do
          lCall c (V0.AddOperator (#operator .! newOperatorAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin
  , testCase
      "Call to `addOperator` from admin adds operator to the set." $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender adminAddress $ do
          lCall c (V0.AddOperator (#operator .! newOperatorAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField V1.operators
              (Set.member newOperatorAddress) "New operator not found")
  ]

test_removeOperator :: TestTree
test_removeOperator = testGroup "TZBTC contract `addOperator` test"
  [ testCase
      "Call to `removeOperator` from random address gets denied with `senderIsNotAdmin` error." $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender bob $ do
          lCall c (V0.RemoveOperator (#operator .! newOperatorAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin

  , testCase
      "Call to `removeOperator` from admin removes operator from the set." $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender adminAddress $ do
          lCall c (V0.AddOperator (#operator .! newOperatorAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField V1.operators
              (Set.member newOperatorAddress) "New operator not found")
        withSender adminAddress $ do
          lCall c (V0.RemoveOperator (#operator .! newOperatorAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField V1.operators
              (Prelude.not . Set.member newOperatorAddress) "Unexpectedly found operator")
  ]

-- Transfer/Accept ownership

test_transferOwnership :: TestTree
test_transferOwnership = testGroup "TZBTC contract `transferOwnership` test"
  [ testCase
      "Call to `transferOwnership` from random address gets denied with `senderIsNotAdmin` error." $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender bob $ do
          lCall c (V0.TransferOwnership (#newOwner .! replaceAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin
  , testCase
      "Call to `transferOwnership` from admin address gets denied with `senderIsNotAdmin` error." $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender adminAddress $ do
          lCall c (V0.TransferOwnership (#newOwner .! replaceAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField V1.newOwner (== Just replaceAddress)  "Expected `newOwner` not found")
  ]

test_acceptOwnership :: TestTree
test_acceptOwnership = testGroup "TZBTC contract `acceptOwnership` test"
  [ testCase
      "Call to `acceptOwnership` to non-transfering contract gets denied with `notInTransferOwnershipMode` error " $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender bob $ do
          lCall c (V0.AcceptOwnership ())
        validate . Left $
          lExpectCustomError_ #notInTransferOwnershipMode
  , testCase
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender adminAddress $ do
          lCall c (V0.TransferOwnership (#newOwner .! replaceAddress))
        withSender bob $ do
          lCall c (V0.AcceptOwnership ())
        validate . Left $
          lExpectCustomError_ #senderIsNotNewOwner
  , testCase
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender adminAddress $ do
          lCall c (V0.TransferOwnership (#newOwner .! replaceAddress))
        withSender replaceAddress $ do
          lCall c (V0.AcceptOwnership ())
        validate . Right $
          lExpectStorageUpdate c (checkField V1.admin (== replaceAddress)  "Expected `admin` not found")
  , testCase
      "Call to `acceptOwnership` to transferring contract from current address gets denied with `senderIsNotNewOwner` error." $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender adminAddress $ do
          lCall c (V0.TransferOwnership (#newOwner .! replaceAddress))
        withSender adminAddress $ do
          lCall c (V0.AcceptOwnership ())
        validate . Left $
          lExpectCustomError_ #senderIsNotNewOwner
  ]

test_setRedeemAddress :: TestTree
test_setRedeemAddress = testGroup "TZBTC contract `setRedeemAddress` test"
  [ testCase
      "Call to `setRedeemAddress` from random address is denied with `SenderIsNotAdmin` error" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender bob $ do
          lCall c (V0.SetRedeemAddress (#redeem .! replaceAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin
  , testCase
      "Call to `setRedeemAddress` sets redeem address correctly" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender adminAddress $ do
          lCall c (V0.SetRedeemAddress (#redeem .! replaceAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField
              V1.redeemAddress (== replaceAddress) "Unexpected redeem address")
  ]

test_burn :: TestTree
test_burn = testGroup "TZBTC contract `burn` test"
  [ testCase
      "Call to `burn` from random address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender bob $ do
          lCall c (V0.Burn (#value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `burn` from admin address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender adminAddress $ do
          lCall c (V0.Burn (#value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `burn` from operator subtracts from redeemAddress and update bookkeeping fields" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        consumer <- lOriginateEmpty contractConsumer "consumer"

        -- Add an operator
        withSender adminAddress $ do
          lCall c (V0.AddOperator (#operator .! newOperatorAddress))

        lCall c $ V0.Views $ V0.GetBalance (View redeemAddress_ consumer)

        withSender newOperatorAddress $ lCall c (V0.Burn (#value .! 130))

        lCall c $ V0.Views $ V0.GetBalance (View redeemAddress_ consumer)
        lCall c $ V0.Views $ V0.GetTotalBurned (View () consumer)
        lCall c $ V0.Views $ V0.GetTotalSupply (View () consumer)
        lCall c $ V0.Views $ V0.GetTotalMinted (View () consumer)
        validate . Right $
          lExpectViewConsumerStorage consumer
            [ initialSupply
            , initialSupply - 130
            , 130
            , initialSupply - 130
            , initialSupply
            ]
  ]

test_mint :: TestTree
test_mint = testGroup "TZBTC contract `mint` test"
  [ testCase
      "Call to `mint` from random address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender bob $ do
          lCall c (V0.Mint (#to .! alice, #value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `mint` from admin address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender adminAddress $ do
          lCall c (V0.Mint (#to .! alice, #value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `mint` from operator adds to account and update bookkeeping fields" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        consumer <- lOriginateEmpty contractConsumer "consumer"

        -- Add an operator
        withSender adminAddress $ do
          lCall c (V0.AddOperator (#operator .! newOperatorAddress))

        withSender newOperatorAddress $ lCall c (V0.Mint (#to .! alice, #value .! 130))

        lCall c $ V0.Views $ V0.GetBalance (View alice consumer)
        lCall c $ V0.Views $ V0.GetTotalBurned (View () consumer)
        lCall c $ V0.Views $ V0.GetTotalSupply (View () consumer)
        lCall c $ V0.Views $ V0.GetTotalMinted (View () consumer)
        validate . Right $
          lExpectViewConsumerStorage consumer
            [ 130
            , 0
            , initialSupply + 130
            , initialSupply + 130
            ]
  ]

-- New Tests End
lContract :: Instr (ToTs '[(Parameter, Storage)]) (ToTs (ContractOut Storage))
lContract = (unI tzbtcContract)

newOperatorAddress :: Address
newOperatorAddress = genesisAddress1

adminAddress :: Address
adminAddress = genesisAddress3

redeemAddress_ :: Address
redeemAddress_ = adminAddress

replaceAddress :: Address
replaceAddress = genesisAddress2

contractAddress :: Address
contractAddress = genesisAddress4

alice :: Address
alice = genesisAddress6

bob :: Address
bob = genesisAddress5

initialSupply :: Natural
initialSupply = 500

storage :: Storage
storage =
  mkStorage adminAddress redeemAddress_
    (Map.fromList [(redeemAddress_, initialSupply)]) mempty

contractPropWithSender
  :: Address
  -> ContractPropValidator (ToT Storage) prop
  -> Parameter
  -> Storage
  -> prop
contractPropWithSender address_ check param initSt =
  contractProp lContract check
    (dummyContractEnv { ceSender = address_ })
    param
    initSt

assertFailureMessage
  :: Either MichelsonFailed ([Operation], Value (ToT Storage))
  -> MText
  -> String
  -> Assertion
assertFailureMessage res msg tstMsg = case res of
  Right (_, _) ->
    assertFailure "Contract did not fail as expected"
  Left err -> case err of
    MichelsonFailedWith (VPair ((VC (CvString t)), _)) -> do
      assertEqual tstMsg msg t
    a -> assertFailure $ "Unexpected contract failure: " <> pretty a

test_proxyCheck :: TestTree
test_proxyCheck = testGroup "TZBTC contract proxy endpoints check"
  [ testCase
      "Fails with `ProxyIsNotSet` if one of the proxy serving endpoints is called and \
      \proxy is not set" $
      contractPropWithSender bob validate'
        (EntrypointsWithoutView $
         TransferViaProxy (#sender .! bob, (#from .! bob, #to .! alice, #value .! 100))) storage
  , testCase
      "Fails with `CallerIsNotProxy` if the caller to a proxy endpoint is not \
      \known proxy address." $
      integrationalTestExpectation $ do
        c <- lOriginate tzbtcContract "TZBTC Contract" storage (toMutez 1000)
        withSender adminAddress $ lCall c (EntrypointsWithoutView $
                                           SetProxy contractAddress)
        withSender bob $
          lCall c (EntrypointsWithoutView $
                   TransferViaProxy (#sender .! bob, (#from .! bob, #to .! alice, #value .! 100)))
        validate . Left $
          lExpectCustomError_ #callerIsNotProxy
  ]
  where
    validate' :: ContractPropValidator (ToT Storage) Assertion
    validate' (res, _) =
      assertFailureMessage
        res [mt|ProxyIsNotSet|]
        "Contract did not fail with 'ProxyIsNotSet' message"

test_pause :: TestTree
test_pause = testGroup "TZBTC contract `pause` permission test"
  [ testCase
      "Call to `pause` from admin gets denied with `SenderIsNotOperator`" $
      contractPropWithSender adminAddress
        validateFail_ (EntrypointsWithoutView $ Pause ()) storageWithOperator
  , testCase
      "Call to `pause` from random address gets denied with `SenderIsNotOperator`" $
      contractPropWithSender bob
        validateFail_ (EntrypointsWithoutView $ Pause ()) storageWithOperator
  , testCase
      "Call to `pause` as operator is allowed" $
      contractPropWithSender newOperatorAddress
        validate_ (EntrypointsWithoutView $ Pause ()) storageWithOperator
  ]
  where
    storageWithOperator =
      mkStorage adminAddress redeemAddress_
        (Map.fromList [(redeemAddress_, initialSupply)])
        (Set.fromList [newOperatorAddress])
    validateFail_ :: ContractPropValidator (ToT Storage) Assertion
    validateFail_ (res, _) =
      assertFailureMessage
        res [mt|SenderIsNotOperator|]
          "Contract did not fail with 'SenderIsNotOperator' message"

    validate_ :: ContractPropValidator (ToT Storage) Assertion
    validate_ (res, _) =
      case res of
        Left err -> assertFailure $ "Unexpected contract failure: " <> pretty err
        Right (_operations, rstorage) ->
          assertEqual
            "Contract's `pause` operation executed with out error"
             True
             (paused $ fields $ (fromVal rstorage :: Storage))

test_unpause_ :: TestTree
test_unpause_ = testGroup "TZBTC contract `unpause` permission test"
  [ testCase
      "Call to `unpause` as operator gets denied with `SenderIsNotAdmin`" $
      contractPropWithSender newOperatorAddress
        validateFail_ (EntrypointsWithoutView $ Unpause ()) storageWithOperator
  , testCase
      "Call to `unpause` from random address gets denied with `SenderIsNotAdmin`" $
      contractPropWithSender bob
        validateFail_ (EntrypointsWithoutView $ Unpause ()) storageWithOperator
  , testCase
      "Call to `unpause` as admin is allowed" $
      contractPropWithSender adminAddress
        validate_ (EntrypointsWithoutView $ Unpause ()) storageWithOperator
  ]
  where
    storageWithOperator =
      mkStorage adminAddress redeemAddress_
        (Map.fromList [(redeemAddress_, initialSupply)])
        (Set.fromList [newOperatorAddress])
    validateFail_ :: ContractPropValidator (ToT Storage) Assertion
    validateFail_ (res, _) =
      assertFailureMessage
        res [mt|SenderIsNotAdmin|]
        "Contract did not fail with 'SenderIsNotAdmin' message"

    validate_ :: ContractPropValidator (ToT Storage) Assertion
    validate_ (res, _) =
      case res of
        Left err -> assertFailure $ "Unexpected contract failure: " <> pretty err
        Right (_operations, rstorage) ->
          assertEqual
            "Contract's `unpause` operation executed with out error"
             False
             (paused $ fields $ (fromVal rstorage :: Storage))

test_bookkeeping :: TestTree
test_bookkeeping = testGroup "TZBTC contract bookkeeping views test"
  [ testCase
      "calling book keeping views returns expected result" $
        integrationalTestExpectation $ do
          v1 <- originateContract
          consumer <- lOriginateEmpty contractConsumer "consumer"
          withSender newOperatorAddress $ do
            -- Mint and burn some tokens
            lCall v1 (EntrypointsWithoutView $ Mint (#to .! alice, #value .! 130))
            lCall v1 (EntrypointsWithoutView $ Burn (#value .! 20))
          lCall v1 $ EntrypointsWithView $ GetTotalSupply (View () consumer)
          lCall v1 $ EntrypointsWithView $ GetTotalMinted (View () consumer)
          lCall v1 $ EntrypointsWithView $ GetTotalBurned (View () consumer)
          -- Check expectations
          validate . Right $
            lExpectViewConsumerStorage consumer [610, 630, 20]
  ]
  where
    originateContract :: IntegrationalScenarioM (ContractAddr Parameter)
    originateContract =
      lOriginate tzbtcContract "TZBTC Contract" st (toMutez 1000)
    st :: Storage
    st = mkStorage adminAddress redeemAddress_
        (Map.fromList [(redeemAddress_, initialSupply)]) (Set.fromList [newOperatorAddress])

test_setProxy :: TestTree
test_setProxy = testGroup "TZBTC contract `setProxy` test"
  [ testCase
      "Call to `setProxy` from random address gets denied with `NotAllowedToSetProxy`" $
      contractPropWithSender bob
        validateFail_ (EntrypointsWithoutView $
                       SetProxy contractAddress) storageWithOperator
  , testCase
      "Call to `setProxy` from expected address sets proxy" $
      contractPropWithSender adminAddress
        validate_ (EntrypointsWithoutView $
                   SetProxy contractAddress) storageWithOperator
  , testCase
      "Call to `setProxy` in contract with proxy set fails with `ProxyAlreadySet` error" $
      integrationalTestExpectation $ do
        c <- lOriginate tzbtcContract "TZBTC Contract" storageWithOperator (toMutez 1000)
        withSender adminAddress $ do
          lCall c (EntrypointsWithoutView $ SetProxy contractAddress)
          lCall c (EntrypointsWithoutView $ SetProxy contractAddress)
        validate . Left $
          lExpectCustomError_ #proxyAlreadySet
  ]
  where
    storageWithOperator =
      mkStorage adminAddress redeemAddress_
        (Map.fromList [(redeemAddress_, initialSupply)])
        (Set.fromList [newOperatorAddress])
    validateFail_ :: ContractPropValidator (ToT Storage) Assertion
    validateFail_ (res, _) =
      assertFailureMessage
        res [mt|NotAllowedToSetProxy|]
        "Contract did not fail with 'NotAllowedToSetProxy' message"
    validate_ :: ContractPropValidator (ToT Storage) Assertion
    validate_ (res, _) =
      case res of
        Left err -> assertFailure $ "Unexpected contract failure: " <> pretty err
        Right (_operations, rstorage) ->
          assertEqual
            "Contract's `proxy` is set as expected"
             (Right contractAddress)
             (proxy $ fields $ (fromVal rstorage :: Storage))

-- Migration tests

storageV1 :: Storage
storageV1 =
  mkStorage adminAddress redeemAddress_
    (Map.fromList [(alice, initialSupply)])
          (Set.fromList [newOperatorAddress])

storageV2 :: Storage
storageV2 =
  mkStorage adminAddress redeemAddress_ mempty mempty

originateV1 :: IntegrationalScenarioM (ContractAddr Parameter)
originateV1 =
  lOriginate tzbtcContract "UserUpgradeable V1" storageV1 (toMutez 1000)

originateV2 :: IntegrationalScenarioM (ContractAddr Parameter)
originateV2 =
  lOriginate tzbtcContract "UserUpgradeable V2" storageV2 (toMutez 1000)

originateAgent
  :: forall v2.
  ( InstrWrapC v2 "cMintForMigration"
  , AppendCtorField
      (GetCtorField v2 "cMintForMigration")
      '[] ~ '[("to" :! Address, "value" :! Natural)]
  , KnownValue v2, NoOperation v2, NoBigMap v2)
  => Address
  -> ContractAddr v2
  -> IntegrationalScenarioM Address
originateAgent oldContract newContract =
  case checkOpPresence (sing @(ToT v2)) of
    OpAbsent ->
      unContractAddress <$>
        lOriginate (Agent.agentContract @v2)
        "Migration Agent" agentStorage (toMutez 1000)
    OpPresent ->
      error "Cannot originate contract with operations in parameter"
    where
      agentStorage = Agent.StorageFields
        { oldVersion = oldContract
        , newVersion = newContract
        }

test_migration :: TestTree
test_migration = testGroup "TZBTC contract migration tests"
  [ --TODO
  ]
