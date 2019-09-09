{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.TZBTC
  ( test_adminCheck
  , test_addOperator
  , test_removeOperator
  , test_setRedeemAddress
  , test_transferOwnership
  , test_acceptOwnership
  , test_burn
  , test_mint
  , test_pause
  , test_unpause_
  , test_bookkeeping
  ) where

import Fmt (pretty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertBool, assertFailure, testCase)
import qualified Data.Map as Map
import Data.Set
import qualified Data.Set as Set
import Named (arg)

import Lorentz
import Lorentz.Contracts.Consumer
import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.Types
import Lorentz.Test.Integrational
import Michelson.Interpret (ContractEnv(..), MichelsonFailed(..))
import Michelson.Test
  ( ContractPropValidator, contractProp, dummyContractEnv)
import Michelson.Text (mt)
import Michelson.Typed (Instr, ToTs, Value, Value'(..))
import Util.Named

lContract :: Instr (ToTs '[(Parameter, Storage)]) (ToTs (ContractOut Storage))
lContract = (unI tzbtcContract)

newOperatorAddress :: Address
newOperatorAddress = genesisAddress1

adminAddress :: Address
adminAddress = genesisAddress3

badAdminAddress :: Address
badAdminAddress = genesisAddress5

redeemAddress_ :: Address
redeemAddress_ = adminAddress

replaceAddress :: Address
replaceAddress = genesisAddress2

contractAddress :: Address
contractAddress = genesisAddress4

alice :: Address
alice = genesisAddress6

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

test_adminCheck :: TestTree
test_adminCheck = testGroup "TZBTC contract admin check test"
  [ testCase "Fails with `SenderNotAdmin` if sender is not administrator for `addOperator` call" $
      contractPropWithSender badAdminAddress validate'
        (AddOperator (#operator .! newOperatorAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `removeOperator` call" $
      contractPropWithSender badAdminAddress validate'
        (RemoveOperator (#operator .! newOperatorAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `startMigrateFrom` call" $
      contractPropWithSender badAdminAddress validate'
        (StartMigrateFrom (#migratefrom .! contractAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `startMigrateTo` call" $
      contractPropWithSender badAdminAddress validate'
        (StartMigrateTo (#migrateto .! contractAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `transferOwnership` call" $
      contractPropWithSender badAdminAddress validate'
        (TransferOwnership (#newowner .! adminAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `setRedeemAddress` call" $
      contractPropWithSender badAdminAddress validate'
        (SetRedeemAddress (#redeem .! redeemAddress_)) storage
  ]
  where
    validate' :: ContractPropValidator (ToT Storage) Assertion
    validate' (res, _) =
      assertFailureMessage
        res [mt|SenderIsNotAdmin|]
        "Contract did not fail with 'SenderIsNotAdmin' message"

test_addOperator :: TestTree
test_addOperator = testGroup "TZBTC contract `addOperator` test"
  [ testCase
      "Call to `addOperator` Adds new operator to the set of operators" $
      contractPropWithSender adminAddress
        validateAdd (AddOperator (#operator .! newOperatorAddress)) storage
  ]
  where
    validateAdd :: ContractPropValidator (ToT Storage) Assertion
    validateAdd (res, _) =
      case res of
        Left err -> assertFailure $ "Unexpected contract failure: " <> pretty err
        Right (_operations, storage_) ->
          assertBool "Contract did not add operator address to the set" $
            member newOperatorAddress ((operators.fields) $ fromVal storage_)

test_removeOperator :: TestTree
test_removeOperator = testGroup "TZBTC contract `removeOperator` test"
  [ testCase
      "Call to `removeOperator` removes operator from the set of operators" $
      contractPropWithSender adminAddress
        validateRemove
        (RemoveOperator (#operator .! operatorToRemove)) storageWithOperator
  ]
  where
    operatorToRemove = replaceAddress
    storageWithOperator = mkStorage adminAddress redeemAddress_ mempty (Set.fromList [operatorToRemove])
    validateRemove :: ContractPropValidator (ToT Storage) Assertion
    validateRemove (res, _) =
      case res of
        Left err -> assertFailure $ "Unexpected contract failure: " <> pretty err
        Right (_operations, storage_) ->
          assertBool "Contract did note remove operator address from the set" $
            Prelude.not $ member operatorToRemove ((operators.fields) $ fromVal storage_)

test_setRedeemAddress :: TestTree
test_setRedeemAddress = testGroup "TZBTC contract `setRedeemAddress` test"
  [ testCase
      "Call to `setRedeemAddress` updates redeemAddress" $
      contractPropWithSender adminAddress
        validate_ (SetRedeemAddress (#redeem .! newRedeemAddress)) storage
  ]
  where
    newRedeemAddress = replaceAddress
    validate_ :: ContractPropValidator (ToT Storage) Assertion
    validate_ (res, _) =
      case res of
        Left err -> assertFailure $ "Unexpected contract failure: " <> pretty err
        Right (_operations, rstorage) ->
          assertEqual
            "Contract did set redeem address to expected value"
            newRedeemAddress
            ((redeemAddress.fields) $ fromVal rstorage)

test_transferOwnership :: TestTree
test_transferOwnership = testGroup "TZBTC contract `transferOwnership` test"
  [ testCase
      "Call to `transferOwnership` updates `newOwner`" $
      contractPropWithSender adminAddress
        validate_ (TransferOwnership (#newowner .! newOwnerAddress)) storage
  ]
  where
    newOwnerAddress = replaceAddress
    validate_ :: ContractPropValidator (ToT Storage) Assertion
    validate_ (res, _) =
      case res of
        Left err -> assertFailure $ "Unexpected contract failure: " <> pretty err
        Right (_operations, rstorage) ->
          assertEqual
            "Contract did not set newOwner field to the expected value"
            (Just newOwnerAddress)
            ((newOwner.fields) $ fromVal rstorage)

test_acceptOwnership :: TestTree
test_acceptOwnership = testGroup "TZBTC contract `acceptOwnership` test"
  [ testCase
      "Call to `acceptOwnership` get denied on contract that is not in transfer mode" $
      contractPropWithSender newOwnerAddress
        validateNotInTransfer (AcceptOwnership ()) storage
  , testCase
      "Call to `acceptOwnership` fails for bad caller" $
      contractPropWithSender badSenderAddress
        validateBadSender (AcceptOwnership ()) storageInTranferOwnership
  , testCase
      "Call to `acceptOwnership` updates admin with address of new owner" $
      contractPropWithSender newOwnerAddress
        validateGoodOwner (AcceptOwnership ()) storageInTranferOwnership
  ]
  where
    newOwnerAddress = replaceAddress
    badSenderAddress = genesisAddress1
    storageInTranferOwnership = let
      f = fields storage
      in storage { fields = f { newOwner = Just newOwnerAddress } }

    validateNotInTransfer :: ContractPropValidator (ToT Storage) Assertion
    validateNotInTransfer (res, _) =
      assertFailureMessage
        res [mt|NotInTransferOwnershipMode|]
          "Contract did not fail with 'NotInTransferOwnershipMode' message"

    validateBadSender :: ContractPropValidator (ToT Storage) Assertion
    validateBadSender (res, _) =
      assertFailureMessage
        res [mt|SenderIsNotNewOwner|]
          "Contract did not fail with 'SenderIsNotNewOwner' message"

    validateGoodOwner :: ContractPropValidator (ToT Storage) Assertion
    validateGoodOwner (res, _) =
      case res of
        Left err -> assertFailure $ "Unexpected contract failure: " <> pretty err
        Right (_operations, rstorage) -> do
          assertEqual
            "Contract did not set admin to the address from newOwner field"
             newOwnerAddress
             ((admin.fields) $ fromVal rstorage)
          assertEqual
            "Contract did not set newOwner field to None"
             Nothing
             ((newOwner.fields) $ fromVal rstorage)

test_burn :: TestTree
test_burn = testGroup "TZBTC contract `burn` test"
  [ testCase
      "Call to `burn` gets denied with `SenderIsNotOperator`" $
      contractPropWithSender adminAddress
        validateFail_ (Burn (#value .! 100)) storageWithOperator
  , testCase
      "Call to `burn` burns from `redeemAddress` and update `totalBurned` \
      \ and `totalSupply` fields correctly" $
      contractPropWithSender newOperatorAddress
        validate_ (Burn (#value .! 100)) storageWithOperator
  ]
  where
    storageWithOperator =
      mkStorage adminAddress redeemAddress_
        (Map.fromList [(redeemAddress_, initialSupply)]) (Set.fromList [newOperatorAddress])
    validateFail_ :: ContractPropValidator (ToT Storage) Assertion
    validateFail_ (res, _) =
      assertFailureMessage
        res [mt|SenderIsNotOperator|]
          "Contract did not fail with 'SenderIsNotOperator' message"

    validate_ :: ContractPropValidator (ToT Storage) Assertion
    validate_ (res, _) =
      case res of
        Left err -> assertFailure $ "Unexpected contract failure: " <> pretty err
        Right (_operations, rstorage) -> do
          assertEqual
            "Contract's `burn` operation reduced the balance in redeem address by expected amount"
             (Just 400  :: Maybe Natural)
             (((arg #balance) . fst)
                <$> (Map.lookup redeemAddress_ $ unBigMap $
                  ledger $ (fromVal rstorage :: Storage)))
          --  Assert the totalBurned field in storage is updated correctly.
          assertEqual
            "Contract's `burn` operation did not update `totalBurned` field correctly."
             100
             (totalBurned $ fields $ (fromVal rstorage :: Storage))
          assertEqual
            "Contract's `burn` operation did not update `totalSupply` field correctly"
             400
             (totalSupply $ fields $ (fromVal rstorage :: Storage))

test_mint :: TestTree
test_mint = testGroup "TZBTC contract `mint` test"
  [ testCase
      "Call to `mint` gets denied with `SenderIsNotOperator`" $
      contractPropWithSender adminAddress
        validateFail_ (Burn (#value .! 100)) storageWithOperator
  , testCase
      "Call to `mint` adds value to `to` parameter in input and update `totalMinted` \
      \ and `totalSupply` fields correctly" $
      contractPropWithSender newOperatorAddress
        validate_ (Mint (#to .! alice, #value .! 200)) storageWithOperator
  ]
  where
    storageWithOperator =
      mkStorage adminAddress redeemAddress_
        (Map.fromList [(redeemAddress_, initialSupply)]) (Set.fromList [newOperatorAddress])
    validateFail_ :: ContractPropValidator (ToT Storage) Assertion
    validateFail_ (res, _) =
      assertFailureMessage
        res [mt|SenderIsNotOperator|]
          "Contract did not fail with 'SenderIsNotOperator' message"

    validate_ :: ContractPropValidator (ToT Storage) Assertion
    validate_ (res, _) =
      case res of
        Left err -> assertFailure $ "Unexpected contract failure: " <> pretty err
        Right (_operations, rstorage) -> do
          assertEqual
            "Contract's `mint` operation credited the target account with the  expected amount"
             (Just 200  :: Maybe Natural)
             (((arg #balance) . fst)
                <$> (Map.lookup alice $ unBigMap $
                  ledger $ (fromVal rstorage :: Storage)))
          --  Assert the totalBurned field in storage is updated correctly.
          assertEqual
            "Contract's `mint` operation did not update `totalMinted` field correctly."
             700
             (totalMinted $ fields $ (fromVal rstorage :: Storage))
          assertEqual
            "Contract's `mint` operation did not update `totalSupply` field correctly"
             700
             (totalSupply $ fields $ (fromVal rstorage :: Storage))

test_pause :: TestTree
test_pause = testGroup "TZBTC contract `pause` permission test"
  [ testCase
      "Call to `pause` gets denied with `SenderIsNotOperator`" $
      contractPropWithSender adminAddress
        validateFail_ (Pause ()) storageWithOperator
  , testCase
      "Call to `pause` as operator is allowed" $
      contractPropWithSender newOperatorAddress
        validate_ (Pause ()) storageWithOperator
  ]
  where
    storageWithOperator =
      mkStorage adminAddress redeemAddress_
        (Map.fromList [(redeemAddress_, initialSupply)]) (Set.fromList [newOperatorAddress])
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
        validateFail_ (Unpause ()) storageWithOperator
  , testCase
      "Call to `unpause` as admin is allowed" $
      contractPropWithSender adminAddress
        validate_ (Unpause ()) storageWithOperator
  ]
  where
    storageWithOperator =
      mkStorage adminAddress redeemAddress_
        (Map.fromList [(redeemAddress_, initialSupply)]) (Set.fromList [newOperatorAddress])
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
            lCall v1 (Mint (#to .! alice, #value .! 130))
            lCall v1 (Burn (#value .! 20))
          lCall v1 $ GetTotalSupply (View () consumer)
          lCall v1 $ GetTotalMinted (View () consumer)
          lCall v1 $ GetTotalBurned (View () consumer)
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
