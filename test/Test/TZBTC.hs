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
  ) where

import Fmt (pretty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertBool, assertFailure, testCase)
import Data.Set
import qualified Data.Set as Set

import Lorentz
import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.Types
import Michelson.Interpret (ContractEnv(..), MichelsonFailed(..))
import Michelson.Runtime.GState
import Michelson.Test (ContractPropValidator, contractProp, dummyContractEnv)
import Michelson.Text (mt)
import Michelson.Typed (Instr, ToTs, Value'(..))
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

contractEnv :: ContractEnv
contractEnv =
  dummyContractEnv
    { ceSender = adminAddress }

storage :: Storage
storage = mkStorage adminAddress redeemAddress_ mempty mempty

test_adminCheck :: TestTree
test_adminCheck = testGroup "TZBTC contract admin check test"
  [ testCase "Fails with `SenderNotAdmin` if sender is not administrator for `addOperator` call" $
      contractProp lContract
        validate' badContractEnv (AddOperator (#operator .! newOperatorAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `removeOperator` call" $
      contractProp lContract
        validate' badContractEnv (RemoveOperator (#operator .! newOperatorAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `startMigrateFrom` call" $
      contractProp lContract
        validate' badContractEnv (StartMigrateFrom (#migratefrom .! contractAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `startMigrateTo` call" $
      contractProp lContract
        validate' badContractEnv (StartMigrateTo (#migrateto .! contractAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `transferOwnership` call" $
      contractProp lContract
        validate' badContractEnv (TransferOwnership (#newowner .! adminAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `setRedeemAddress` call" $
      contractProp lContract
        validate' badContractEnv (SetRedeemAddress (#redeem .! redeemAddress_)) storage
  ]
  where
    badContractEnv :: ContractEnv
    badContractEnv =
      dummyContractEnv
        { ceSender = badAdminAddress }

    validate' :: ContractPropValidator (ToT Storage) Assertion
    validate' (res, _) =
      case res of
        Left err -> do
          case err of
            MichelsonFailedWith (VPair ((VC (CvString t)), _)) ->
              assertEqual
                "Contract did not fail with 'SenderIsNotAdmin' message"
                 [mt|SenderIsNotAdmin|]
                 t
            a -> assertFailure $ "Unexpected contract failure: " <> pretty a
        Right (_operations, _) ->
          assertFailure "Contract did not fail as expected"

test_addOperator :: TestTree
test_addOperator = testGroup "TZBTC contract `addOperator` test"
  [ testCase
      "Call to `addOperator` Adds new operator to the set of operators" $
      contractProp lContract
        validateAdd contractEnv (AddOperator (#operator .! newOperatorAddress)) storage
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
      contractProp lContract
        validateRemove
        contractEnv (RemoveOperator (#operator .! operatorToRemove)) storageWithOperator
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
      contractProp lContract
        validate_ contractEnv (SetRedeemAddress (#redeem .! newRedeemAddress)) storage
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
      contractProp lContract
        validate_ contractEnv (TransferOwnership (#newowner .! newOwnerAddress)) storage
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
      contractProp lContract
        validateNotInTransfer contractEnvWithGoodSender (AcceptOwnership ()) storage
  , testCase
      "Call to `acceptOwnership` fails for bad caller" $
      contractProp lContract
        validateBadSender contractEnvWithBadSender (AcceptOwnership ()) storageInTranferOwnership
  , testCase
      "Call to `acceptOwnership` updates admin with address of new owner" $
      contractProp lContract
        validateGoodOwner contractEnvWithGoodSender (AcceptOwnership ()) storageInTranferOwnership
  ]
  where
    newOwnerAddress = replaceAddress
    badSenderAddress = genesisAddress1
    contractEnvWithBadSender =
      dummyContractEnv
        { ceSender = badSenderAddress }
    contractEnvWithGoodSender =
      dummyContractEnv
        { ceSender = newOwnerAddress }
    storageInTranferOwnership = let
      f = fields storage
      in storage { fields = f { newOwner = Just newOwnerAddress } }

    validateNotInTransfer :: ContractPropValidator (ToT Storage) Assertion
    validateNotInTransfer (res, _) =
      case res of
        Left err -> do
          case err of
            MichelsonFailedWith (VPair ((VC (CvString t)), _)) ->
              assertEqual
                "Contract did not fail with 'NotInTransferOwnershipMode' message"
                 [mt|NotInTransferOwnershipMode|]
                 t
            a -> assertFailure $ "Unexpected contract failure: " <> pretty a
        Right (_operations, _) ->
          assertFailure "Contract did not fail as expected"

    validateBadSender :: ContractPropValidator (ToT Storage) Assertion
    validateBadSender (res, _) =
      case res of
        Left err -> do
          case err of
            MichelsonFailedWith (VPair ((VC (CvString t)), _)) ->
              assertEqual
                "Contract did not fail with 'SenderIsNotNewOwner' message"
                 [mt|SenderIsNotNewOwner|]
                 t
            a -> assertFailure $ "Unexpected contract failure: " <> pretty a
        Right (_operations, _) ->
          assertFailure "Contract did not fail as expected"

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
