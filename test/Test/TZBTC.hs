{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.TZBTC
  (
    test_tree
  ) where

import Fmt (pretty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

import Lorentz
import Lorentz.Contracts.TZBTC
import Michelson.Interpret (ContractEnv(..), MichelsonFailed(..))
import Michelson.Runtime.GState (genesisAddress1, genesisAddress2, genesisAddress3, genesisAddress4)
import Michelson.Test (ContractPropValidator, contractProp, dummyContractEnv)
import Michelson.Text (mt)
import Michelson.Typed (Value'(..))
import Util.Named

test_tree :: TestTree
test_tree = testGroup "TZBTC contract test"
  [ testCase "Fails with `SenderNotAdmin` if sender is not administrator for `addOperator` call" $
      contractProp lContract
        validate' contractEnv (AddOperator (#operator .! newOperatorAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `removeOperator` call" $
      contractProp lContract
        validate' contractEnv (RemoveOperator (#operator .! newOperatorAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `startMigrateFrom` call" $
      contractProp lContract
        validate' contractEnv (StartMigrateFrom (#migratefrom .! contractAddress)) storage
  , testCase
      "Fails with `SenderNotAdmin` if sender is not administrator for `startMigrateTo` call" $
      contractProp lContract
        validate' contractEnv (StartMigrateTo (#migrateto .! contractAddress)) storage
  ]
  where
    lContract = (unI tzbtcContract)
    newOperatorAddress = genesisAddress1
    senderAddress = genesisAddress2
    adminAddress = genesisAddress3
    contractAddress = genesisAddress4
    contractEnv =
      dummyContractEnv
        { ceSender = senderAddress }
    storage = mkStorage adminAddress mempty
    validate' :: ContractPropValidator (ToT Storage) Assertion
    validate' (res, _) =
      case res of
        Left err -> do
          case err of
            MichelsonFailedWith (VPair ((VC (CvString t)), _)) ->
              assertEqual
                "Contract did not fail with 'SenderIsNotAdmin' message"
                 t
                 [mt|SenderIsNotAdmin|]
            a -> assertFailure $ "Unexpected contract failure: " <> pretty a
        Right (_operations, _) ->
          assertFailure "Contract did not fail as expected"
