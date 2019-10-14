{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.TZBTC
  ( test_acceptOwnership
  , test_burn
  , test_mint
  , test_pause
  , test_unpause
  , test_setRedeemAddress
  , test_transferOwnership
  , test_bookkeeping
  , test_proxyCalls

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

-- Helpers to check storage state.
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

-- Some constants

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

-- Tests

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

test_pause :: TestTree
test_pause = testGroup "TZBTC contract `pause` test"
  [ testCase
      "Call to `pause` from random address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender bob $ do
          lCall c (V0.Pause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `pause` from admin address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender adminAddress $ do
          lCall c (V0.Pause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `pause` from operator sets the paused status" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        -- Add an operator
        withSender adminAddress $ do
          lCall c (V0.AddOperator (#operator .! newOperatorAddress))

        -- Call pause
        withSender newOperatorAddress $ lCall c (V0.Pause ())

        validate . Right $
          lExpectStorageUpdate c
            (checkField V1.paused id "Unexpected Paused status")
  ]

test_unpause :: TestTree
test_unpause = testGroup "TZBTC contract `unpause` test"
  [ testCase
      "Call to `unpause` from random address is denied with `SenderIsNotAdmin` error" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        withSender bob $ do
          lCall c (V0.Unpause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin
  , testCase
      "Call to `unpause` from operator address is denied with `SenderIsNotAdmin` error" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        -- Add an operator
        withSender adminAddress $ do
          lCall c (V0.AddOperator (#operator .! newOperatorAddress))
        withSender newOperatorAddress $ do
          lCall c (V0.Unpause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin
  , testCase
      "Call to `unpause` from admin unsets the paused status" $
      integrationalTestExpectation $ do
        c <- tzbtcV1Contract
        -- Add an operator
        withSender adminAddress $ do
          lCall c (V0.AddOperator (#operator .! newOperatorAddress))

        -- Call pause
        withSender newOperatorAddress $ lCall c (V0.Pause ())

        validate . Right $
          lExpectStorageUpdate c
            (checkField V1.paused id "Unexpected Paused status")
            --
        -- Call unpause
        withSender adminAddress $ lCall c (V0.Unpause ())

        validate . Right $
          lExpectStorageUpdate c
            (checkField V1.paused Prelude.not "Unexpected Paused status")
  ]

test_bookkeeping :: TestTree
test_bookkeeping = testGroup "TZBTC contract bookkeeping views test"
  [ testCase
      "calling book keeping views returns expected result" $
        integrationalTestExpectation $ do
          v1 <- tzbtcV1Contract
          -- Add an operator
          withSender adminAddress $ do
            lCall v1 (V0.AddOperator (#operator .! newOperatorAddress))
          withSender newOperatorAddress $ do
            -- Mint and burn some tokens
            lCall v1 (V0.Mint (#to .! alice, #value .! 130))
            lCall v1 (V0.Burn (#value .! 20))

          consumer <- lOriginateEmpty contractConsumer "consumer"

          lCall v1 $ V0.Views $ V0.GetTotalSupply (View () consumer)
          lCall v1 $ V0.Views $ V0.GetTotalMinted (View () consumer)
          lCall v1 $ V0.Views $ V0.GetTotalBurned (View () consumer)
          -- Check expectations
          validate . Right $
            lExpectViewConsumerStorage consumer [610, 630, 20]
  ]

test_proxyCalls :: TestTree
test_proxyCalls = testGroup "TZBTC contract `setProxy` test"
  [ testCase
      "calling `setProxy` from expected address sets proxy as expected" $
        integrationalTestExpectation $ do
          v1 <- tzbtcV1Contract
          -- Add an operator
          withSender adminAddress $ do
            lCall v1 (V0.SetProxy contractAddress)
          validate . Right $
            lExpectStorageUpdate v1
              (checkField V1.proxy (== (Right contractAddress)) "Unexpected proxy status")
  , testCase
      "calling `setProxy` multiple times is denied" $
        integrationalTestExpectation $ do
          v1 <- tzbtcV1Contract
          -- Add an operator
          withSender adminAddress $ do
            lCall v1 (V0.SetProxy contractAddress)
          withSender adminAddress $ do
            lCall v1 (V0.SetProxy contractAddress)
          validate . Left $
            lExpectCustomError_ #proxyAlreadySet
  , testCase
      "calling `setProxy from random address sets proxy as expected" $
        integrationalTestExpectation $ do
          v1 <- tzbtcV1Contract
          -- Add an operator
          withSender bob $ do
            lCall v1 (V0.SetProxy contractAddress)
          validate . Left $
            lExpectCustomError_ #notAllowedToSetProxy

  , testCase
      "calling proxy endpoint fails if proxy is not set" $
        integrationalTestExpectation $ do
          v1 <- tzbtcV1Contract
          -- Add an operator
          withSender bob $ do
            lCall v1 (V0.TransferViaProxy (#sender .! bob, (#from .! bob, #to .! alice, #value .! 100)))
          validate . Left $
            lExpectCustomError_ #proxyIsNotSet
  , testCase
      "calling proxy endpoint from random address fails with `callerIsNotProxy` error" $
        integrationalTestExpectation $ do
          v1 <- tzbtcV1Contract
          -- Add an operator
          withSender adminAddress $ do
            lCall v1 (V0.SetProxy contractAddress)
          withSender bob $ do
            lCall v1 (V0.TransferViaProxy (#sender .! bob, (#from .! bob, #to .! alice, #value .! 100)))
          validate . Left $
            lExpectCustomError_ #callerIsNotProxy
  ]

-- New Tests End
