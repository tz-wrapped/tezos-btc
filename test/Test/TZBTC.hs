{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.TZBTC
  ( checkField
  , test_acceptOwnership
  , test_burn
  , test_mint
  , test_approvableLedger
  , test_pause
  , test_unpause
  , test_setRedeemAddress
  , test_transferOwnership
  , test_bookkeeping

  , test_addOperator
  , test_removeOperator
  , originateTzbtcV1ContractRaw
  ) where

import qualified Data.Map as M
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.HUnit (testCase)

import Lorentz
import qualified Lorentz.Contracts.ApprovableLedgerInterface as AL
import Lorentz.Contracts.Consumer
import Lorentz.Contracts.ManagedLedger.Test
  (ApprovableLedger(..), OriginationParams(..), approvableLedgerSpec, originateManagedLedger)
import Lorentz.Test.Integrational
import Lorentz.UStore.Migration
import Util.Named

import Lorentz.Contracts.TZBTC

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Convert sane parameter to parameter of this contract.
fromProxyParam :: AL.Parameter -> Parameter s
fromProxyParam =
  \case
    AL.Transfer tp -> fromFlatParameter $ Transfer tp
    AL.Approve ap' -> fromFlatParameter $ Approve ap'
    AL.GetAllowance v -> fromFlatParameter $ GetAllowance v
    AL.GetTotalSupply v ->
      fromFlatParameter $ GetTotalSupply v
    AL.GetBalance v ->
      fromFlatParameter $ GetBalance v

-- Helpers to check storage state.
checkStorage
  :: (StoreTemplate -> Either ValidationError ())
  -> TZBTCStorage -> Either ValidationError ()
checkStorage fn st =
  case ustoreDecomposeFull $ dataMap st of
    Right template -> fn template
    Left err -> error err

checkField
  :: (StoreTemplate -> UStoreField a)
  -> (a -> Bool)
  -> Text
  -> (TZBTCStorage -> Either ValidationError ())
checkField ef cf ms = checkStorage (\st ->
  if cf $ unUStoreField $ ef st
  then Right ()
  else Left $ CustomValidationError ms)

originateTzbtcV1ContractRaw
  :: Address -> OriginationParams -> IntegrationalScenarioM (ContractAddr (Parameter Interface))
originateTzbtcV1ContractRaw redeem op = do
  c <- lOriginate tzbtcContract "TZBTC Contract" (mkEmptyStorageV0 adminAddress) (toMutez 1000)
  let
    o = originationParams (opAdmin op) redeem (opBalances op)
    upgradeParams =
      ( #newVersion .! 1
      , #migrationScript .! (manualConcatMigrationScripts $ migrationScripts o)
      , #newCode tzbtcContractRouter
      )
  withSender adminAddress $ lCall c (fromFlatParameter $ Upgrade upgradeParams)
  pure c

originateTzbtcV1Contract :: IntegrationalScenarioM (ContractAddr (Parameter Interface))
originateTzbtcV1Contract = originateTzbtcV1ContractRaw redeemAddress_ $ OriginationParams
  { opAdmin = adminAddress
  , opBalances = M.fromList [(redeemAddress_, initialSupply)]
  }

-- Some constants

newOperatorAddress :: Address
newOperatorAddress = genesisAddress1

adminAddress :: Address
adminAddress = genesisAddress3

redeemAddress_ :: Address
redeemAddress_ = adminAddress

replaceAddress :: Address
replaceAddress = genesisAddress2

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
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCall c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin
  , testCase
      "Call to `addOperator` from admin adds operator to the set." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField operators
              (Set.member newOperatorAddress) "New operator not found")
  ]

test_removeOperator :: TestTree
test_removeOperator = testGroup "TZBTC contract `addOperator` test"
  [ testCase
      "Call to `removeOperator` from random address gets denied with `senderIsNotAdmin` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCall c (fromFlatParameter $ RemoveOperator (#operator .! newOperatorAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin

  , testCase
      "Call to `removeOperator` from admin removes operator from the set." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField operators
              (Set.member newOperatorAddress) "New operator not found")
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ RemoveOperator (#operator .! newOperatorAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField operators
              (Prelude.not . Set.member newOperatorAddress) "Unexpectedly found operator")
  ]

test_transferOwnership :: TestTree
test_transferOwnership = testGroup "TZBTC contract `transferOwnership` test"
  [ testCase
      "Call to `transferOwnership` from random address gets denied with `senderIsNotAdmin` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCall c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin
  , testCase
      "Call to `transferOwnership` from admin address gets denied with `senderIsNotAdmin` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField newOwner (== Just replaceAddress)  "Expected `newOwner` not found")
  ]

test_acceptOwnership :: TestTree
test_acceptOwnership = testGroup "TZBTC contract `acceptOwnership` test"
  [ testCase
      "Call to `acceptOwnership` to non-transfering contract gets denied with `notInTransferOwnershipMode` error " $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCall c (fromFlatParameter $ AcceptOwnership ())
        validate . Left $
          lExpectCustomError_ #notInTransferOwnershipMode
  , testCase
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        withSender bob $ do
          lCall c (fromFlatParameter $ AcceptOwnership ())
        validate . Left $
          lExpectCustomError_ #senderIsNotNewOwner
  , testCase
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        withSender replaceAddress $ do
          lCall c (fromFlatParameter $ AcceptOwnership ())
        validate . Right $
          lExpectStorageUpdate c (checkField admin (== replaceAddress)  "Expected `admin` not found")
  , testCase
      "Call to `acceptOwnership` to transferring contract from current address gets denied with `senderIsNotNewOwner` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ AcceptOwnership ())
        validate . Left $
          lExpectCustomError_ #senderIsNotNewOwner
  ]

test_setRedeemAddress :: TestTree
test_setRedeemAddress = testGroup "TZBTC contract `setRedeemAddress` test"
  [ testCase
      "Call to `setRedeemAddress` from random address is denied with `SenderIsNotAdmin` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCall c (fromFlatParameter $ SetRedeemAddress (#redeem .! replaceAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin
  , testCase
      "Call to `setRedeemAddress` sets redeem address correctly" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ SetRedeemAddress (#redeem .! replaceAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField
              redeemAddress (== replaceAddress) "Unexpected redeem address")
  ]

test_burn :: TestTree
test_burn = testGroup "TZBTC contract `burn` test"
  [ testCase
      "Call to `burn` from random address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCall c (fromFlatParameter $ Burn (#value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `burn` from admin address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ Burn (#value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `burn` from operator subtracts from redeemAddress and update bookkeeping fields" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        consumer <- lOriginateEmpty contractConsumer "consumer"

        -- Add an operator
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))

        lCall c $ fromFlatParameter $ GetBalance (View (#owner .! redeemAddress_) consumer)

        withSender newOperatorAddress $ lCall c (fromFlatParameter $ Burn (#value .! 130))

        lCall c $ fromFlatParameter $ GetBalance (View (#owner .! redeemAddress_) consumer)
        lCall c $ fromFlatParameter $ GetTotalBurned (View () consumer)
        lCall c $ fromFlatParameter $ GetTotalSupply (View () consumer)
        lCall c $ fromFlatParameter $ GetTotalMinted (View () consumer)
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
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCall c (fromFlatParameter $ Mint (#to .! alice, #value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `mint` from admin address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ Mint (#to .! alice, #value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `mint` from operator adds to account and update bookkeeping fields" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        consumer <- lOriginateEmpty contractConsumer "consumer"

        -- Add an operator
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))

        withSender newOperatorAddress $ lCall c (fromFlatParameter $ Mint (#to .! alice, #value .! 130))

        lCall c $ fromFlatParameter $ GetBalance (View (#owner .! alice) consumer)
        lCall c $ fromFlatParameter $ GetTotalBurned (View () consumer)
        lCall c $ fromFlatParameter $ GetTotalSupply (View () consumer)
        lCall c $ fromFlatParameter $ GetTotalMinted (View () consumer)
        validate . Right $
          lExpectViewConsumerStorage consumer
            [ 130
            , 0
            , initialSupply + 130
            , initialSupply + 130
            ]
  ]

test_approvableLedger :: IO TestTree
test_approvableLedger = testSpec "TZBTC contract approvable ledger tests" $
  approvableLedgerSpec $ ApprovableLedger
    { alOriginate = originateManagedLedger (originateTzbtcV1ContractRaw redeemAddress_)
    , alMkParam = fromProxyParam
    }

test_pause :: TestTree
test_pause = testGroup "TZBTC contract `pause` test"
  [ testCase
      "Call to `pause` from random address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCall c (fromFlatParameter $ Pause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `pause` from admin address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ Pause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `pause` from operator sets the paused status" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        -- Add an operator
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))

        -- Call pause
        withSender newOperatorAddress $ lCall c (fromFlatParameter $ Pause ())

        validate . Right $
          lExpectStorageUpdate c
            (checkField paused id "Unexpected Paused status")
  ]

test_unpause :: TestTree
test_unpause = testGroup "TZBTC contract `unpause` test"
  [ testCase
      "Call to `unpause` from random address is denied with `SenderIsNotAdmin` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCall c (fromFlatParameter $ Unpause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin
  , testCase
      "Call to `unpause` from operator address is denied with `SenderIsNotAdmin` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        -- Add an operator
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        withSender newOperatorAddress $ do
          lCall c (fromFlatParameter $ Unpause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotAdmin
  , testCase
      "Call to `unpause` from admin unsets the paused status" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        -- Add an operator
        withSender adminAddress $ do
          lCall c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        -- Mint some coins for alice.
        withSender newOperatorAddress $ lCall c (fromFlatParameter $ Mint (#to .! alice, #value .! 200))
        -- Pause the contract
        withSender newOperatorAddress $ lCall c (fromFlatParameter $ Pause ())

        validate . Right $
          lExpectStorageUpdate c
            (checkField paused id "Unexpected Paused status")

        -- Call unpause
        withSender adminAddress $ lCall c (fromFlatParameter $ Unpause ())

        validate . Right $
          lExpectStorageUpdate c
            (checkField paused Prelude.not "Unexpected Paused status")
  ]

test_bookkeeping :: TestTree
test_bookkeeping = testGroup "TZBTC contract bookkeeping views test"
  [ testCase
      "calling book keeping views returns expected result" $
        integrationalTestExpectation $ do
          v1 <- originateTzbtcV1Contract
          -- Add an operator
          withSender adminAddress $ do
            lCall v1 (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
          withSender newOperatorAddress $ do
            -- Mint and burn some tokens
            lCall v1 (fromFlatParameter $ Mint (#to .! alice, #value .! 130))
            lCall v1 (fromFlatParameter $ Burn (#value .! 20))

          consumer <- lOriginateEmpty contractConsumer "consumer"

          lCall v1 $ fromFlatParameter $ GetTotalSupply (View () consumer)
          lCall v1 $ fromFlatParameter $ GetTotalMinted (View () consumer)
          lCall v1 $ fromFlatParameter $ GetTotalBurned (View () consumer)
          -- Check expectations
          validate . Right $
            lExpectViewConsumerStorage consumer [610, 630, 20]
  ]

-- New Tests End
