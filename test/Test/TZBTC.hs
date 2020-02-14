{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.TZBTC
  ( test_acceptOwnership
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
  , unit_get_meta
  , test_documentation

  -- * Utilities
  , checkField
  , dummyOriginationParameters
  , originateTzbtcV1ContractRaw
  ) where

import Data.Coerce (coerce)
import qualified Data.Map as M
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.HUnit (Assertion, testCase)

import Lorentz
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import Lorentz.Contracts.ManagedLedger.Test
  (ApprovableLedger(..), OriginationParams(..), approvableLedgerSpec, originateManagedLedger)
import qualified Lorentz.Contracts.ManagedLedger.Test as ML
import Lorentz.Contracts.Upgradeable.Common (EpwUpgradeParameters (..), emptyPermanentImpl)
import Lorentz.Test
import Lorentz.UStore.Migration
import Util.Named

import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.V0 (TZBTCv0)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Convert sane parameter to parameter of this contract.
fromProxyParam :: AL.Parameter -> Parameter v
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

dummyTokenName :: MText
dummyTokenName = [mt|Test token|]

dummyTokenCode :: MText
dummyTokenCode = [mt|TEST|]

dummyOriginationParameters :: Address -> Address -> Map Address Natural -> OriginationParameters
dummyOriginationParameters owner redeem balances = OriginationParameters
  { opOwner = owner
  , opRedeemAddress = redeem
  , opBalances = balances
  , opTokenName = dummyTokenName
  , opTokenCode = dummyTokenCode
  }

originateTzbtcV1ContractRaw
  :: Address -> OriginationParams -> IntegrationalScenarioM (TAddress (Parameter TZBTCv1))
originateTzbtcV1ContractRaw redeem op = do
  c <- lOriginate tzbtcContract "TZBTC Contract"
    (mkEmptyStorageV0 ownerAddress) (toMutez 1000)
  let
    opTZBTC =
      dummyOriginationParameters (ML.opAdmin op) redeem (ML.opBalances op)
    upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
      { upMigrationScripts =
        Identity $
        manualConcatMigrationScripts (migrationScripts opTZBTC)
      , upNewCode = tzbtcContractRouter
      , upNewPermCode = emptyPermanentImpl
      }
  withSender ownerAddress $ lCallDef c (fromFlatParameter $ Upgrade upgradeParams)
  pure $ coerce c

originateTzbtcV1Contract
  :: IntegrationalScenarioM (TAddress (Parameter TZBTCv1))
originateTzbtcV1Contract = originateTzbtcV1ContractRaw redeemAddress_ $ OriginationParams
  { opAdmin = ownerAddress
  , opBalances = M.fromList [(redeemAddress_, initialSupply)]
  }

-- Some constants

newOperatorAddress :: Address
newOperatorAddress = genesisAddress1

ownerAddress :: Address
ownerAddress = genesisAddress3

redeemAddress_ :: Address
redeemAddress_ = ownerAddress

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
      "Call to `addOperator` from random address gets denied with `senderIsNotOwner` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotOwner
  , testCase
      "Call to `addOperator` from owner adds operator to the set." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField operators
              (Set.member newOperatorAddress) "New operator not found")
  ]

test_removeOperator :: TestTree
test_removeOperator = testGroup "TZBTC contract `addOperator` test"
  [ testCase
      "Call to `removeOperator` from random address gets denied with `senderIsNotOwner` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCallDef c (fromFlatParameter $ RemoveOperator (#operator .! newOperatorAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotOwner

  , testCase
      "Call to `removeOperator` from owner removes operator from the set." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField operators
              (Set.member newOperatorAddress) "New operator not found")
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ RemoveOperator (#operator .! newOperatorAddress))
        validate . Right $
          lExpectStorageUpdate c
            (checkField operators
              (Prelude.not . Set.member newOperatorAddress) "Unexpectedly found operator")
  ]

test_transferOwnership :: TestTree
test_transferOwnership = testGroup "TZBTC contract `transferOwnership` test"
  [ testCase
      "Call to `transferOwnership` from random address gets denied with `senderIsNotOwner` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCallDef c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotOwner
  , testCase
      "Call to `transferOwnership` from owner address gets denied with `senderIsNotOwner` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
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
          lCallDef c (fromFlatParameter $ AcceptOwnership ())
        validate . Left $
          lExpectCustomError_ #notInTransferOwnershipMode
  , testCase
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        withSender bob $ do
          lCallDef c (fromFlatParameter $ AcceptOwnership ())
        validate . Left $
          lExpectCustomError_ #senderIsNotNewOwner
  , testCase
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        withSender replaceAddress $ do
          lCallDef c (fromFlatParameter $ AcceptOwnership ())
        validate . Right $
          lExpectStorageUpdate c (checkField owner (== replaceAddress)  "Expected `owner` not found")
  , testCase
      "Call to `acceptOwnership` to transferring contract from current address gets denied with `senderIsNotNewOwner` error." $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AcceptOwnership ())
        validate . Left $
          lExpectCustomError_ #senderIsNotNewOwner
  ]

test_setRedeemAddress :: TestTree
test_setRedeemAddress = testGroup "TZBTC contract `setRedeemAddress` test"
  [ testCase
      "Call to `setRedeemAddress` from random address is denied with `SenderIsNotOwner` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCallDef c (fromFlatParameter $ SetRedeemAddress (#redeem .! replaceAddress))
        validate . Left $
          lExpectCustomError_ #senderIsNotOwner
  , testCase
      "Call to `setRedeemAddress` sets redeem address correctly" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ SetRedeemAddress (#redeem .! replaceAddress))
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
          lCallDef c (fromFlatParameter $ Burn (#value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `burn` from owner address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ Burn (#value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `burn` from operator subtracts from redeemAddress and update bookkeeping fields" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        consumer <- lOriginateEmpty contractConsumer "consumer"

        -- Add an operator
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))

        lCallDef c $ fromFlatParameter $ GetBalance (mkView (#owner .! redeemAddress_) consumer)

        withSender newOperatorAddress $ lCallDef c (fromFlatParameter $ Burn (#value .! 130))

        lCallDef c $ fromFlatParameter $ GetBalance (mkView (#owner .! redeemAddress_) consumer)
        lCallDef c $ fromFlatParameter $ GetTotalBurned (mkView () consumer)
        lCallDef c $ fromFlatParameter $ GetTotalSupply (mkView () consumer)
        lCallDef c $ fromFlatParameter $ GetTotalMinted (mkView () consumer)
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
          lCallDef c (fromFlatParameter $ Mint (#to .! alice, #value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `mint` from owner address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ Mint (#to .! alice, #value .! 100))
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `mint` from operator adds to account and update bookkeeping fields" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        consumer <- lOriginateEmpty contractConsumer "consumer"

        -- Add an operator
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))

        withSender newOperatorAddress $ lCallDef c (fromFlatParameter $ Mint (#to .! alice, #value .! 130))

        lCallDef c $ fromFlatParameter $ GetBalance (mkView (#owner .! alice) consumer)
        lCallDef c $ fromFlatParameter $ GetTotalBurned (mkView () consumer)
        lCallDef c $ fromFlatParameter $ GetTotalSupply (mkView () consumer)
        lCallDef c $ fromFlatParameter $ GetTotalMinted (mkView () consumer)
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
          lCallDef c (fromFlatParameter $ Pause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `pause` from owner address is denied with `SenderIsNotOperator` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ Pause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotOperator
  , testCase
      "Call to `pause` from operator sets the paused status" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        -- Add an operator
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))

        -- Call pause
        withSender newOperatorAddress $ lCallDef c (fromFlatParameter $ Pause ())

        validate . Right $
          lExpectStorageUpdate c
            (checkField paused id "Unexpected Paused status")
  ]

test_unpause :: TestTree
test_unpause = testGroup "TZBTC contract `unpause` test"
  [ testCase
      "Call to `unpause` from random address is denied with `SenderIsNotOwner` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        withSender bob $ do
          lCallDef c (fromFlatParameter $ Unpause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotOwner
  , testCase
      "Call to `unpause` from operator address is denied with `SenderIsNotOwner` error" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        -- Add an operator
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        withSender newOperatorAddress $ do
          lCallDef c (fromFlatParameter $ Unpause ())
        validate . Left $
          lExpectCustomError_ #senderIsNotOwner
  , testCase
      "Call to `unpause` from owner unsets the paused status" $
      integrationalTestExpectation $ do
        c <- originateTzbtcV1Contract
        -- Add an operator
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        -- Mint some coins for alice.
        withSender newOperatorAddress $ lCallDef c (fromFlatParameter $ Mint (#to .! alice, #value .! 200))
        -- Pause the contract
        withSender newOperatorAddress $ lCallDef c (fromFlatParameter $ Pause ())

        validate . Right $
          lExpectStorageUpdate c
            (checkField paused id "Unexpected Paused status")

        -- Call unpause
        withSender ownerAddress $ lCallDef c (fromFlatParameter $ Unpause ())

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
          withSender ownerAddress $ do
            lCallDef v1 (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
          withSender newOperatorAddress $ do
            -- Mint and burn some tokens
            lCallDef v1 (fromFlatParameter $ Mint (#to .! alice, #value .! 130))
            lCallDef v1 (fromFlatParameter $ Burn (#value .! 20))

          consumer <- lOriginateEmpty contractConsumer "consumer"

          lCallDef v1 $ fromFlatParameter $ GetTotalSupply (mkView () consumer)
          lCallDef v1 $ fromFlatParameter $ GetTotalMinted (mkView () consumer)
          lCallDef v1 $ fromFlatParameter $ GetTotalBurned (mkView () consumer)
          -- Check expectations
          validate . Right $
            lExpectViewConsumerStorage consumer [610, 630, 20]

          -- Check redeem address getter (we need another consumer
          -- because the type is different).
          withSender ownerAddress $ do
            lCallDef v1 (fromFlatParameter $ SetRedeemAddress (#redeem .! newOperatorAddress))
          consumerAddr <- lOriginateEmpty contractConsumer "consumer"
          lCallDef v1 $ fromFlatParameter $ GetRedeemAddress (mkView () consumerAddr)
          validate . Right $
            lExpectViewConsumerStorage consumerAddr [newOperatorAddress]
  ]

unit_get_meta :: Assertion
unit_get_meta = integrationalTestExpectation $ do
  v1 <- originateTzbtcV1Contract
  consumer <- lOriginateEmpty contractConsumer "consumer"
  lCallDef v1 $ fromFlatParameter $ GetTokenName (mkView () consumer)
  lCallDef v1 $ fromFlatParameter $ GetTokenCode (mkView () consumer)
  validate . Right $
    lExpectViewConsumerStorage consumer [dummyTokenName, dummyTokenCode]

test_documentation :: [TestTree]
test_documentation = runDocTests testLorentzDoc tzbtcDoc
