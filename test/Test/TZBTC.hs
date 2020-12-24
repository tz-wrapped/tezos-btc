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
  , dummyV2Parameters
  , originateTzbtcV1ContractRaw
  ) where

import Data.Coerce (coerce)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text.IO.Utf8 as Utf8
import Test.HUnit (Assertion, assertFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.HUnit (testCase)

import Lorentz
import Lorentz.Contracts.Metadata
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import qualified Lorentz.Contracts.Test.ApprovableLedger as AL
import Lorentz.Contracts.Test.ManagedLedger (OriginationParams(..), originateManagedLedger)
import qualified Lorentz.Contracts.Test.ManagedLedger as ML
import Lorentz.Contracts.Upgradeable.Common (EpwUpgradeParameters(..), emptyPermanentImpl)
import Lorentz.Test
import Lorentz.UStore
import Lorentz.UStore.Migration
import Michelson.Runtime (parseExpandContract)
import Michelson.Test.Unit (matchContractEntrypoints, mkEntrypointsMap)
import Michelson.Typed.Convert (convertContract)
import qualified Michelson.Untyped as U
import Util.Named

import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.V1 as V1
import qualified Lorentz.Contracts.TZBTC.V2 as V2

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Convert sane parameter to parameter of this contract.
-- Helpers to check storage state.
checkStorage
  :: (StoreTemplateV1 -> Either TestError ())
  -> TZBTCStorage -> Either TestError ()
checkStorage fn st =
  case ustoreDecomposeFull $ dataMap st of
    Right template -> fn template
    Left err -> error err

checkField
  :: (StoreTemplateV1 -> UStoreField a)
  -> (a -> Bool)
  -> Text
  -> (TZBTCStorage -> Either TestError ())
checkField ef cf msg = checkStorage (\st ->
  if cf $ unUStoreField $ ef st
  then Right ()
  else Left $ CustomTestError msg)

dummyV1Parameters :: Address -> Map Address Natural -> V1Parameters
dummyV1Parameters redeem balances = V1Parameters
  { v1RedeemAddress = redeem
  , v1TokenMetadata = defaultTZBTCMetadata
  , v1Balances = balances
  }

dummyV2Parameters :: Address -> Map Address Natural -> V2Parameters
dummyV2Parameters = dummyV1Parameters

-- TODO [#134]: reuse testUpgradeToV1 instead of methods below

originateTzbtcV1ContractRaw
  :: Address -> OriginationParams -> IntegrationalScenarioM (TAddress (Parameter SomeTZBTCVersion))
originateTzbtcV1ContractRaw redeem op = do
  let owner = ML.opAdmin op
  c <- lOriginate tzbtcContract "TZBTC Contract"
    (mkEmptyStorageV0 owner) (toMutez 1000)
  let
    opTZBTC = dummyV1Parameters redeem (ML.opBalances op)
    upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
      { upMigrationScripts =
        Identity $
        manualConcatMigrationScripts (migrationScriptsV1 opTZBTC)
      , upNewCode = tzbtcContractRouterV1
      , upNewPermCode = emptyPermanentImpl
      }
  withSender owner $ lCallDef c (fromFlatParameter $ Upgrade upgradeParams)
  pure $ coerce c

originateTzbtcV1Contract
  :: IntegrationalScenarioM (TAddress (Parameter SomeTZBTCVersion))
originateTzbtcV1Contract = originateTzbtcV1ContractRaw redeemAddress_ $ OriginationParams
  { opAdmin = ownerAddress
  , opBalances = M.fromList [(redeemAddress_, initialSupply)]
  }

originateTzbtcV2ContractRaw
  :: Address -> OriginationParams -> IntegrationalScenarioM (TAddress (Parameter SomeTZBTCVersion))
originateTzbtcV2ContractRaw redeem op = do
  let owner = ML.opAdmin op
  c <- lOriginate tzbtcContract "TZBTC Contract"
    (mkEmptyStorageV0 owner) (toMutez 1000)
  let
    opTZBTC = dummyV2Parameters redeem (ML.opBalances op)
    upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
      { upMigrationScripts =
        Identity $
        manualConcatMigrationScripts (migrationScriptsV2 opTZBTC)
      , upNewCode = tzbtcContractRouterV2
      , upNewPermCode = emptyPermanentImpl
      }
  withSender owner $ lCallDef c (fromFlatParameter $ Upgrade upgradeParams)
  pure $ coerce c

originateTzbtcV2Contract
  :: IntegrationalScenarioM (TAddress (Parameter SomeTZBTCVersion))
originateTzbtcV2Contract = originateTzbtcV2ContractRaw redeemAddress_ $ OriginationParams
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

-- We have the header of the actual tzbtc contract (parameter, storage types with
-- some nop code) in ./test/resources/tzbtc-parameter-entrypoints-ref.tz.
-- We parse it, extract the paramter type from the parsed contract. Then we use
-- the 'mkEntrypointsMap' function to extract the field and type annotations from
-- the relavant parts of the parameter type. This is compared against the
-- auto derivied entrypoints of the contract. They should match or else the
-- test fails. If the tzbtc parameter has to be changed, then this test should
-- be fixed by editing the `tzbtc-parameter-entrypoints-ref.tz` file.
entrypointsRef :: IO (Map EpName U.Type)
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
  -> (IntegrationalScenarioM (TAddress (Parameter SomeTZBTCVersion)) -> Assertion)
  -> TestTree
testContract name testSuite =
  testGroup name $
    [ ("V1 contract", originateTzbtcV1Contract)
    , ("V2 contract", originateTzbtcV2Contract)
    ] <&> \(verName, originateContract) ->
      testCase verName (testSuite originateContract)

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
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender bob $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        lExpectCustomError_ #senderIsNotOwner err
  , testContract
      "Call to `addOperator` from owner adds operator to the set." $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        lExpectStorageUpdate c $
          checkField (operators . stCustom)
            (Set.member newOperatorAddress) "New operator not found"
  ]

test_removeOperator :: TestTree
test_removeOperator = testGroup "TZBTC contract `addOperator` test"
  [ testContract
      "Call to `removeOperator` from random address gets denied with `senderIsNotOwner` error." $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender bob $ do
          lCallDef c (fromFlatParameter $ RemoveOperator (#operator .! newOperatorAddress))
        lExpectCustomError_ #senderIsNotOwner err

  , testContract
      "Call to `removeOperator` from owner removes operator from the set." $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        lExpectStorageUpdate c
          (checkField (operators . stCustom)
            (Set.member newOperatorAddress) "New operator not found")
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ RemoveOperator (#operator .! newOperatorAddress))
        lExpectStorageUpdate c
          (checkField (operators . stCustom)
            (Prelude.not . Set.member newOperatorAddress) "Unexpectedly found operator")
  ]

test_transferOwnership :: TestTree
test_transferOwnership = testGroup "TZBTC contract `transferOwnership` test"
  [ testContract
      "Call to `transferOwnership` from random address gets denied with `senderIsNotOwner` error." $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender bob $ do
          lCallDef c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        lExpectCustomError_ #senderIsNotOwner err
  , testContract
      "Call to `transferOwnership` from owner address gets denied with `senderIsNotOwner` error." $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        lExpectStorageUpdate c
          (checkField (newOwner . stCustom) (== Just replaceAddress)
            "Expected `newOwner` not found")
  ]

test_acceptOwnership :: TestTree
test_acceptOwnership = testGroup "TZBTC contract `acceptOwnership` test"
  [ testContract
      "Call to `acceptOwnership` to non-transfering contract gets denied with `notInTransferOwnershipMode` error " $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender bob $ do
          lCallDef c (fromFlatParameter $ AcceptOwnership ())
        lExpectCustomError_ #notInTransferOwnershipMode err
  , testContract
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        err <- expectError . withSender bob $ do
          lCallDef c (fromFlatParameter $ AcceptOwnership ())
        lExpectCustomError_ #senderIsNotNewOwner err
  , testContract
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        withSender replaceAddress $ do
          lCallDef c (fromFlatParameter $ AcceptOwnership ())
        lExpectStorageUpdate c (checkField owner (== replaceAddress)  "Expected `owner` not found")
  , testContract
      "Call to `acceptOwnership` to transferring contract from current address gets denied with `senderIsNotNewOwner` error." $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ TransferOwnership (#newOwner .! replaceAddress))
        err <- expectError . withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AcceptOwnership ())
        lExpectCustomError_ #senderIsNotNewOwner err
  ]

test_setRedeemAddress :: TestTree
test_setRedeemAddress = testGroup "TZBTC contract `setRedeemAddress` test"
  [ testContract
      "Call to `setRedeemAddress` from random address is denied with `SenderIsNotOwner` error" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender bob $ do
          lCallDef c (fromFlatParameter $ SetRedeemAddress (#redeem .! replaceAddress))
        lExpectCustomError_ #senderIsNotOwner err
  , testContract
      "Call to `setRedeemAddress` sets redeem address correctly" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ SetRedeemAddress (#redeem .! replaceAddress))
        lExpectStorageUpdate c
          (checkField
            (redeemAddress . stCustom) (== replaceAddress) "Unexpected redeem address")
  ]

test_burn :: TestTree
test_burn = testGroup "TZBTC contract `burn` test"
  [ testContract
      "Call to `burn` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender bob $ do
          lCallDef c (fromFlatParameter $ Burn (#value .! 100))
        lExpectCustomError_ #senderIsNotOperator err
  , testContract
      "Call to `burn` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ Burn (#value .! 100))
        lExpectCustomError_ #senderIsNotOperator err
  , testContract
      "Call to `burn` from operator subtracts from redeemAddress and update bookkeeping fields" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
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
  [ testContract
      "Call to `mint` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender bob $ do
          lCallDef c (fromFlatParameter $ Mint (#to .! alice, #value .! 100))
        lExpectCustomError_ #senderIsNotOperator err
  , testContract
      "Call to `mint` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ Mint (#to .! alice, #value .! 100))
        lExpectCustomError_ #senderIsNotOperator err
  , testContract
      "Call to `mint` from operator adds to account and update bookkeeping fields" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        consumer <- lOriginateEmpty contractConsumer "consumer"

        -- Add an operator
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))

        withSender newOperatorAddress $ lCallDef c (fromFlatParameter $ Mint (#to .! alice, #value .! 130))

        lCallDef c $ fromFlatParameter $ GetBalance (mkView (#owner .! alice) consumer)
        lCallDef c $ fromFlatParameter $ GetTotalBurned (mkView () consumer)
        lCallDef c $ fromFlatParameter $ GetTotalSupply (mkView () consumer)
        lCallDef c $ fromFlatParameter $ GetTotalMinted (mkView () consumer)
        lExpectViewConsumerStorage consumer
          [ 130
          , 0
          , initialSupply + 130
          , initialSupply + 130
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
    alOriginate = originateManagedLedger (originateTzbtcV1ContractRaw redeemAddress_)

test_approvableLedgerV2 :: IO TestTree
test_approvableLedgerV2 = testSpec "TZBTC contract approvable ledger tests" $ do
  AL.approvableLedgerSpec $
    originateManagedLedger (originateTzbtcV2ContractRaw redeemAddress_)

test_pause :: TestTree
test_pause = testGroup "TZBTC contract `pause` test"
  [ testContract
      "Call to `pause` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender bob $ do
          lCallDef c (fromFlatParameter $ Pause ())
        lExpectCustomError_ #senderIsNotOperator err
  , testContract
      "Call to `pause` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ Pause ())
        lExpectCustomError_ #senderIsNotOperator err
  , testContract
      "Call to `pause` from operator sets the paused status" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        -- Add an operator
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))

        -- Call pause
        withSender newOperatorAddress $ lCallDef c (fromFlatParameter $ Pause ())

        lExpectStorageUpdate c
          (checkField (paused . stCustom) id "Unexpected Paused status")
  ]

test_unpause :: TestTree
test_unpause = testGroup "TZBTC contract `unpause` test"
  [ testContract
      "Call to `unpause` from random address is denied with `SenderIsNotOwner` error" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        err <- expectError . withSender bob $ do
          lCallDef c (fromFlatParameter $ Unpause ())
        lExpectCustomError_ #senderIsNotOwner err
  , testContract
      "Call to `unpause` from operator address is denied with `SenderIsNotOwner` error" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        -- Add an operator
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        err <- expectError . withSender newOperatorAddress $
          lCallDef c (fromFlatParameter $ Unpause ())
        lExpectCustomError_ #senderIsNotOwner err
  , testContract
      "Call to `unpause` from owner unsets the paused status" $
      \originateContract -> integrationalTestExpectation $ do
        c <- originateContract
        -- Add an operator
        withSender ownerAddress $ do
          lCallDef c (fromFlatParameter $ AddOperator (#operator .! newOperatorAddress))
        -- Mint some coins for alice.
        withSender newOperatorAddress $ lCallDef c (fromFlatParameter $ Mint (#to .! alice, #value .! 200))
        -- Pause the contract
        withSender newOperatorAddress $ lCallDef c (fromFlatParameter $ Pause ())

        lExpectStorageUpdate c
          (checkField (paused . stCustom) id "Unexpected Paused status")

        -- Call unpause
        withSender ownerAddress $ lCallDef c (fromFlatParameter $ Unpause ())

        lExpectStorageUpdate c
          (checkField (paused . stCustom) Prelude.not "Unexpected Paused status")
  ]

test_bookkeeping :: TestTree
test_bookkeeping = testGroup "TZBTC contract bookkeeping views test"
  [ testContract
      "calling book keeping views returns expected result" $
      \originateContract -> integrationalTestExpectation $ do
          v1 <- originateContract
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
          lExpectViewConsumerStorage consumer [610, 630, 20]

          -- Check redeem address getter (we need another consumer
          -- because the type is different).
          withSender ownerAddress $ do
            lCallDef v1 (fromFlatParameter $ SetRedeemAddress (#redeem .! newOperatorAddress))
          consumerAddr <- lOriginateEmpty contractConsumer "consumer"
          lCallDef v1 $ fromFlatParameter $ GetRedeemAddress (mkView () consumerAddr)
          lExpectViewConsumerStorage consumerAddr [newOperatorAddress]
  ]

test_get_meta :: TestTree
test_get_meta =
  testContract "Can get TZBTC metadata" $
    \originateContract -> integrationalTestExpectation $ do
      v1 <- originateContract
      consumer <- lOriginateEmpty contractConsumer "consumer"
      lCallDef v1 $ fromFlatParameter $ GetTokenMetadata (mkView [singleTokenTokenId] consumer)
      lExpectViewConsumerStorage consumer [[defaultTZBTCMetadata]]

test_documentation :: [TestTree]
test_documentation =
  [ testGroup "V1" $
      runDocTests testLorentzDoc (buildLorentzDoc V1.tzbtcDoc)
  , testGroup "V2" $
      runDocTests testLorentzDoc (buildLorentzDoc V2.tzbtcDoc)
  ]
