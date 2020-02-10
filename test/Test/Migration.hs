{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.Migration
  ( test_ownerCheck
  , test_notMigratingStatus
  , test_migratingStatus
  , test_migratingVersion
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Util.Named

import Lorentz hiding (SomeContract)
import Lorentz.Contracts.TZBTC as TZBTC
import Lorentz.Contracts.TZBTC.Preprocess (upgradeParameters)
import qualified Lorentz.Contracts.TZBTC.Types as TZBTCTypes
import Lorentz.Contracts.TZBTC.V0 (StoreTemplateV0, TZBTCv0)
import Lorentz.Contracts.Upgradeable.Common (UContractRouter, mkUContractRouter)
import Lorentz.Test.Integrational
import Lorentz.UStore.Migration

import Test.TZBTC (dummyOriginationParameters)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

originateContract
  :: IntegrationalScenarioM (TAddress (Parameter TZBTCv0))
originateContract = lOriginate tzbtcContract "TZBTC Contract" (mkEmptyStorageV0 ownerAddress) (toMutez 1000)

-- Test that all owneristrative endpoints can only be called as master
test_ownerCheck :: TestTree
test_ownerCheck = testGroup "TZBTC contract migration endpoints test"
  [  testCase "Test call to owneristrative endpoints are only available to master" $
      integrationalTestExpectation $ do
        -- Originate a V0 contract
        v0 <- originateContract
        withSender bob $ lCallDef v0 $ fromFlatParameter $ EpwBeginUpgrade 1
        validate . Left $ lExpectCustomError_ #senderIsNotOwner
  , testCase "Test call to `ApplyMigration` endpoints are only available to master" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender bob $ lCallDef v0 $ fromFlatParameter $ EpwApplyMigration (checkedCoerce migrationScript)
        validate . Left $ lExpectCustomError_ #senderIsNotOwner

  , testCase "Test call to `SetCode` endpoints are only available to master" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender bob $ lCallDef v0 $ fromFlatParameter $ EpwSetCode emptyCode
        validate . Left $ lExpectCustomError_ #senderIsNotOwner

  , testCase "Test call to `FinishUpgrade` endpoints are only available to master" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender bob $ lCallDef v0 $ fromFlatParameter $ EpwFinishUpgrade
        validate . Left $ lExpectCustomError_ #senderIsNotOwner
  ]

-- Test that migration entrypoints check a not migrating status
test_notMigratingStatus :: TestTree
test_notMigratingStatus = testGroup "TZBTC contract migration status not active check"
  [  testCase "Test call to `ApplyMigration` that require a migrating state fails in non migrating state" $
      integrationalTestExpectation $ do
        -- Originate a V0 contract
        v0 <- originateContract
        withSender ownerAddress $ lCallDef v0 $ fromFlatParameter $ EpwApplyMigration (checkedCoerce migrationScript)
        validate . Left $ lExpectCustomError_ #upgContractIsNotMigrating

  ,  testCase "Test call to `EpwSetCode` that require a non-migrating state fails in migrating state" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender ownerAddress $ lCallDef v0 $ fromFlatParameter $ EpwSetCode emptyCode
        validate . Left $ lExpectCustomError_ #upgContractIsNotMigrating

  ,  testCase "Test call to `EpwFinishUpgrade` that require a non-migrating state fails in migrating state" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender ownerAddress $ lCallDef v0 $ fromFlatParameter $ EpwFinishUpgrade
        validate . Left $ lExpectCustomError_ #upgContractIsNotMigrating
  ]

-- Test that other entrypoints respect migrating status and fail
test_migratingStatus :: TestTree
test_migratingStatus = testGroup "TZBTC contract migration status active check"
  [  testCase "Test call to `Upgrade` that require a non-migrating state fails in migrating state" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender ownerAddress $ lCallDef v0 $ fromFlatParameter $ EpwBeginUpgrade 1
        withSender ownerAddress $ lCallDef v0 $ fromFlatParameter $ Upgrade upgradeParams
        validate . Left $ lExpectCustomError_ #upgContractIsMigrating
  , testCase "Test call to `Run` that require a non-migrating state fails in migrating state" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender ownerAddress $ lCallDef v0 $ fromFlatParameter $ EpwBeginUpgrade 1
        withSender ownerAddress $ lCallDef v0 $ fromFlatParameter $ Run $ mkUParam #callBurn (#value .! 100)
        validate . Left $ lExpectCustomError_ #upgContractIsMigrating

  , testCase "Test call to `Burn` that require a non-migrating state fails in migrating state" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender ownerAddress $ lCallDef v0 $ fromFlatParameter $ EpwBeginUpgrade 1
        withSender ownerAddress $ lCallDef v0 $ fromFlatParameter $ Burn (#value .! 100)
        validate . Left $ lExpectCustomError_ #upgContractIsMigrating
  ]

-- Test that migration bumps version
test_migratingVersion :: TestTree
test_migratingVersion = testGroup "TZBTC contract migration version check"
  [ testCase "Test EpwFinishUpgrade bumps version" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender ownerAddress $ lCallDef v0 $ fromFlatParameter $ EpwBeginUpgrade 1
        withSender ownerAddress $ lCallDef v0 $ fromFlatParameter $ EpwFinishUpgrade
        let
          checkVersion s =
            if (TZBTCTypes.currentVersion . TZBTCTypes.fields) s == 1
              then Right ()
              else Left $ CustomValidationError "Version was not updated"
        validate . Right $
          lExpectStorageUpdate v0 checkVersion
  ]

upgradeParams :: OneShotUpgradeParameters TZBTCv0
upgradeParams = upgradeParameters originationParameters

-- Some constants
migrationScript :: MigrationScript StoreTemplateV0 StoreTemplate
migrationScript =
  manualConcatMigrationScripts $ migrationScripts originationParameters

emptyCode :: UContractRouter ver
emptyCode = mkUContractRouter (Lorentz.drop # nil # pair)

ownerAddress :: Address
ownerAddress = genesisAddress3

redeemAddress_ :: Address
redeemAddress_ = ownerAddress

originationParameters :: OriginationParameters
originationParameters =
  dummyOriginationParameters ownerAddress redeemAddress_ mempty

bob :: Address
bob = genesisAddress5
