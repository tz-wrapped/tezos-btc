{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.Migration
  ( test_adminCheck
  , test_notMigratingStatus
  , test_migratingStatus
  , test_migratingVersion
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Util.Named

import Lorentz hiding (SomeContract)
import Lorentz.Contracts.TZBTC as TZBTC
import qualified Lorentz.Contracts.TZBTC.Types as TZBTCTypes
import Lorentz.Contracts.Upgradeable.Common (UContractRouter, mkUContractRouter)
import Lorentz.Test.Integrational
import Lorentz.UStore.Migration

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

originateContract :: IntegrationalScenarioM (ContractAddr TZBTCParameter)
originateContract = lOriginate tzbtcContract "TZBTC Contract" (mkEmptyStorageV0 adminAddress) (toMutez 1000)

-- Test that all administrative endpoints can only be called as master
test_adminCheck :: TestTree
test_adminCheck = testGroup "TZBTC contract migration endpoints test"
  [  testCase "Test call to administrative endpoints are only available to master" $
      integrationalTestExpectation $ do
        -- Originate a V0 contract
        v0 <- originateContract
        withSender bob $ lCall v0 $ fromFlatParameter $ EpwBeginUpgrade 1
        validate . Left $ lExpectCustomError_ #senderIsNotAdmin
  , testCase "Test call to `ApplyMigration` endpoints are only available to master" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender bob $ lCall v0 $ fromFlatParameter $ EpwApplyMigration migrationScript
        validate . Left $ lExpectCustomError_ #senderIsNotAdmin

  , testCase "Test call to `SetCode` endpoints are only available to master" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender bob $ lCall v0 $ fromFlatParameter $ EpwSetCode emptyCode
        validate . Left $ lExpectCustomError_ #senderIsNotAdmin

  , testCase "Test call to `FinishUpgrade` endpoints are only available to master" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender bob $ lCall v0 $ fromFlatParameter $ EpwFinishUpgrade
        validate . Left $ lExpectCustomError_ #senderIsNotAdmin
  ]

-- Test that migration entrypoints check a not migrating status
test_notMigratingStatus :: TestTree
test_notMigratingStatus = testGroup "TZBTC contract migration status not active check"
  [  testCase "Test call to `ApplyMigration` that require a migrating state fails in non migrating state" $
      integrationalTestExpectation $ do
        -- Originate a V0 contract
        v0 <- originateContract
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ EpwApplyMigration migrationScript
        validate . Left $ lExpectCustomError_ #upgContractIsNotMigrating

  ,  testCase "Test call to `EpwSetCode` that require a non-migrating state fails in migrating state" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ EpwSetCode emptyCode
        validate . Left $ lExpectCustomError_ #upgContractIsNotMigrating

  ,  testCase "Test call to `EpwFinishUpgrade` that require a non-migrating state fails in migrating state" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ EpwFinishUpgrade
        validate . Left $ lExpectCustomError_ #upgContractIsNotMigrating
  ]

-- Test that other entrypoints respect migrating status and fail
test_migratingStatus :: TestTree
test_migratingStatus = testGroup "TZBTC contract migration status active check"
  [  testCase "Test call to `Upgrade` that require a non-migrating state fails in migrating state" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ EpwBeginUpgrade 1
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ Upgrade upgradeParams
        validate . Left $ lExpectCustomError_ #upgContractIsMigrating
  , testCase "Test call to `Run` that require a non-migrating state fails in migrating state" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ EpwBeginUpgrade 1
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ Run $ mkUParam #callBurn (#value .! 100)
        validate . Left $ lExpectCustomError_ #upgContractIsMigrating

  , testCase "Test call to `Burn` that require a non-migrating state fails in migrating state" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ EpwBeginUpgrade 1
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ Burn (#value .! 100)
        validate . Left $ lExpectCustomError_ #upgContractIsMigrating
  ]

-- Test that migration bumps version
test_migratingVersion :: TestTree
test_migratingVersion = testGroup "TZBTC contract migration version check"
  [  testCase "Test EpwFinishUpgrade bumps version" $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ EpwBeginUpgrade 1
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ EpwFinishUpgrade
        let
          checkVersion s =
            if (TZBTCTypes.currentVersion . TZBTCTypes.fields) s == 1
              then Right ()
              else Left $ CustomValidationError "Version was not updated"
        validate . Right $
          lExpectStorageUpdate v0 checkVersion
  , testCase "Test upgrades to unknown versions are denied " $
      integrationalTestExpectation $ do
        v0 <- originateContract
        withSender adminAddress $ lCall v0 $ fromFlatParameter $ EpwBeginUpgrade 2
        validate . Left $ lExpectCustomError #upgVersionMismatch (#expected .! 1, #actual 2)
  ]

upgradeParams :: UpgradeParameters Interface
upgradeParams = let
  o = originationParams adminAddress redeemAddress_ mempty
  in ( #newVersion .! 1
     , #migrationScript .! (manualConcatMigrationScripts $ migrationScripts o)
     , #newCode tzbtcContractRouter
     )

-- Some constants
migrationScript :: MigrationScript
migrationScript = let
  o = originationParams adminAddress redeemAddress_ mempty
  in manualConcatMigrationScripts $ migrationScripts o

emptyCode :: UContractRouter a
emptyCode = mkUContractRouter (Lorentz.drop # nil # pair)

adminAddress :: Address
adminAddress = genesisAddress3

redeemAddress_ :: Address
redeemAddress_ = adminAddress

bob :: Address
bob = genesisAddress5
