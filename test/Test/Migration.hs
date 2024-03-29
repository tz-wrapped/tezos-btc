{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

-- | General tests on migration machinery, not tied to any contract version.
module Test.Migration
  ( test_ownerCheck
  , test_notMigratingStatus
  , test_migratingStatus
  , test_migratingVersion
  ) where

import Test.Tasty (TestTree, testGroup)

import Lorentz.Base ((#))
import Lorentz.Coercions (checkedCoerce)
import Lorentz.Contracts.TZBTC as TZBTC
import Lorentz.Contracts.TZBTC.Preprocess
import Lorentz.Contracts.TZBTC.Types qualified as TZBTCTypes
import Lorentz.Contracts.TZBTC.V0 (StoreTemplateV0)
import Lorentz.Contracts.Upgradeable.Common (UContractRouter, mkUContractRouter)
import Lorentz.Instr as Lorentz (drop, nil, pair)
import Lorentz.UParam (mkUParam)
import Lorentz.UStore.Migration
import Morley.Tezos.Address (ta)
import Morley.Util.Named
import Test.Cleveland

import Test.AsRPC qualified as RPC
import Test.TZBTC (dummyV1Parameters)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}


-- Test that all administrative endpoints can only be called as owner
test_ownerCheck :: TestTree
test_ownerCheck = testGroup "TZBTC contract migration endpoints test"
  [ testScenario "Test call to administrative endpoints are only available to owner" $ scenario $ do
      admin <- refillable $ newAddress "admin"
      bob <- refillable $ newAddress "bob"
      v0 <- originate "tzbtc" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract
      expectCustomError_ #senderIsNotOwner $ withSender bob $
        transfer v0 $ calling def $ fromFlatParameter $ EpwBeginUpgrade (#current :! 0, #new :! 1)
  , testScenario "Test call to `ApplyMigration` endpoints are only available to owner" $ scenario $ do
      admin <- refillable $ newAddress "admin"
      bob <- refillable $ newAddress "bob"
      v0 <- originate "tzbtc" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract
      expectCustomError_ #senderIsNotOwner $ withSender bob $
        transfer v0 $ calling def $ fromFlatParameter $ EpwApplyMigration (checkedCoerce migrationScriptV1)
  , testScenario "Test call to `SetCode` endpoints are only available to owner" $ scenario $ do
      admin <- refillable $ newAddress "admin"
      bob <- refillable $ newAddress "bob"
      v0 <- originate "tzbtc" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract
      expectCustomError_ #senderIsNotOwner $ withSender bob $
        transfer v0 $ calling def $ fromFlatParameter $ EpwSetCode emptyCode
  , testScenario "Test call to `EpwFinishUpgrade` endpoints are only available to owner" $ scenario $ do
      admin <- refillable $ newAddress "admin"
      bob <- refillable $ newAddress "bob"
      v0 <- originate "tzbtc" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract
      expectCustomError_ #senderIsNotOwner $ withSender bob $
        transfer v0 $ calling def $ fromFlatParameter $ EpwFinishUpgrade
  ]

-- Test that migration entrypoints check a not migrating status
test_notMigratingStatus :: TestTree
test_notMigratingStatus = testGroup "TZBTC contract migration status not active check"
  [ testScenario "Test call to `ApplyMigration` that require a migrating state fails in non migrating state" $ scenario $ do
      admin <- refillable $ newAddress "admin"
      v0 <- originate "tzbtc" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract
      expectCustomError_ #upgContractIsNotMigrating $ withSender admin $
        transfer v0 $ calling def $ fromFlatParameter $ EpwApplyMigration (checkedCoerce migrationScriptV1)
  , testScenario "Test call to `EpwSetCode` that require a non-migrating state fails in migrating state" $ scenario $ do
      admin <- refillable $ newAddress "admin"
      v0 <- originate "tzbtc" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract
      expectCustomError_ #upgContractIsNotMigrating $ withSender admin $
        transfer v0 $ calling def $ fromFlatParameter $ EpwSetCode emptyCode
  , testScenario "Test call to `EpwFinishUpgrade` that require a non-migrating state fails in migrating state" $ scenario $ do
      admin <- refillable $ newAddress "admin"
      v0 <- originate "tzbtc" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract
      expectCustomError_ #upgContractIsNotMigrating $ withSender admin $
        transfer v0 $ calling def $ fromFlatParameter $ EpwFinishUpgrade
  ]

-- Test that other entrypoints respect migrating status and fail
test_migratingStatus :: TestTree
test_migratingStatus = testGroup "TZBTC contract migration status active check"
  [ testScenario "Test call to `Upgrade` that require a non-migrating state fails in migrating state" $ scenario $ do
      admin <- refillable $ newAddress "admin"
      v0 <- originate "tzbtc" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract
      withSender admin $ do
        transfer v0 $ calling def $ fromFlatParameter $ EpwBeginUpgrade (#current :! 0, #new :! 1)
        expectCustomError_ #upgContractIsMigrating $
          transfer v0 $ calling def $ fromFlatParameter $ Upgrade upgradeParamsV1
  , testScenario "Test call to `Run` that require a non-migrating state fails in migrating state" $ scenario $ do
      admin <- refillable $ newAddress "admin"
      v0 <- originate "tzbtc" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract
      withSender admin $ do
        transfer v0 $ calling def $ fromFlatParameter $ EpwBeginUpgrade (#current :! 0, #new :! 1)
        expectCustomError_ #upgContractIsMigrating $
          transfer v0 $ calling def $ fromFlatParameter $ Run $ mkUParam #callBurn (#value :! 100)
  , testScenario "Test call to `Burn` that require a non-migrating state fails in migrating state" $ scenario $ do
      admin <- refillable $ newAddress "admin"
      v0 <- originate "tzbtc" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract
      withSender admin $ do
        transfer v0 $ calling def $ fromFlatParameter $ EpwBeginUpgrade (#current :! 0, #new :! 1)
        expectCustomError_ #upgContractIsMigrating $
          transfer v0 $ calling def $ fromFlatParameter $ Burn (#value :! 100)
  ]

-- Test that migration bumps version
test_migratingVersion :: TestTree
test_migratingVersion = testGroup "TZBTC contract migration version check"
  [ testScenario "Test EpwFinishUpgrade bumps version" $ scenario $ do
      admin <- refillable $ newAddress "admin"
      v0 <- originate "tzbtc" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract
      withSender admin $ do
        transfer v0 $ calling def $ fromFlatParameter $ EpwBeginUpgrade (#current :! 0, #new :! 1)
        transfer v0 $ calling def $ fromFlatParameter EpwFinishUpgrade
      storage <- getStorage v0
      assert ((TZBTCTypes.currentVersion . RPC.fields) storage == 1)
        "Version was not updated"
  ]

upgradeParamsV1 :: OneShotUpgradeParameters TZBTCv0
upgradeParamsV1 = upgradeParametersV1 v1Parameters

-- Some constants
migrationScriptV1 :: MigrationScript StoreTemplateV0 StoreTemplateV1
migrationScriptV1 =
  manualConcatMigrationScripts $ migrationScriptsV1 v1Parameters

emptyCode :: UContractRouter ver
emptyCode = mkUContractRouter (Lorentz.drop # nil # pair)

redeemAddress_ :: ImplicitAddress
redeemAddress_ = [ta|tz1Mdd7rL6jGFAWCMUqatQ64K9pTpe8kazfy|]

v1Parameters :: V1Parameters
v1Parameters = dummyV1Parameters (toAddress redeemAddress_) mempty
