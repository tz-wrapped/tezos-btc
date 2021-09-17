{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

-- | Preprocessing of TZBTC contract. We modify Lorentz code (by
-- optimizing it) before compiling to Michelson.

module Lorentz.Contracts.TZBTC.Preprocess
  ( tzbtcContract

  , tzbtcContractRouterV1
  , migrationScriptsV1
  , upgradeParametersV1

  , tzbtcContractRouterV2
  , migrationScriptsV2
  , upgradeParametersV2
  , migrationScriptsV2FromV1
  , upgradeParametersV2FromV1
  ) where

import Lorentz

import Michelson.Optimizer (OptimizerConf(..))

import Lorentz.Contracts.TZBTC.Types (OneShotUpgradeParameters)
import qualified Lorentz.Contracts.TZBTC.V0 as V0
import qualified Lorentz.Contracts.TZBTC.V1 as V1
import qualified Lorentz.Contracts.TZBTC.V2 as V2
import Lorentz.Contracts.Upgradeable.Common.Base
import Lorentz.Contracts.Upgradeable.Common.Interface
  (EpwUpgradeParameters(..), makeOneShotUpgradeParameters)
import Lorentz.UStore.Migration

-- | Full V0 contract info.
tzbtcContract :: Contract (V1.Parameter V0.TZBTCv0) V0.UStoreV0
tzbtcContract = mkContractWith compilationOptions V0.tzbtcContractRaw

-- | Preprocessed version of contract router for V1.
tzbtcContractRouterV1 :: UContractRouter V1.TZBTCv1
tzbtcContractRouterV1 =
  UContractRouter . preprocess . unUContractRouter $ V1.tzbtcContractRouterRaw

-- | Preprocessed migrations for V1.
migrationScriptsV1 :: V1.V1Parameters -> [MigrationScript V0.StoreTemplateV0 V1.StoreTemplateV1]
migrationScriptsV1 op =
  manualMapMigrationScript preprocess <$> V1.migrationScriptsRaw op

-- | Preproccessed upgrade command parameters.
upgradeParametersV1 :: V1.V1Parameters -> OneShotUpgradeParameters V0.TZBTCv0
upgradeParametersV1 op =
  makeOneShotUpgradeParameters @V0.TZBTCv0 EpwUpgradeParameters
    { upMigrationScripts =
        Identity . manualConcatMigrationScripts $
        migrationScriptsV1 op
    , upNewCode = tzbtcContractRouterV1
    , upNewPermCode = emptyPermanentImpl
    }

-- | Preprocessed version of contract router for V1.
tzbtcContractRouterV2 :: UContractRouter V2.TZBTCv2
tzbtcContractRouterV2 =
  UContractRouter . preprocess . unUContractRouter $ V2.tzbtcContractRouterRaw

-- | Preprocessed migrations for V2.
migrationScriptsV2 :: V2.V2Parameters -> [MigrationScript V0.StoreTemplateV0 V2.StoreTemplateV2]
migrationScriptsV2 op =
  manualMapMigrationScript preprocess <$> V2.migrationScriptsRaw op

upgradeParametersV2 :: V2.V2Parameters -> OneShotUpgradeParameters V0.TZBTCv0
upgradeParametersV2 op =
  makeOneShotUpgradeParameters @V0.TZBTCv0 EpwUpgradeParameters
    { upMigrationScripts =
        Identity . manualConcatMigrationScripts $
        migrationScriptsV2 op
    , upNewCode = tzbtcContractRouterV2
    , upNewPermCode = emptyPermanentImpl
    }

-- | Preprocessed migrations for V2 from V1.
migrationScriptsV2FromV1 :: V2.V2ParametersFromV1 -> [MigrationScript V1.StoreTemplateV1 V2.StoreTemplateV2]
migrationScriptsV2FromV1 op =
  manualMapMigrationScript preprocess <$> V2.migrationScriptsFromV1Raw op

upgradeParametersV2FromV1 :: V2.V2ParametersFromV1 -> OneShotUpgradeParameters V1.TZBTCv1
upgradeParametersV2FromV1 op =
  makeOneShotUpgradeParameters @V1.TZBTCv1 EpwUpgradeParameters
    { upMigrationScripts =
        Identity . manualConcatMigrationScripts $
        migrationScriptsV2FromV1 op
    , upNewCode = tzbtcContractRouterV2
    , upNewPermCode = emptyPermanentImpl
    }

preprocess :: inp :-> out -> inp :-> out
preprocess = optimizeLorentzWithConf optimizationOptions

compilationOptions :: CompilationOptions
compilationOptions = defaultCompilationOptions
  { coOptimizerConf = Just optimizationOptions
  }

optimizationOptions :: OptimizerConf
optimizationOptions = def { ocGotoValues = True }
