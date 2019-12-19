{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}

-- | Preprocessing of TZBTC contract. We modify Lorentz code (by
-- optimizing it) before compiling to Michelson.

module Lorentz.Contracts.TZBTC.Preprocess
  ( tzbtcContract
  , tzbtcContractRouter
  , migrationScripts
  , upgradeParameters
  ) where

import Lorentz

import Michelson.Optimizer (OptimizerConf(..))

import Lorentz.Contracts.TZBTC.Types (OneShotUpgradeParameters, OriginationParameters)
import Lorentz.Contracts.TZBTC.V0
import Lorentz.Contracts.TZBTC.V1
  (StoreTemplate, TZBTCv1, migrationScriptsRaw, tzbtcContractRouterRaw)
import Lorentz.Contracts.Upgradeable.Common.Base
import Lorentz.Contracts.Upgradeable.Common.Contract
  (EpwUpgradeParameters(..), makeOneShotUpgradeParameters)
import Lorentz.UStore.Migration

-- | Preprocessed version of V0 contract.
tzbtcContract :: Contract (Parameter TZBTCv0) UStoreV0
tzbtcContract = preprocess tzbtcContractRaw

-- | Preprocessed version of contract router for V1.
tzbtcContractRouter :: UContractRouter TZBTCv1
tzbtcContractRouter =
  UContractRouter . preprocess . unUContractRouter $ tzbtcContractRouterRaw

-- | Preprocessed migrations for V1.
migrationScripts :: OriginationParameters -> [MigrationScript StoreTemplateV0 StoreTemplate]
migrationScripts op =
  manualMapMigrationScript preprocess <$> migrationScriptsRaw op

-- | Preproccessed upgrade command parameters.
upgradeParameters :: OriginationParameters -> OneShotUpgradeParameters TZBTCv0
upgradeParameters op =
  makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
    { upMigrationScripts =
        Identity . manualConcatMigrationScripts $
        migrationScripts op
    , upNewCode = tzbtcContractRouter
      -- [morley:#85] will make this field unnecessary
    , upNewPermCode = emptyPermanentImpl
    , upOverrideNewVersion = Nothing
    }

preprocess :: inp :-> out -> inp :-> out
preprocess = optimizeLorentzWithConf (def { gotoValues = True })
