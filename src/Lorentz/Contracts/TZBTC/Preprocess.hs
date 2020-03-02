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

import Lorentz.Contracts.TZBTC.Types (OneShotUpgradeParameters, V1Parameters(..))
import Lorentz.Contracts.TZBTC.V0
import Lorentz.Contracts.TZBTC.V1
  (StoreTemplateV1, TZBTCv1, migrationScriptsRaw, tzbtcContractRouterRaw)
import Lorentz.Contracts.Upgradeable.Common.Base
import Lorentz.Contracts.Upgradeable.Common.Interface
  (EpwUpgradeParameters(..), makeOneShotUpgradeParameters)
import Lorentz.UStore.Migration

-- | Preprocessed version of V0 contract.
tzbtcContract :: ContractCode (Parameter TZBTCv0) UStoreV0
tzbtcContract = preprocess tzbtcContractRaw

-- | Preprocessed version of contract router for V1.
tzbtcContractRouter :: UContractRouter TZBTCv1
tzbtcContractRouter =
  UContractRouter . preprocess . unUContractRouter $ tzbtcContractRouterRaw

-- | Preprocessed migrations for V1.
migrationScripts :: V1Parameters -> [MigrationScript StoreTemplateV0 StoreTemplateV1]
migrationScripts op =
  manualMapMigrationScript preprocess <$> migrationScriptsRaw op

-- | Preproccessed upgrade command parameters.
upgradeParameters :: V1Parameters -> OneShotUpgradeParameters TZBTCv0
upgradeParameters op =
  makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
    { upMigrationScripts =
        Identity . manualConcatMigrationScripts $
        migrationScripts op
    , upNewCode = tzbtcContractRouter
    , upNewPermCode = emptyPermanentImpl
    }

preprocess :: inp :-> out -> inp :-> out
preprocess = optimizeLorentzWithConf (def { gotoValues = True })
