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
  ) where

import Lorentz

import Michelson.Optimizer (OptimizerConf(..))

import Lorentz.Contracts.TZBTC.Types (OriginationParameters)
import Lorentz.Contracts.TZBTC.V0
import Lorentz.Contracts.TZBTC.V1 (migrationScriptsRaw, tzbtcContractRouterRaw)
import Lorentz.Contracts.Upgradeable.Common.Base

-- | Preprocessed version of V0 contract.
tzbtcContract :: Contract (Parameter Interface) UStoreV0
tzbtcContract = preprocess tzbtcContractRaw

-- | Preprocessed version of contract router for V1.
tzbtcContractRouter :: UContractRouter Interface
tzbtcContractRouter =
  UContractRouter . preprocess . unContractCode $ tzbtcContractRouterRaw

-- | Preprocessed migrations for V1.
migrationScripts :: OriginationParameters -> [MigrationScript]
migrationScripts op =
  MigrationScript . preprocess . unMigrationScript <$> migrationScriptsRaw op

preprocess :: inp :-> out -> inp :-> out
preprocess = optimizeLorentzWithConf (def { gotoValues = True })
