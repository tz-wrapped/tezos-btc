{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Util.Migration
  ( makeMigrationParams
  ) where

import Data.Text.Lazy as LT

import Lorentz

import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.V0
import Lorentz.Contracts.Upgradeable.Common
  (MigrationScript, UContractRouter, coerceUContractRouter)

makeMigrationParams
  :: Natural
  -> UContractRouter TZBTCv1
  -> [MigrationScript StoreTemplateV0 StoreTemplate]
  -> LText
makeMigrationParams version contractCode scripts = case version of
  1 ->
    LT.intercalate "\n" $
      printLorentzValue @(Parameter TZBTCv0) True <$>
        [ fromFlatParameter $ EpwBeginUpgrade 1
        , fromFlatParameter $ EpwSetCode $ coerceUContractRouter contractCode
        ] ++ (fromFlatParameter . EpwApplyMigration . checkedCoerce <$> scripts)
          ++ [fromFlatParameter EpwFinishUpgrade]
  _ -> error "unknown version"
