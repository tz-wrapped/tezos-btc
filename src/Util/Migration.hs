{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
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
import Util.Named

makeMigrationParams
  :: Natural
  -> UContractRouter TZBTCv1
  -> [MigrationScript StoreTemplateV0 StoreTemplateV1]
  -> LText
makeMigrationParams version contractCode scripts = case version of
  1 ->
    LT.intercalate "\n" $
      printLorentzValue @(Parameter TZBTCv0) True <$>
        [ fromFlatParameter $ EpwBeginUpgrade (#current .! 0, #new .! 1)
        , fromFlatParameter $ EpwSetCode $ coerceUContractRouter contractCode
        ] ++ (fromFlatParameter . EpwApplyMigration . checkedCoerce <$> scripts)
          ++ [fromFlatParameter EpwFinishUpgrade]
  _ -> error "unknown version"
