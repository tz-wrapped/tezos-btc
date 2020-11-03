{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Util.Migration
  ( makeMigrationParamsV1
  ) where

import Data.Text.Lazy as LT

import Lorentz
import Lorentz.UStore

import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.V0
import Lorentz.Contracts.Upgradeable.Common (UContractRouter, coerceUContractRouter)
import Util.Named

makeMigrationParamsV1
  :: UContractRouter TZBTCv1
  -> [MigrationScript StoreTemplateV0 StoreTemplateV1]
  -> LText
makeMigrationParamsV1 contractCode scripts =
  LT.intercalate "\n" $
    printLorentzValue @(Parameter TZBTCv0) True <$>
      [ fromFlatParameter $ EpwBeginUpgrade (#current .! 0, #new .! 1)
      , fromFlatParameter $ EpwSetCode $ coerceUContractRouter contractCode
      ] ++ (fromFlatParameter . EpwApplyMigration . checkedCoerce <$> scripts)
        ++ [fromFlatParameter EpwFinishUpgrade]
