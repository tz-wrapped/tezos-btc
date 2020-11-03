{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Util.Migration
  ( makeMigrationParamsV1
  , makeMigrationParamsV2
  , makeMigrationParamsV2FromV1
  ) where

import Data.Text.Lazy as LT

import Lorentz
import Lorentz.UStore

import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.V0 as V0
import qualified Lorentz.Contracts.TZBTC.V1 as V1
import qualified Lorentz.Contracts.TZBTC.V2 as V2
import Lorentz.Contracts.Upgradeable.Common (UContractRouter, coerceUContractRouter)
import Util.Named

makeMigrationParamsV1
  :: UContractRouter V1.TZBTCv1
  -> [MigrationScript V0.StoreTemplateV0 V1.StoreTemplateV1]
  -> LText
makeMigrationParamsV1 contractCode scripts =
  LT.intercalate "\n" $
    printLorentzValue @(Parameter TZBTCv0) True <$>
      [ fromFlatParameter $ EpwBeginUpgrade (#current .! 0, #new .! 1)
      , fromFlatParameter $ EpwSetCode $ coerceUContractRouter contractCode
      ] ++ (fromFlatParameter . EpwApplyMigration . checkedCoerce <$> scripts)
        ++ [fromFlatParameter EpwFinishUpgrade]

makeMigrationParamsV2
  :: UContractRouter V2.TZBTCv2
  -> [MigrationScript V0.StoreTemplateV0 V2.StoreTemplateV2]
  -> LText
makeMigrationParamsV2 contractCode scripts =
  LT.intercalate "\n" $
    printLorentzValue @(Parameter TZBTCv0) True <$>
      [ fromFlatParameter $ EpwBeginUpgrade (#current .! 0, #new .! 2)
      , fromFlatParameter $ EpwSetCode $ coerceUContractRouter contractCode
      ] ++ (fromFlatParameter . EpwApplyMigration . checkedCoerce <$> scripts)
        ++ [fromFlatParameter EpwFinishUpgrade]

makeMigrationParamsV2FromV1
  :: [MigrationScript V1.StoreTemplateV1 V2.StoreTemplateV2]
  -> LText
makeMigrationParamsV2FromV1 scripts =
  LT.intercalate "\n" $
    printLorentzValue @(Parameter TZBTCv1) True <$>
      [ fromFlatParameter $ EpwBeginUpgrade (#current .! 1, #new .! 2)
        -- No need to migrate contract code
        -- TODO: cover code equality with test
      ] ++ (fromFlatParameter . EpwApplyMigration . checkedCoerce <$> scripts)
        ++ [fromFlatParameter EpwFinishUpgrade]
