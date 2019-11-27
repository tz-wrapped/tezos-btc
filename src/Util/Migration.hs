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
import Lorentz.Contracts.Upgradeable.Common (UContractRouter, MigrationScript)

makeMigrationParams
  :: Natural
  -> UContractRouter Interface StoreTemplate
  -> [MigrationScript]
  -> LText
makeMigrationParams version contractCode scripts = case version of
  1 ->
    LT.intercalate "\n" $
      printLorentzValue True <$>
        [ fromFlatParameter $ EpwBeginUpgrade 1
        , fromFlatParameter $ EpwSetCode contractCode
        ] ++ (fromFlatParameter . EpwApplyMigration <$> scripts)
          ++ [fromFlatParameter EpwFinishUpgrade]
  _ -> error "unknown version"
