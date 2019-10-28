{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC
  ( Interface
  , FlatParameter(..)
  , Parameter
  , SafeParameter
  , Storage(..)
  , StoreTemplate(..)
  , TZBTCParameter
  , TZBTCStorage
  , UpgradeParameters
  , tzbtcContract
  , fromFlatParameter
  , mkEmptyStorageV0
  , migrationScripts
  , originationParams
  , toSafeParam
  , tzbtcCompilationWay
  , tzbtcContractCode
  , tzbtcDoc
  ) where

import Lorentz

import Lorentz.Contracts.TZBTC.FlatParameter
import Lorentz.Contracts.TZBTC.Types as Types
import Lorentz.Contracts.TZBTC.V0
import Lorentz.Contracts.TZBTC.V1

-- Implementation
----------------------------------------------------------------------------

tzbtcCompilationWay :: (NiceStorage store) => LorentzCompilationWay TZBTCParameter store
tzbtcCompilationWay = lcwEntryPointsRecursive

toSafeParam :: Parameter a -> Maybe (SafeParameter a)
toSafeParam (SafeEntrypoints s) = Just s
toSafeParam _ = Nothing
