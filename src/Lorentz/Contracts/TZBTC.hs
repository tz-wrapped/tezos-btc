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
  , TZBTCv0
  , TZBTCv1
  , SomeTZBTCVersion
  , TZBTCVersionC
  , TZBTCParameter
  , TZBTCStorage
  , OneShotUpgradeParameters
  , OriginationParameters (..)
  , tzbtcContract
  , fromFlatParameter
  , mkEmptyStorageV0
  , migrationScripts
  , toSafeParam
  , makeOneShotUpgradeParameters
  , tzbtcContractRouter
  , tzbtcDoc
  ) where

import Lorentz

import Lorentz.Contracts.TZBTC.FlatParameter
import Lorentz.Contracts.TZBTC.Preprocess
import Lorentz.Contracts.TZBTC.Types as Types
import Lorentz.Contracts.TZBTC.V0

-- Implementation
----------------------------------------------------------------------------

toSafeParam :: Parameter s -> Maybe (SafeParameter s)
toSafeParam (SafeEntrypoints s) = Just s
toSafeParam _ = Nothing
