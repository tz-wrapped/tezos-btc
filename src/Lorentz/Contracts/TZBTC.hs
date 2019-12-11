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
  , OriginationParameters (..)
  , tzbtcContract
  , fromFlatParameter
  , mkEmptyStorageV0
  , migrationScripts
  , toSafeParam
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

toSafeParam :: Parameter i s -> Maybe (SafeParameter i s)
toSafeParam (SafeEntrypoints s) = Just s
toSafeParam _ = Nothing
