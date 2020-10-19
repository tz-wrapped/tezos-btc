{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
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
  , StoreTemplateWithCommons(..)
  , StoreTemplateV1
  , TZBTCv0
  , TZBTCv1
  , SomeTZBTCVersion
  , TZBTCVersionC
  , TZBTCParameter
  , TZBTCStorage
  , OneShotUpgradeParameters
  , V1Parameters (..)
  , V1DeployParameters (..)
  , tzbtcContract
  , fromFlatParameter
  , mkEmptyStorageV0
  , migrationScripts
  , toSafeParam
  , makeOneShotUpgradeParameters
  , tzbtcContractRouter
  , tzbtcDoc
  , defaultTZBTCMetadata
  ) where

import Lorentz

import Lorentz.Contracts.Metadata
import Lorentz.Contracts.TZBTC.FlatParameter
import Lorentz.Contracts.TZBTC.Preprocess
import Lorentz.Contracts.TZBTC.Types as Types
import Lorentz.Contracts.TZBTC.V0

-- Implementation
----------------------------------------------------------------------------

toSafeParam :: Parameter s -> Maybe (SafeParameter s)
toSafeParam (SafeEntrypoints s) = Just s
toSafeParam _ = Nothing

defaultTZBTCMetadata :: TokenMetadata
defaultTZBTCMetadata =
  TokenMetadata
    { token_id = 0
    , symbol = [mt|"TZBTC"|]
    , name = [mt|"Tezos BTC"|]
    , decimals = 0
    , extras = mempty
    }
