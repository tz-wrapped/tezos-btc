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
  , V2.V2Parameters
  , V2.V2DeployParameters (..)
  , tzbtcContract
  , fromFlatParameter
  , mkEmptyStorageV0
  , migrationScriptsV1
  , migrationScriptsV2
  , migrationScriptsV2FromV1
  , toSafeParam
  , makeOneShotUpgradeParameters
  , tzbtcContractRouterV1
  , tzbtcContractRouterV2
  , defaultTZBTCMetadata
  ) where

import Lorentz

import Lorentz.Contracts.Metadata
import Lorentz.Contracts.TZBTC.Common.FlatParameter
import Lorentz.Contracts.TZBTC.Preprocess
import Lorentz.Contracts.TZBTC.Types
import Lorentz.Contracts.TZBTC.V0
import qualified Lorentz.Contracts.TZBTC.V2 as V2

-- Implementation
----------------------------------------------------------------------------

toSafeParam :: Parameter s -> Maybe (SafeParameter s)
toSafeParam (SafeEntrypoints s) = Just s
toSafeParam _ = Nothing

defaultTZBTCMetadata :: TokenMetadata
defaultTZBTCMetadata =
  TokenMetadata
    { tmTokenId = singleTokenTokenId
    , tmSymbol = [mt|"TZBTC"|]
    , tmName = [mt|"Tezos BTC"|]
    , tmDecimals = 0
    , tmExtras = mempty
    }
