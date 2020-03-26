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
import Util.Named

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
  mkTokenMetadata tokenId symbol name decimals extras
  where
    tokenId = #tokenId .! 0
    symbol = #symbol .! [mt|"TZBTC"|]
    name = #name .! [mt|"Tezos BTC"|]
    decimals = #decimals .! 0
    extras = #extras .! mempty
