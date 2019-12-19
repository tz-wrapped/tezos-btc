{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.FlatParameter
  ( FlatParameter(..)
  , fromFlatParameter
  ) where

import Prelude hiding (drop, swap, (>>))

import Lorentz
import Lorentz.Contracts.Upgradeable.Common hiding (Parameter(..), Storage, mkEmptyStorage)
import Util.Named

import Lorentz.Contracts.TZBTC.Types
  (AcceptOwnershipParams, ApproveParams, BurnParams, GetAllowanceParams, GetBalanceParams,
  MintParams, OneShotUpgradeParameters, OperatorParams, SetRedeemAddressParams,
  TransferOwnershipParams, TransferParams)
import qualified Lorentz.Contracts.TZBTC.Types as TZBTC

-- | This is a type that is meant to hide details of how the actual parameter
-- is structured, so that consumers of the contract are spared of any changes
-- that might be required when the contract parameter changes structure.
data FlatParameter (ver :: VersionKind)
  = Run (VerParam ver)
  | Upgrade (OneShotUpgradeParameters ver)
  | EpwBeginUpgrade Version
  | EpwApplyMigration (MigrationScriptFrom (VerUStoreTemplate ver))
  | EpwSetCode SomeUContractRouter
  | EpwFinishUpgrade
  -- TZBTC Entrypoints
  | GetVersion (View () Version)
  | GetAllowance        !(View GetAllowanceParams Natural)
  | GetBalance          !(View GetBalanceParams Natural)
  | GetTotalSupply      !(View () Natural)
  | GetTotalMinted      !(View () Natural)
  | GetTotalBurned      !(View () Natural)
  | GetOwner            !(View () Address)
  | GetTokenName        !(View () MText)
  | GetTokenCode        !(View () MText)
  | GetRedeemAddress    !(View () Address)
  | Transfer            !TransferParams
  | Approve             !ApproveParams
  | Mint                !MintParams
  | Burn                !BurnParams
  | AddOperator         !OperatorParams
  | RemoveOperator      !OperatorParams
  | SetRedeemAddress    !SetRedeemAddressParams
  | Pause               !()
  | Unpause             !()
  | TransferOwnership   !TransferOwnershipParams
  | AcceptOwnership     !AcceptOwnershipParams

fromFlatParameter :: FlatParameter s -> TZBTC.Parameter s
fromFlatParameter = \case
  Run a -> TZBTC.SafeEntrypoints $ TZBTC.Run a
  Upgrade a -> TZBTC.SafeEntrypoints $ TZBTC.Upgrade a
  EpwBeginUpgrade a -> TZBTC.SafeEntrypoints $ TZBTC.EpwBeginUpgrade a
  EpwApplyMigration a -> TZBTC.SafeEntrypoints $ TZBTC.EpwApplyMigration (#migrationscript .! a)
  EpwSetCode a -> TZBTC.SafeEntrypoints $ TZBTC.EpwSetCode (#contractcode .! a)
  EpwFinishUpgrade -> TZBTC.SafeEntrypoints $ TZBTC.EpwFinishUpgrade
  -- TZBTC Entrypoints
  Transfer a -> TZBTC.SafeEntrypoints $ TZBTC.Transfer a
  Approve a -> TZBTC.SafeEntrypoints $ TZBTC.Approve a
  Mint a -> TZBTC.SafeEntrypoints $ TZBTC.Mint a
  Burn a -> TZBTC.SafeEntrypoints $ TZBTC.Burn a
  AddOperator a -> TZBTC.SafeEntrypoints $ TZBTC.AddOperator a
  RemoveOperator a -> TZBTC.SafeEntrypoints $ TZBTC.RemoveOperator a
  SetRedeemAddress a -> TZBTC.SafeEntrypoints $ TZBTC.SetRedeemAddress a
  Pause () -> TZBTC.SafeEntrypoints $ TZBTC.Pause ()
  Unpause () -> TZBTC.SafeEntrypoints $ TZBTC.Unpause ()
  TransferOwnership a -> TZBTC.SafeEntrypoints $ TZBTC.TransferOwnership a
  AcceptOwnership a -> TZBTC.SafeEntrypoints $ TZBTC.AcceptOwnership a
  -- Views
  GetVersion a -> TZBTC.GetVersion a
  GetAllowance a -> TZBTC.GetAllowance a
  GetBalance a -> TZBTC.GetBalance a
  GetTotalSupply a -> TZBTC.GetTotalSupply a
  GetTotalMinted a -> TZBTC.GetTotalMinted a
  GetTotalBurned a -> TZBTC.GetTotalBurned a
  GetOwner a -> TZBTC.GetOwner a
  GetTokenName a -> TZBTC.GetTokenName a
  GetTokenCode a -> TZBTC.GetTokenCode a
  GetRedeemAddress a -> TZBTC.GetRedeemAddress a
