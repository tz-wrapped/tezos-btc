{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.Common.FlatParameter
  ( FlatParameter(..)
  , fromFlatParameter
  ) where

import Prelude hiding (drop, swap, (>>))

import Lorentz
import Lorentz.Contracts.Metadata
import Lorentz.Contracts.Upgradeable.Common hiding (Parameter(..), Storage, mkEmptyStorage)
import Util.Named

import Lorentz.Contracts.TZBTC.Common.Types
  (AcceptOwnershipParams, ApproveParams, BurnParams, GetAllowanceParams, GetBalanceParams,
  MintParams, OperatorParams, SetRedeemAddressParams, TransferOwnershipParams, TransferParams)
import qualified Lorentz.Contracts.TZBTC.Common.Types as TZBTC

-- | This is a type that is meant to hide details of how the actual parameter
-- is structured, so that consumers of the contract are spared of any changes
-- that might be required when the contract parameter changes structure.
data FlatParameter (ver :: VersionKind)
  = Run (VerParam ver)
  | Upgrade (OneShotUpgradeParameters ver)
  | EpwBeginUpgrade ("current" :! Version, "new" :! Version)
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
  | GetRedeemAddress    !(View () Address)
  | GetTokenMetadata    !(View [TokenId] [TokenMetadata])
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

fromFlatParameter :: FlatParameter ver -> TZBTC.Parameter ver
fromFlatParameter = \case
  Run a -> wrapInSafe $ TZBTC.Run a
  Upgrade a -> wrapInSafe $ TZBTC.Upgrade a
  EpwBeginUpgrade a -> wrapInSafe $ TZBTC.EpwBeginUpgrade a
  EpwApplyMigration a -> wrapInSafe $ TZBTC.EpwApplyMigration (#migrationscript .! a)
  EpwSetCode a -> wrapInSafe $ TZBTC.EpwSetCode (#contractcode .! a)
  EpwFinishUpgrade -> wrapInSafe $ TZBTC.EpwFinishUpgrade
  -- TZBTC Entrypoints
  Transfer a -> wrapInSafe $ TZBTC.Transfer a
  Approve a -> wrapInSafe $ TZBTC.Approve a
  Mint a -> wrapInSafe $ TZBTC.Mint a
  Burn a -> wrapInSafe $ TZBTC.Burn a
  AddOperator a -> wrapInSafe $ TZBTC.AddOperator a
  RemoveOperator a -> wrapInSafe $ TZBTC.RemoveOperator a
  SetRedeemAddress a -> wrapInSafe $ TZBTC.SetRedeemAddress a
  Pause () -> wrapInSafe $ TZBTC.Pause ()
  Unpause () -> wrapInSafe $ TZBTC.Unpause ()
  TransferOwnership a -> wrapInSafe $ TZBTC.TransferOwnership a
  AcceptOwnership a -> wrapInSafe $ TZBTC.AcceptOwnership a
  -- Views
  GetVersion a -> TZBTC.GetVersion a
  GetAllowance a -> TZBTC.GetAllowance a
  GetBalance a -> TZBTC.GetBalance a
  GetTotalSupply a -> TZBTC.GetTotalSupply a
  GetTotalMinted a -> TZBTC.GetTotalMinted a
  GetTotalBurned a -> TZBTC.GetTotalBurned a
  GetOwner a -> TZBTC.GetOwner a
  GetRedeemAddress a -> TZBTC.GetRedeemAddress a
  GetTokenMetadata a -> TZBTC.GetTokenMetadata a
  where
    wrapInSafe :: TZBTC.SafeParameter s -> TZBTC.Parameter s
    wrapInSafe s = TZBTC.SafeEntrypoints s
