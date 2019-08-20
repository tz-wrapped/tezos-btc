{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# Language RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | Implementation of manalged ledger which does not require
-- particular storage type.

module Lorentz.Contracts.TZBTC.Impl
  ( StorageFieldsC
  , ManagedLedger.transfer
  , ManagedLedger.transfer'
  , ManagedLedger.approve
  , ManagedLedger.approve'
  , ManagedLedger.getAllowance
  , ManagedLedger.getBalance
  , ManagedLedger.getTotalSupply
  , ManagedLedger.setPause
  , ManagedLedger.setAdministrator
  , ManagedLedger.getAdministrator
  , ManagedLedger.mint
  , ManagedLedger.burn
  , addOperator
  , removeOperator
  , setRedeemAddress
  , transferOwnership
  , acceptOwnership
  , startMigrateTo
  , startMigrateFrom
  , migrate
  ) where

import Prelude hiding (drop, get, some, swap, (>>))

import Lorentz
import Lorentz.Contracts.TZBTC.Types
import qualified Lorentz.Contracts.ManagedLedger.Impl as ManagedLedger

type StorageFieldsC fields =
  fields `HasFieldsOfType`
  [ "admin" := Address
  , "paused" := Bool
  , "totalSupply" := Natural
  ]

type Entrypoint param fields
  = '[ param, Storage' fields ] :-> ContractOut (Storage' fields)

stub :: StorageFieldsC fields => Entrypoint a fields
stub = do
  dip authorizeAdmin
  drop
  nil; pair

addOperator :: StorageFieldsC fields => Entrypoint OperatorParams fields
addOperator = stub

removeOperator :: StorageFieldsC fields => Entrypoint OperatorParams fields
removeOperator = stub

setRedeemAddress
  :: StorageFieldsC fields
  => Entrypoint SetRedeemAddressParams fields
setRedeemAddress = stub

transferOwnership
  :: StorageFieldsC fields
  => Entrypoint TransferOwnershipParams fields
transferOwnership = stub

acceptOwnership :: StorageFieldsC fields => Entrypoint () fields
acceptOwnership = stub

startMigrateTo
  :: StorageFieldsC fields
  => Entrypoint StartMigrateToParams fields
startMigrateTo = stub

startMigrateFrom
  :: StorageFieldsC fields
  => Entrypoint StartMigrateFromParams fields
startMigrateFrom = stub

migrate :: StorageFieldsC fields => Entrypoint MigrateParams fields
migrate = stub

authorizeAdmin
  :: StorageFieldsC fields
  => Storage' fields : s :-> Storage' fields : s
authorizeAdmin = do
  getField #fields; toField #admin; sender; eq
  if_ nop (failUsing SenderIsNotAdmin)

