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
  , ManagedLedger.approve
  , ManagedLedger.approve'
  , ManagedLedger.burn
  , ManagedLedger.getAdministrator
  , ManagedLedger.getAllowance
  , ManagedLedger.getBalance
  , ManagedLedger.getTotalSupply
  , ManagedLedger.mint
  , ManagedLedger.setAdministrator
  , ManagedLedger.transfer
  , ManagedLedger.transfer'
  , acceptOwnership
  , addOperator
  , migrate
  , pause
  , removeOperator
  , setRedeemAddress
  , startMigrateFrom
  , startMigrateTo
  , transferOwnership
  , unpause
  ) where

import Prelude hiding (drop, get, some, swap, (>>))

import Data.Set (Set)

import Lorentz
import Lorentz.Contracts.TZBTC.Types
import qualified Lorentz.Contracts.ManagedLedger.Impl as ManagedLedger

type StorageFieldsC fields =
  fields `HasFieldsOfType`
  [ "admin" := Address
  , "paused" := Bool
  , "totalSupply" := Natural
  , "operators" := Set Address
  , "redeemAddress" := Address
  , "newOwner" := Maybe Address
  ]

type Entrypoint param fields
  = '[ param, Storage' fields ] :-> ContractOut (Storage' fields)

stub :: StorageFieldsC fields => Entrypoint a fields
stub = do
  dip authorizeAdmin
  drop
  finishNoOp

addOperator :: forall fields. StorageFieldsC fields => Entrypoint OperatorParams fields
addOperator = addRemoveOperator AddOperator

removeOperator :: StorageFieldsC fields => Entrypoint OperatorParams fields
removeOperator = addRemoveOperator RemoveOperator

data OperatorAction = AddOperator | RemoveOperator

addRemoveOperator :: StorageFieldsC fields => OperatorAction -> Entrypoint OperatorParams fields
addRemoveOperator ar = do
  dip authorizeAdmin
  stackType @'[OperatorParams, Storage' _]
  -- Unwrap operator address
  fromNamed #operator
  -- Get operator set from storage...
  dip $ do
    getField #fields
    getField #operators
    push $ case ar of
      AddOperator -> True
      RemoveOperator -> False
  -- and insert/remove the operator address depending
  -- on the value of boolean flag ar
  update
  -- Update storage with the new operator set
  setField #operators
  setField #fields
  finishNoOp

setRedeemAddress
  :: StorageFieldsC fields
  => Entrypoint SetRedeemAddressParams fields
setRedeemAddress = do
  dip authorizeAdmin
  -- Unwrap operator address
  fromNamed #redeem
  dip (getField #fields)
  -- Set redeem address
  setField #redeemAddress
  setField #fields
  finishNoOp

transferOwnership
  :: StorageFieldsC fields
  => Entrypoint TransferOwnershipParams fields
transferOwnership = do
  dip authorizeAdmin
  -- Unwrap new owner address
  fromNamed #newowner
  dip (getField #fields)
  -- Make a `Some` value and..
  some
  -- set it as newOwner field.
  setField #newOwner
  setField #fields
  finishNoOp

acceptOwnership :: StorageFieldsC fields => Entrypoint () fields
acceptOwnership = do
  dip authorizeNewOwner
  -- Discart the unit parameter
  drop
  getField #fields
  -- Get sender newOwner address
  getField #newOwner
  ifSome (do
    setField #admin
    none
    setField #newOwner -- Reset newOwner field to None.
    setField #fields) (failUsing NotInTransferOwnershipMode)
    -- This isSome check is probably redundant since the
    -- check is done in `authorizeNewOwner`
  finishNoOp

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

pause :: StorageFieldsC fields => Entrypoint () fields
pause = do
  drop
  push True
  ManagedLedger.setPause

unpause :: StorageFieldsC fields => Entrypoint () fields
unpause = do
  drop
  push False
  ManagedLedger.setPause

authorizeAdmin
  :: StorageFieldsC fields
  => Storage' fields : s :-> Storage' fields : s
authorizeAdmin = do
  getField #fields; toField #admin; sender; eq
  if_ nop (failUsing SenderIsNotAdmin)

authorizeNewOwner
  :: StorageFieldsC fields
  => Storage' fields : s :-> Storage' fields : s
authorizeNewOwner = do
  getField #fields; toField #newOwner;
  if IsSome then do
    sender
    eq
    if_ nop (failUsing SenderIsNotNewOwner)
    else (failUsing NotInTransferOwnershipMode)

finishNoOp :: '[st] :-> (ContractOut st)
finishNoOp = do;nil;pair
