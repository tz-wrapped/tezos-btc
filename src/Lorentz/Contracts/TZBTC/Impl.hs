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
  , burn
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
import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger

type StorageFieldsC fields =
  fields `HasFieldsOfType`
  [ "admin" := Address
  , "paused" := Bool
  , "totalSupply" := Natural
  , "totalMinted" := Natural
  , "totalBurned" := Natural
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

userFail :: forall name fieldTy s s'. FailUsingArg Error name fieldTy s s'
userFail = failUsingArg @Error @name

burn :: forall fields. StorageFieldsC fields => Entrypoint BurnParams fields
burn = do
  dip authorizeOperator
  -- Get redeem address from storage
  dip $ do
    dup
    stackType @'[Storage' _, Storage' _]
    toField #fields; toField #redeemAddress
    toNamed #from
  -- Make managed ledger's burn entrypoint parameter
  stackType @'["value" :! Natural, "from" :! Address, Storage' _]
  dup
  dip $ do
    swap
    pair
    stackType @'[ManagedLedger.BurnParams, Storage' _]
    -- Call managed ledgers's burn entry point
    debitFrom
    -- Drop burn prameters
    drop
    stackType @'[Storage' _]
  stackType @'["value" :! Natural, Storage' _]
  -- Now add the value to total burned field
  -- Assuming totalSupply field to be amended by
  -- ManagedLedger's burn procedure.
  dip $ do
    dup
    toField #fields; getField #totalBurned
  stackType @'["value" :! Natural, Natural, _, Storage' _]
  fromNamed #value
  add
  -- Update storage
  setField #totalBurned
  setField #fields
  stackType @'[Storage' _]
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

authorizeOperator
  :: StorageFieldsC fields
  => Storage' fields : s :-> Storage' fields : s
authorizeOperator = do
  getField #fields; toField #operators; sender; mem;
  assert SenderIsNotOperator

finishNoOp :: '[st] :-> (ContractOut st)
finishNoOp = do;nil;pair

---
addTotalSupply
  :: StorageFieldsC fields
  => Integer : Storage' fields : s :-> Storage' fields : s
addTotalSupply = do
  dip $ getField #fields >> getField #totalSupply
  add; isNat; ifSome nop (failUnexpected [mt|Negative total supply|])
  setField #totalSupply; setField #fields

debitFrom
  :: forall param fields.
     ( param `HasFieldsOfType` ["from" := Address, "value" := Natural]
     , StorageFieldsC fields
     )
  => '[param, Storage' fields] :-> '[param, Storage' fields]
debitFrom = do
    -- Get LedgerValue
    duupX @2; toField #ledger; duupX @2; toField #from
    get; ifSome nop $ do
      -- Fail if absent
      stackType @[param, Storage' _]
      toField #value; toNamed #required; push 0; toNamed #present
      swap; pair; userFail #cNotEnoughBalance
    -- Get balance
    stackType @[LedgerValue, param, Storage' _]
    getField #balance
    duupX @3; toField #value
    rsub; isNat
    ifSome nop $ do
      -- Fail if balance is not enough
      stackType @[LedgerValue, param, Storage' _]
      toField #balance; toNamed #present
      duupX @2; toField #value; toNamed #required
      pair; userFail #cNotEnoughBalance
    -- Update balance, LedgerValue and Storage
    setField #balance;
    duupX @2; dip $ do
      nonEmptyLedgerValue; swap; toField #from
      dip (dip $ getField #ledger)
      update; setField #ledger

    -- Update total supply
    dup; dip $ do toField #value; neg; addTotalSupply

-- | Ensure that given 'LedgerValue' value cannot be safely removed
-- and return it.
nonEmptyLedgerValue :: LedgerValue : s :-> Maybe LedgerValue : s
nonEmptyLedgerValue = do
  getField #balance; int
  if IsNotZero
  then some
  else do
    getField #approvals
    size; int
    if IsNotZero
    then some
    else drop >> none

