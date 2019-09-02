{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# Language RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.Impl
  ( StorageFieldsC
  , ManagedLedger.approve
  , ManagedLedger.approve'
  , ManagedLedger.getAdministrator
  , ManagedLedger.getAllowance
  , ManagedLedger.getBalance
  , ManagedLedger.getTotalSupply
  , ManagedLedger.setAdministrator
  , ManagedLedger.transfer
  , ManagedLedger.transfer'
  , acceptOwnership
  , addOperator
  , burn
  , getTotal
  , mint
  , migrate
  , mintForMigration
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
import Data.Vinyl.Derived (Label)

import Lorentz
import Lorentz.Contracts.TZBTC.Types hiding (AddOperator, RemoveOperator)
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
  , "migrationManagerIn" := Maybe MigrationManager
  , "migrationManagerOut" := Maybe MigrationManager
  ]

type Entrypoint param fields
  = '[ param, Storage' fields ] :-> ContractOut (Storage' fields)

stub :: StorageFieldsC fields => Entrypoint a fields
stub = do
  dip authorizeAdmin
  drop
  finishNoOp

getTotal
  :: forall fields a.
    (fields `HasFieldsOfType` '[a := Natural])
  => Label a -> Entrypoint (View () Natural) fields
getTotal bp = view_ (do cdr; toField #fields; toField bp)

-- | Burn the specified amount of tokens from redeem address. Since it
-- is not possible to burn from any other address, this entry point does
-- not have an address input. Only operators are allowed to call this entry
-- point.
burn :: forall fields. StorageFieldsC fields => Entrypoint BurnParams fields
burn = do
  dip authorizeOperator
  -- Get redeem address from storage
  dip $ do
    dup
    stackType @'[Storage' _, Storage' _]
    toField #fields; toField #redeemAddress
    toNamed #from
  -- Make ManagedLedger's burn entrypoint parameter
  stackType @'["value" :! Natural, "from" :! Address, Storage' _]
  pair
  burn_

burn_
  :: forall fields. StorageFieldsC fields
  => Entrypoint ("value" :! Natural, "from" :! Address) fields
burn_ = do
  unpair
  dup
  dip $ do
    swap
    pair
    stackType @'[ManagedLedger.BurnParams, Storage' _]
    -- Call ManagedLedgers's `debitFrom` function.
    ManagedLedger.debitFrom
    -- Drop burn prameters
    drop
    stackType @'[Storage' _]
  stackType @'["value" :! Natural, Storage' _]
  -- Now add the value to total burned field
  -- assuming totalSupply field to be update by
  -- managedLedger's burn procedure.
  dip $ do
    dup
    toField #fields; getField #totalBurned
  fromNamed #value
  add
  -- Update storage
  setField #totalBurned
  setField #fields
  finishNoOp

mint_ :: forall fields. StorageFieldsC fields => Entrypoint MintParams fields
mint_ = do
  -- Make managed ledger's mint entrypoint parameter
  stackType @'[("to" :! Address, "value" :! Natural), Storage' _]
  dup
  dip $ do
    stackType @'[ManagedLedger.MintParams, Storage' _]
    -- update ledger
    ManagedLedger.creditTo
    -- Drop mint prameters
    drop
    stackType @'[Storage' _]
  cdr
  stackType @'["value" :! Natural, Storage' _]
  -- Now add the value to total minted field
  -- assuming totalSupply field to be updated by
  -- managedLedger's mint procedure.
  dip $ do
    dup
    toField #fields; getField #totalMinted
  fromNamed #value
  add
  -- Update storage
  setField #totalMinted
  setField #fields
  finishNoOp

mint :: forall fields. StorageFieldsC fields => Entrypoint MintParams fields
mint = do
  dip authorizeOperator
  mint_

-- | Add a new operator to the set of Operators. Only admin is allowed to call this
-- entrypoint.
addOperator :: forall fields. StorageFieldsC fields => Entrypoint OperatorParams fields
addOperator = addRemoveOperator AddOperator

-- | Add an operator from the set of Operators. Only admin is allowed to call this
-- entrypoint.
removeOperator :: StorageFieldsC fields => Entrypoint OperatorParams fields
removeOperator = addRemoveOperator RemoveOperator

-- | A type to indicate required action to the `addRemoveOperator` function.
data OperatorAction = AddOperator | RemoveOperator

-- | Adds or remove an operator.
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

-- | Start the transfer of ownership to a new owner. This stores the
-- address of the new owenr in the `newOwner` field in storage. Only
-- admin is allowed to make this call.
transferOwnership
  :: StorageFieldsC fields
  => Entrypoint TransferOwnershipParams fields
transferOwnership = do
  dip authorizeAdmin
  -- Unwrap new owner address
  fromNamed #newOwner
  dip (getField #fields)
  -- Make a `Some` value and..
  some
  -- set it as newOwner field.
  setField #newOwner
  setField #fields
  finishNoOp

setFields :: (StorageFieldsC fields) => Entrypoint fields fields
setFields = do
  setField #fields
  nil
  pair

-- | Accept ownership of the contract. This is only callable by
-- the address in `newOwner` field, if it contains one.
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

-- | This accepts a migration script (a lambda) and stores it
-- in the storage field `migrationScript'.
startMigrateTo
  :: forall fields. StorageFieldsC fields
  => Entrypoint StartMigrateToParams fields
startMigrateTo = do
  dip authorizeAdmin
  fromNamed #migrationManager
  stackType @'[MigrationManager, Storage' _]
  saveMigrationManager

-- | This accepts a contract address and store it in the `previousVersion`
-- storage field. This marks this contract to act as a successor of the input
-- contract, and thus enables the `previousContract` to mint tokens in this
-- contract via the `migrateFrom` call here.
startMigrateFrom
  :: forall fields. StorageFieldsC fields
  => Entrypoint StartMigrateFromParams fields
startMigrateFrom = do
  dip authorizeAdmin
  fromNamed #migrationManager
  some
  dip $ getField #fields
  setField #migrationManagerIn
  setField #fields
  finishNoOp

-- Check previous version and mint tokens for address
-- in param
mintForMigration
  :: forall fields. StorageFieldsC fields
  => Entrypoint MintForMigrationParams fields
mintForMigration = do
  dip ensureMigrationAgent
  mint_
  where
    ensureMigrationAgent = do
      getField #fields
      toField #migrationManagerIn
      ifSome (do; address; sender # eq) (failUsing MigrationNotEnabled)
      if_ nop $ failUsing SenderIsNotAgent

-- | This entry point just fetches the migrationScript from storage
-- and calls it passing senders address and balance migrating all the
-- accounts credits, it also burns all tokens for the sender in this contract.
migrate :: forall fields. StorageFieldsC fields => Entrypoint MigrateParams fields
migrate = do
  drop
  dup
  toField #ledger;
  sender
  get
  if IsSome then do
    toField #balance
    dup; int
    if IsZero
      then failUsing NoBalanceToMigrate
      else do
        stackType @'[Natural, Storage' fields]
        toNamed #value
        stackType @'["value" :! Natural, Storage' fields]
        sender
        swap
        dip $ toNamed #from
        stackType @'["value" :! Natural, "from" :! Address, Storage' fields]
        dup
        dip $ do
          pair
          burn_
          unpair
          drop
        stackType @'["value" :! Natural, Storage' fields]
        fromNamed #value
        stackType @'[Natural, Storage' fields]
        doMigrate
    else failUsing NoBalanceToMigrate
  where
    doMigrate :: '[Natural, Storage' fields] :-> '[([Operation], Storage' fields)]
    doMigrate = do
      stackType @'[Natural, Storage' fields]
      dip $ do
        getField #fields
        toField #migrationManagerOut
        if IsSome then do
          stackType @'[MigrationManager, Storage' fields]
          nop
        else
          failUsing MigrationNotEnabled
      stackType @'[Natural, MigrationManager, Storage' fields]
      sender
      pair
      push (toMutez 0)
      swap
      transferTokens
      nil
      swap
      cons
      pair

saveMigrationManager
  :: forall fields. StorageFieldsC fields
  => Entrypoint MigrationManager fields
saveMigrationManager = do
  dip $ do
    getField #fields
  some;
  setField #migrationManagerOut
  setFields

-- | Pause end user actions. This is callable only by the operators.
pause :: StorageFieldsC fields => Entrypoint () fields
pause = do
  dip authorizeOperator
  drop
  push True
  dip (getField #fields); setField #paused; setFields

-- | Resume end user actions if the contract is in a paused state.
-- This is callable only by the admin.
unpause :: StorageFieldsC fields => Entrypoint () fields
unpause = do
  drop
  push False
  ManagedLedger.setPause

-- | Check that the sender is admin
authorizeAdmin
  :: StorageFieldsC fields
  => Storage' fields : s :-> Storage' fields : s
authorizeAdmin = do
  getField #fields; toField #admin; sender; eq
  if_ nop (failUsing SenderIsNotAdmin)

-- | Check that the address of the sender is an address that is
-- present in the `newOwner` storage field.
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

-- | Check that the sender is an operator
authorizeOperator
  :: StorageFieldsC fields
  => Storage' fields : s :-> Storage' fields : s
authorizeOperator = do
  getField #fields; toField #operators; sender; mem;
  assert SenderIsNotOperator

finishNoOp :: '[st] :-> (ContractOut st)
finishNoOp = do;nil;pair
