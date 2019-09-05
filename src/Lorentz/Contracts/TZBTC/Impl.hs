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
  , approveViaProxy
  , burn
  , getTotal
  , mint
  , mintForMigration
  , migrate
  , pause
  , removeOperator
  , setProxy
  , setRedeemAddress
  , startMigrateFrom
  , startMigrateTo
  , transferOwnership
  , transferViaProxy
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
  , "proxy" := Either Address Address
  ]

type Entrypoint param fields
  = '[ param, Storage' fields ] :-> ContractOut (Storage' fields)

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

-- | Burns tokens for an account. Uses ML's `debitFrom` function
-- to actually update the ledger, and do ML's bookeeping. Also
-- update the value of `totalBurned` field in this contract.
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

-- | Mint tokens for an account. Uses ML's `creditTo` function
-- to actually update the ledger, and do ML's bookeeping. Also
-- update the value of `totalMinted` field in this contract.
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

-- | Mints tokens for an account
mint :: forall fields. StorageFieldsC fields => Entrypoint MintParams fields
mint = do
  dip authorizeOperator
  mint_

transferViaProxy
  :: forall fields. StorageFieldsC fields
  => Entrypoint TransferViaProxyParams fields
transferViaProxy = do
  dip authorizeProxy
  ManagedLedger.transfer'

approveViaProxy
  :: forall fields. StorageFieldsC fields
  => Entrypoint ApproveViaProxyParams fields
approveViaProxy = do
  dip authorizeProxy
  coerce_
  ManagedLedger.approve'

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
  -- Get set of operators from storage...
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

-- | Entry point handler to set redeem address
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

-- | Set inner `fields` value of the storage.
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
  drop
  -- Get `newOwner` address from storage
  getField #fields
  getField #newOwner
  ifSome (do
    setField #admin
    none
    setField #newOwner -- Reset newOwner field to None.
    setField #fields) (failUsing NotInTransferOwnershipMode)
  finishNoOp

-- | This accepts a `MigrationManager` (a proxy contract address) and
-- stores it in the storage field `MigrationManagerOut'.
startMigrateTo
  :: forall fields. StorageFieldsC fields
  => Entrypoint StartMigrateToParams fields
startMigrateTo = do
  dip $ do
    authorizeAdmin
    ensurePaused
  fromNamed #migrationManager
  stackType @'[MigrationManager, Storage' _]
  dip $ do
    getField #fields
  some;
  setField #migrationManagerOut
  setFields

-- | This accepts a `MigrationManager` (a proxy contract address) and stores it
-- in the `MigrationManagerIn` storage field.
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

-- Check the existence of migration agent and if it is equal to the
-- sender and mints tokens in this contract for the address in parameter
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
      ifSome (do address; sender # eq) (failUsing MigrationNotEnabled)
      if_ nop $ failUsing SenderIsNotAgent

-- | This entry point just fetches the migration manager from storage
-- (MigrationManagerOut) and calls it passing senders address and balance,
-- migrating all the account's credits. It also burns all tokens for the sender
-- in this contract. This entrypoint require contract to be running as it
-- is an end user call.
migrate :: forall fields. StorageFieldsC fields => Entrypoint MigrateParams fields
migrate = do
  dip ensureNotPaused
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

-- | Pause end user actions. This is callable only by the operator.
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

setProxy :: StorageFieldsC fields => Entrypoint SetProxyParams fields
setProxy = do
  -- Check sender
  dip $ do
    getField #fields
    toField #proxy
    ifLeft
      (do sender; if IsEq then nop else failUsing NotAllowedToSetProxy)
      (failUsing ProxyAlreadySet)
  right @Address
  dip $ getField #fields
  setField #proxy
  setFields

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

-- | Check that the contract is paused
ensurePaused
  :: forall fields. StorageFieldsC fields
  => '[Storage' fields] :-> '[Storage' fields]
ensurePaused = do
  getField #fields; toField #paused
  if_ (nop) (failUsing ContractIsNotPaused)

-- | Check that the contract is NOT paused
ensureNotPaused
  :: forall fields. StorageFieldsC fields
  => '[Storage' fields] :-> '[Storage' fields]
ensureNotPaused = do
  getField #fields; toField #paused
  if_ (failUsing ContractIsPaused) (nop)

-- | Check that the sender is proxy
authorizeProxy
  :: StorageFieldsC fields
  => Storage' fields ': s :-> Storage' fields ': s
authorizeProxy = do
  getField #fields
  toField #proxy
  ifLeft (failUsing ProxyIsNotSet) $ do
    sender
    if IsEq then nop else failUsing CallerIsNotProxy

-- | Finish with an empty list of operations
finishNoOp :: '[st] :-> (ContractOut st)
finishNoOp = do nil;pair
