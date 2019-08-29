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
  , mint
  , migrate
  , migrateFrom
  , mkMigrationScript
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
import qualified Lorentz.Contracts.UserUpgradeable.Migrations as Migrations
import Michelson.Typed.Haskell.Instr.Sum

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
  , "previousVersion" := Maybe Address
  , "migrationScript" := Maybe Migrations.MigrationScript
  ]

type Entrypoint param fields
  = '[ param, Storage' fields ] :-> ContractOut (Storage' fields)

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
  fromNamed #startMigrateTo
  stackType @'[Migrations.MigrationScript, Storage' _]
  usingFields Migrations.initiateMigration

-- | This accepts a contract address and store it in the `previousVersion`
-- storage field. This marks this contract to act as a successor of the input
-- contract, and thus enables the `previousContract` to mint tokens in this
-- contract via the `migrateFrom` call here.
startMigrateFrom
  :: forall fields. StorageFieldsC fields
  => Entrypoint StartMigrateFromParams fields
startMigrateFrom = do
  dip authorizeAdmin
  fromNamed #startMigrateFrom
  some
  dip $ getField #fields
  setField #previousVersion
  setField #fields
  finishNoOp

-- Check previous version and mint tokens for address
-- in param
migrateFrom
  :: forall fields. StorageFieldsC fields
  => Entrypoint MigrateFromParams fields
migrateFrom = do
  dip ensurePrevVersion
  unpair
  fromNamed #migrateMintTo
  toNamed #to
  pair
  mint_
  where
    ensurePrevVersion = do
      getField #fields
      toField #previousVersion
      ifSome (sender # eq) (push False)
      if_ nop $ failUsing UnauthorizedMigrateFrom

-- | Here we basically extract the fields from our storage, and
-- pass it as storage to the `proc` procedure
-- Then we write the storage component of the result to the `fields`
-- of our real storage, and the [operations] of the result as the
-- result of this call.
usingFields
  :: forall a fields. (StorageFieldsC fields)
  => '[a, fields] :-> '[([Operation], fields)]
  -> Entrypoint a fields
usingFields proc = do
  swap
  dup
  stackType @'[Storage' _, Storage' _, a]
  dip $ do
    toField #fields
    swap
    stackType @'[a, fields]
    proc
    stackType @'[([Operation], fields)]
  stackType @'[Storage' fields, ([Operation], fields)]
  swap
  stackType @'[([Operation], fields), Storage' fields]
  unpair
  stackType @'[[Operation], fields, Storage' fields]
  dip $ setField #fields
  pair

-- | This entry point just fetches the migrationScript from storage
-- and calls it passing senders address and balance migrating all the
-- accounts credits.
migrate :: forall fields. StorageFieldsC fields => Entrypoint MigrateParams fields
migrate = do
  drop
  dup
  toField #ledger;
  sender
  get
  if IsSome then do
    toField #balance;
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
    else finishNoOp
  where
    doMigrate :: '[Natural, Storage' fields] :-> '[([Operation], Storage' fields)]
    doMigrate = usingFields Migrations.callMigrationScript

-- | Creates a lambda that, given a sender and the amount of tokens, returns
-- an operation to call the new version of the contract. The admin of the old
-- version is supposed to supply this lambda to the old version of the contract
-- during an `InitiateMigration` call. Unlike the old version itself, this
-- lambda knows the parameter type of the new contract, and thus can forge an
-- operation in Athens.
mkMigrationScript
  :: forall parameter.
  ( InstrWrapC parameter "cMigrateFrom"
  , AppendCtorField (GetCtorField parameter "cMigrateFrom") '[] ~ '[(Address, Natural)]
  , KnownValue parameter, NoOperation parameter, NoBigMap parameter
  )
  => ContractAddr parameter -> Migrations.MigrationScript
mkMigrationScript targetContract = do
  stackType @('[(Address, Natural)])
  wrap_ #cMigrateFrom
  stackType @('[parameter])
  dip $ do
    push targetContract
    push (toMutez 0)
  transferTokens

-- | Pause end user actions. This is callable only by the operators.
pause :: StorageFieldsC fields => Entrypoint () fields
pause = do
  dip authorizeOperator
  drop
  push True
  dip (getField #fields); setField #paused; setField #fields
  nil; pair

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
