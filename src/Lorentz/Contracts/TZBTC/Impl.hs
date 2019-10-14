{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.Impl
  ( Entrypoint
  , StorageC
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
import Fmt (Builder)

import Util.Markdown (mdTicked)

import Lorentz
import qualified Lorentz.Contracts.ManagedLedger.Impl as ManagedLedger
import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import Lorentz.Contracts.ManagedLedger.Athens ()
import Lorentz.Contracts.TZBTC.Types hiding (AddOperator, RemoveOperator)

type StorageC store = StorageContains store
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
  , "ledger" := Address ~> LedgerValue
  , "proxy" := Either Address Address
  ]

type Entrypoint param store
  = '[ param, store ] :-> ContractOut store

getTotal
  :: forall store a.
    (StoreHasField store a Natural)
  => Label a -> Markdown -> Entrypoint (View () Natural) store
getTotal bp entrypointDoc = do
  doc $ DDescription entrypointDoc
  view_ $ do cdr; stToField bp

-- | Burn the specified amount of tokens from redeem address. Since it
-- is not possible to burn from any other address, this entry point does
-- not have an address input. Only operators are allowed to call this entry
-- point.
burn :: forall store. StorageC store => Entrypoint BurnParams store
burn = do
  doc $ DDescription "Burn some tokens from the `redeem` address."
  dip authorizeOperator
  -- Get redeem address from storage
  dip $ do
    dup
    stackType @'[store, store]
    stToField #redeemAddress
    toNamed #from
  -- Make ManagedLedger's burn entrypoint parameter
  stackType @'["value" :! Natural, "from" :! Address, store]
  pair
  burn_

-- | Burns tokens for an account. Uses ML's `debitFrom` function
-- to actually update the ledger, and do ML's bookeeping. Also
-- update the value of `totalBurned` field in this contract.
burn_
  :: forall store. StorageC store
  => Entrypoint ("value" :! Natural, "from" :! Address) store
burn_ = do
  unpair
  dup
  dip $ do
    swap
    pair
    stackType @'[ManagedLedger.BurnParams, store]
    -- Call ManagedLedgers's `debitFrom` function.
    ManagedLedger.debitFrom
    -- Drop burn prameters
    drop
    stackType @'[store]
  stackType @'["value" :! Natural, store]
  -- Now add the value to total burned field
  -- assuming totalSupply field to be update by
  -- managedLedger's burn procedure.
  dip $ stGetField #totalBurned
  fromNamed #value
  add
  -- Update storage
  stSetField #totalBurned
  finishNoOp

-- | Mint tokens for an account. Uses ML's `creditTo` function
-- to actually update the ledger, and do ML's bookeeping. Also
-- update the value of `totalMinted` field in this contract.
mint_ :: forall store. StorageC store => Entrypoint MintParams store
mint_ = do
  -- Make managed ledger's mint entrypoint parameter
  stackType @'[("to" :! Address, "value" :! Natural), store]
  dup
  dip $ do
    stackType @'[ManagedLedger.MintParams, store]
    -- update ledger
    ManagedLedger.creditTo
    -- Drop mint prameters
    drop
    stackType @'[store]
  cdr
  stackType @'["value" :! Natural, store]
  -- Now add the value to total minted field
  -- assuming totalSupply field to be updated by
  -- managedLedger's mint procedure.
  dip $ stGetField #totalMinted
  fromNamed #value
  add
  -- Update storage
  stSetField #totalMinted
  finishNoOp

-- | Mints tokens for an account
mint :: forall store. StorageC store => Entrypoint MintParams store
mint = do
  doc $ DDescription "Mint tokens to the given address."
  dip authorizeOperator
  mint_

transferViaProxy
  :: forall store. StorageC store
  => Entrypoint TransferViaProxyParams store
transferViaProxy = do
  doc $ DDescription "Proxy version of Transfer entrypoint."
  dip authorizeProxy
  ManagedLedger.transfer'

approveViaProxy
  :: forall store. StorageC store
  => Entrypoint ApproveViaProxyParams store
approveViaProxy = do
  doc $ DDescription "Proxy version of Approve entrypoint."
  dip authorizeProxy
  coerce_
  ManagedLedger.approve'

-- | Add a new operator to the set of Operators. Only admin is allowed to call this
-- entrypoint.
addOperator :: forall store. StorageC store => Entrypoint OperatorParams store
addOperator = do
  doc $ DDescription "Add operator with given address."
  addRemoveOperator AddOperator

-- | Add an operator from the set of Operators. Only admin is allowed to call this
-- entrypoint.
removeOperator :: StorageC store => Entrypoint OperatorParams store
removeOperator = do
  doc $ DDescription "Remove operator with given address."
  addRemoveOperator RemoveOperator

-- | A type to indicate required action to the `addRemoveOperator` function.
data OperatorAction = AddOperator | RemoveOperator

-- | Adds or remove an operator.
addRemoveOperator
  :: forall store.
      StorageC store
  => OperatorAction -> Entrypoint OperatorParams store
addRemoveOperator ar = do
  dip authorizeAdmin
  stackType @'[OperatorParams, store]
  -- Unwrap operator address
  fromNamed #operator
  -- Get set of operators from storage...
  dip $ do
    stGetField #operators
    push $ case ar of
      AddOperator -> True
      RemoveOperator -> False
  -- and insert/remove the operator address depending
  -- on the value of boolean flag ar
  update
  -- Update storage with the new operator set
  stSetField #operators
  finishNoOp

-- | Entry point handler to set redeem address
setRedeemAddress
  :: StorageC store
  => Entrypoint SetRedeemAddressParams store
setRedeemAddress = do
  doc $ DDescription "Update `redeem` address, from which tokens can be burned."
  dip authorizeAdmin
  -- Unwrap operator address
  fromNamed #redeem
  -- Set redeem address
  stSetField #redeemAddress
  finishNoOp

-- | Start the transfer of ownership to a new owner. This stores the
-- address of the new owner in the `newOwner` field in storage. Only
-- admin is allowed to make this call.
transferOwnership
  :: StorageC store
  => Entrypoint TransferOwnershipParams store
transferOwnership = do
  doc $ DDescription "Start the transfer ownership to a new owner."
  dip authorizeAdmin
  -- Unwrap new owner address
  fromNamed #newOwner
  -- Make a `Some` value and..
  some
  -- set it as newOwner field.
  stSetField #newOwner
  finishNoOp

-- | Accept ownership of the contract. This is only callable by
-- the address in `newOwner` field, if it contains one.
acceptOwnership :: StorageC store => Entrypoint () store
acceptOwnership = do
  doc $ DDescription "Accept the ownership of the contract."
  dip authorizeNewOwner
  drop
  -- Get `newOwner` address from storage
  stGetField #newOwner
  ifSome (do
    stSetField #admin
    none
    stSetField #newOwner -- Reset newOwner field to None.
    ) (failCustom_ #notInTransferOwnershipMode)
  finishNoOp

-- | This accepts a `MigrationManager` (a proxy contract address) and
-- stores it in the storage field `MigrationManagerOut'.
startMigrateTo
  :: forall store. StorageC store
  => Entrypoint StartMigrateToParams store
startMigrateTo = do
  doc $ DDescription "Initialize process of migration from the old contract."
  dip $ do
    authorizeAdmin
    ensurePaused
  fromNamed #migrationManager
  stackType @'[MigrationManager, store]
  some;
  stSetField #migrationManagerOut
  finishNoOp

-- | This accepts a `MigrationManager` (a proxy contract address) and stores it
-- in the `MigrationManagerIn` storage field.
startMigrateFrom
  :: forall store. StorageC store
  => Entrypoint StartMigrateFromParams store
startMigrateFrom = do
  doc $ DDescription "Initialize process of migration from the new contract."
  dip authorizeAdmin
  fromNamed #migrationManager
  some
  stSetField #migrationManagerIn
  finishNoOp

-- Check the existence of migration agent and if it is equal to the
-- sender and mints tokens in this contract for the address in parameter
mintForMigration
  :: forall store. StorageC store
  => Entrypoint MintForMigrationParams store
mintForMigration = do
  doc $ DDescription "Mint tokens to the given address from the agent."
  dip ensureMigrationAgent
  mint_
  where
    ensureMigrationAgent = do
      stGetField #migrationManagerIn
      ifSome (do sender # eq) (failCustom_ #migrationNotEnabled)
      if_ nop $ failCustom_ #senderIsNotAgent

-- | This entry point just fetches the migration manager from storage
-- (MigrationManagerOut) and calls it passing senders address and balance,
-- migrating all the account's credits. It also burns all tokens for the sender
-- in this contract. This entrypoint require contract to be running as it
-- is an end user call.
migrate :: forall store. StorageC store => Entrypoint MigrateParams store
migrate = do
  doc $ DDescription
    "Migrate from one verstion of the contract to another. \
    \Thus tokens in the old version is burned and minted in the new."
  dip ensureNotPaused
  drop
  dup
  sender
  stGet #ledger
  if IsSome then do
    toField #balance
    dup; int
    if IsZero
      then failCustom_ #noBalanceToMigrate
      else do
        stackType @'[Natural, store]
        toNamed #value
        stackType @'["value" :! Natural, store]
        sender
        swap
        dip $ toNamed #from
        stackType @'["value" :! Natural, "from" :! Address, store]
        dup
        dip $ do
          pair
          burn_
          unpair
          drop
        stackType @'["value" :! Natural, store]
        fromNamed #value
        stackType @'[Natural, store]
        doMigrate
    else failCustom_ #noBalanceToMigrate
  where
    doMigrate :: '[Natural, store] :-> '[([Operation], store)]
    doMigrate = do
      stackType @'[Natural, store]
      dip $ do
        stGetField #migrationManagerOut
        if IsSome then do
          stackType @'[MigrationManager, store]
          contract
          stackType @'[Maybe MigrationManagerCType, store]
          if IsSome then do
            nop
          else
            failCustom_ #illTypedMigrationManager
        else
          failCustom_ #migrationNotEnabled
      stackType @'[Natural, MigrationManagerCType, store]
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
pause :: StorageC store => Entrypoint () store
pause = do
  doc $ DDescription
    "Pause the contract, after the pause all end users actions are prohibited."
  dip authorizeOperator
  drop
  push True
  stSetField #paused
  finishNoOp

-- | Resume end user actions if the contract is in a paused state.
-- This is callable only by the admin.
unpause :: StorageC store => Entrypoint () store
unpause = do
  doc $ DDescription "Unpause the contract and resume end users actions."
  drop
  push False
  ManagedLedger.setPause

setProxy :: StorageC store => Entrypoint SetProxyParams store
setProxy = do
  doc $ DDescription "Set address of the proxy contract."
  -- Check sender
  dip $ do
    stGetField #proxy
    ifLeft
      (do sender; if IsEq then nop else failCustom_ #notAllowedToSetProxy)
      (failCustom_ #proxyAlreadySet)
  right @Address
  stSetField #proxy
  finishNoOp

data DRequireRole = DRequireRole Builder

instance DocItem DRequireRole where
  type DocItemPosition DRequireRole = 53
  docItemSectionName = Nothing
  docItemToMarkdown _ (DRequireRole role) =
    "The sender has to be the " <> mdTicked role <> "."

-- | Check that the sender is admin
authorizeAdmin
  :: StorageC store
  => store : s :-> store : s
authorizeAdmin = do
  doc $ DRequireRole "admin"
  stGetField #admin; sender; eq
  if_ nop (failCustom_ #senderIsNotAdmin)

-- | Check that the address of the sender is an address that is
-- present in the `newOwner` storage field.
authorizeNewOwner
  :: StorageC store
  => store : s :-> store : s
authorizeNewOwner = do
  stGetField #newOwner;
  doc $ DRequireRole "new owner"
  if IsSome then do
    sender
    eq
    if_ nop (failCustom_ #senderIsNotNewOwner)
    else (failCustom_ #notInTransferOwnershipMode)

-- | Check that the sender is an operator
authorizeOperator
  :: StorageC store
  => store : s :-> store : s
authorizeOperator = do
  doc $ DRequireRole "operator"
  stGetField #operators; sender; mem;
  assert (CustomError #senderIsNotOperator)

-- | Check that the contract is paused
ensurePaused
  :: forall store. StorageC store
  => '[store] :-> '[store]
ensurePaused = do
  stGetField #paused
  if_ (nop) (failCustom_ #tokenOperationsAreNotPaused)

-- | Check that the contract is NOT paused
ensureNotPaused
  :: forall store. StorageC store
  => '[store] :-> '[store]
ensureNotPaused = do
  stGetField #paused
  if_ (failCustom_ #tokenOperationsArePaused) (nop)

-- | Check that the sender is proxy
authorizeProxy
  :: StorageC store
  => store ': s :-> store ': s
authorizeProxy = do
  doc $ DRequireRole "proxy"
  stGetField #proxy
  ifLeft (failCustom_ #proxyIsNotSet) $ do
    sender
    if IsEq then nop else failCustom_ #callerIsNotProxy

-- | Finish with an empty list of operations
finishNoOp :: '[st] :-> (ContractOut st)
finishNoOp = do nil;pair
