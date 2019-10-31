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
  , ManagedLedger.getAdministrator
  , ManagedLedger.getAllowance
  , ManagedLedger.getBalance
  , ManagedLedger.getTotalSupply
  , ManagedLedger.transfer
  , acceptOwnership
  , addOperator
  , burn
  , getTotal
  , mint
  , pause
  , removeOperator
  , setRedeemAddress
  , transferOwnership
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
  , "ledger" := Address ~> LedgerValue
  ]

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


-- | Finish with an empty list of operations
finishNoOp :: '[st] :-> (ContractOut st)
finishNoOp = do nil;pair
