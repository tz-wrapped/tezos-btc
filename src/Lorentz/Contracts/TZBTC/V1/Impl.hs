{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.V1.Impl
  ( Entrypoint
  , StorageC
  , ManagedLedger.approve
  , ManagedLedger.getAllowance
  , ManagedLedger.getBalance
  , ManagedLedger.getTotalSupply
  , ManagedLedger.transfer
  , acceptOwnership
  , addOperator
  , burn
  , getOwner
  , getTotalBurned
  , getTotalMinted
  , getRedeemAddress
  , getTokenMetadata
  , mint
  , pause
  , removeOperator
  , setRedeemAddress
  , transferOwnership
  , unpause

    -- * Helpers
  , authorizeOperator
  ) where

import Fmt (Builder)

import Morley.Util.Markdown (mdTicked)

import Lorentz
import Lorentz.Contracts.Metadata
import Lorentz.Contracts.TZBTC.Common.Types hiding (AddOperator, RemoveOperator)
import Lorentz.Contracts.TZBTC.V1.ManagedLedger qualified as ManagedLedger
import Lorentz.Contracts.TZBTC.V1.Types

type StorageC store =
  ( StorageContains store
    [ "owner" := Address
    , "paused" := Bool
    , "totalSupply" := Natural
    , "totalMinted" := Natural
    , "totalBurned" := Natural
    , "operators" := Set Address
    , "redeemAddress" := Address
    , "tokenMetadata" := TokenMetadata
    , "newOwner" := Maybe Address
    , "ledger" := Address ~> LedgerValue
    ]
  , Dupable store
  )

-- | A view entrypoint that has no arguments and returns a single
-- value from the storage as is.
getSingleField
  :: forall store label val.
    (StoreHasField store label val, NiceParameter val, Dupable store)
  => Label label -> Markdown -> Entrypoint (View_ () val) store
getSingleField label entrypointDocDesc = do
  doc $ DDescription $ "This view returns " <> entrypointDocDesc <> "."
  view_ $ do drop @(); stToField $ fieldNameFromLabel label

getTotalMinted
  :: forall store. StorageC store => Entrypoint (View_ () Natural) store
getTotalMinted = getSingleField #totalMinted "the total number of minted tokens"

getTotalBurned
  :: forall store. StorageC store => Entrypoint (View_ () Natural) store
getTotalBurned = getSingleField #totalBurned "the total number of burned tokens"

getOwner
  :: forall store. StorageC store => Entrypoint (View_ () Address) store
getOwner = getSingleField #owner "the current contract owner"

getRedeemAddress
  :: forall store. StorageC store => Entrypoint (View_ () Address) store
getRedeemAddress = getSingleField #redeemAddress "the redeem address"

-- | Assert that all of the given `TokenId`'s are @0@ (since this is a single
-- token contract) and replace each with the current `TokenMetadata`.
getTokenMetadata
  :: forall store. StorageC store => Entrypoint (View_ [TokenId] [TokenMetadata]) store
getTokenMetadata =  do
  doc $ DDescription $ "This view returns the token metadata."
  view_ $ do
    dip $ stToField #tokenMetadata
    singleTokenResolveMetadata

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
  doc $ DDescription
    "This entry point is used mint new tokes for an account."
  dip authorizeOperator
  mint_

-- | Add a new operator to the set of Operators. Only owner is allowed to call this
-- entrypoint.
addOperator :: forall store. StorageC store => Entrypoint OperatorParams store
addOperator = do
  addRemoveOperator AddOperatorAction

-- | Add an operator from the set of Operators. Only owner is allowed to call this
-- entrypoint.
removeOperator :: StorageC store => Entrypoint OperatorParams store
removeOperator = do
  addRemoveOperator RemoveOperatorAction

-- | A type to indicate required action to the `addRemoveOperator` function.
data OperatorAction = AddOperatorAction | RemoveOperatorAction

-- | Adds or remove an operator.
addRemoveOperator
  :: forall store. StorageC store
  => OperatorAction -> Entrypoint OperatorParams store
addRemoveOperator ar = do
  dip authorizeOwner
  stackType @'[OperatorParams, store]
  -- Unwrap operator address
  fromNamed #operator
  -- Get set of operators from storage...
  dip $ do
    stGetField #operators
    push $ case ar of
      AddOperatorAction -> True
      RemoveOperatorAction -> False
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
  dip authorizeOwner
  -- Unwrap operator address
  fromNamed #redeem
  -- Set redeem address
  stSetField #redeemAddress
  finishNoOp

-- | Start the transfer of ownership to a new owner. This stores the
-- address of the new owner in the `newOwner` field in storage. Only
-- owner is allowed to make this call.
transferOwnership
  :: StorageC store
  => Entrypoint TransferOwnershipParams store
transferOwnership = do
  dip authorizeOwner
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
  dip authorizeNewOwner
  drop
  -- Get `newOwner` address from storage
  stGetField #newOwner
  ifSome (do
    stSetField #owner
    none
    stSetField #newOwner -- Reset newOwner field to None.
    ) (failCustom_ #notInTransferOwnershipMode)
  finishNoOp

-- | Pause end user actions. This is callable only by the operator.
pause :: StorageC store => Entrypoint () store
pause = do
  dip authorizeOperator
  drop
  push True
  stSetField #paused
  finishNoOp

-- | Resume end user actions if the contract is in a paused state.
-- This is callable only by the owner.
unpause :: StorageC store => Entrypoint () store
unpause = do
  drop
  push False
  setPause

setPause :: StorageC store => Entrypoint Bool store
setPause = do
  dip authorizeOwner
  stSetField #paused
  nil; pair

data DRequireRole = DRequireRole Builder

instance DocItem DRequireRole where
  docItemPos = 53
  docItemSectionName = Nothing
  docItemToMarkdown _ (DRequireRole role) =
    "The sender has to be the " <> mdTicked role <> "."

-- | Check that the sender is owner
authorizeOwner
  :: StorageC store
  => store : s :-> store : s
authorizeOwner = do
  doc $ DRequireRole "owner"
  stGetField #owner; sender; eq
  if_ nop (failCustom_ #senderIsNotOwner)

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
  :: (StorageContains store '["operators" := Set Address], Dupable store)
  => store : s :-> store : s
authorizeOperator = do
  doc $ DRequireRole "operator"
  stGetField #operators; sender; mem;
  assert (CustomError #senderIsNotOperator
          (errorTagToMText #senderIsNotOperator, ())
         )

-- | Finish with an empty list of operations
finishNoOp :: '[st] :-> (ContractOut st)
finishNoOp = do nil;pair
