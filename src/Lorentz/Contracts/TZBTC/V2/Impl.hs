{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.V2.Impl
  ( Entrypoint
  , StorageC
  , ManagedLedger.approve
  , ManagedLedger.getAllowance
  , ManagedLedger.getBalance
  , ManagedLedger.getTotalSupply
  , ManagedLedger.transfer
  , V1.acceptOwnership
  , V1.addOperator
  , burn
  , V1.getOwner
  , V1.getTotalBurned
  , V1.getTotalMinted
  , V1.getRedeemAddress
  , V1.getTokenMetadata
  , mint
  , V1.pause
  , V1.removeOperator
  , V1.setRedeemAddress
  , V1.transferOwnership
  , V1.unpause
  ) where

import Lorentz
import qualified Lorentz.Contracts.ManagedLedger.Impl as ManagedLedger
import Lorentz.Contracts.Metadata
import qualified Lorentz.Contracts.Spec.ManagedLedgerInterface as ManagedLedger
import Lorentz.Contracts.TZBTC.Common.Types hiding (AddOperator, RemoveOperator)
import qualified Lorentz.Contracts.TZBTC.V1.Impl as V1
import Lorentz.Contracts.TZBTC.V2.Types

type StorageC store = StorageContains store
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
  , "approvals" := GetAllowanceParams ~> Natural
  ]

-- | Burn the specified amount of tokens from redeem address. Since it
-- is not possible to burn from any other address, this entry point does
-- not have an address input. Only operators are allowed to call this entry
-- point.
burn :: forall store. StorageC store => Entrypoint BurnParams store
burn = do
  doc $ DDescription "Burn some tokens from the `redeem` address."
  dip V1.authorizeOperator
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
  dip V1.authorizeOperator
  mint_

-- | Finish with an empty list of operations
finishNoOp :: '[st] :-> (ContractOut st)
finishNoOp = do nil;pair
