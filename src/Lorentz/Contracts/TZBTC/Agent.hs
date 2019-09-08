{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.Agent
  ( Parameter
  , StorageFields(..)
  , agentContract
  , Error(..)
  , mkStorage
  ) where

import Lorentz
import Michelson.Typed.Haskell.Instr.Sum

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------
--
type CacheTokenParams = (Address, Natural)
type ForwardTokenParams = ()

data Parameter
  = CacheTokens !CacheTokenParams
  | ForwardTokens !ForwardTokenParams
  deriving stock Generic
  deriving anyclass IsoValue


data StorageFields tp = StorageFields
  { oldVersion :: Address
  , newVersion :: ContractAddr tp
  } deriving stock Generic
    deriving anyclass IsoValue

data Storage tp = Storage
  { cache :: BigMap Address Natural
  , fields :: StorageFields tp
  } deriving stock Generic
    deriving anyclass IsoValue

data Error
  = MigrationBadOrigin
  | CacheExists
  deriving stock (Eq, Generic)

deriveCustomError ''Error

type Entrypoint param fields
  = '[ param, Storage fields ] :-> ContractOut (Storage fields)

agentContract
  :: forall parameter.
  ( InstrWrapC parameter "cMintForMigration"
  , AppendCtorField
      (GetCtorField parameter "cMintForMigration") '[]
    ~ '[("to" :! Address, "value" :! Natural)]
  , KnownValue parameter, NoOperation parameter, NoBigMap parameter
  ) => Contract Parameter (Storage parameter)
agentContract = do
  unpair
  caseT @Parameter
    ( #cCacheTokens /-> cacheTokens
    , #cForwardTokens /-> forwardTokens
    )

cacheTokens ::
  forall cp.  Entrypoint CacheTokenParams cp
cacheTokens = do
  stackType @((Address, Natural): Storage cp : _)
  dip $ do
    ensureOldContract
  stackType @((Address, Natural): Storage cp : _)
  dup
  dip $ do
    stackType @((Address, Natural) : Storage cp : _)
    car
    stackType @(Address : Storage cp : _)
    dip $ getField #cache
    stackType @(Address : BigMap Address Natural : Storage cp : _)
    get
    stackType @(Maybe Natural : Storage cp : _)
    if IsSome
      then failUsing CacheExists
      else do
        stackType @(Storage cp : _)
        getField #cache
  stackType @((Address, Natural): BigMap Address Natural : Storage cp : _)
  unpair
  dip some
  update
  stackType @(BigMap Address Natural : Storage cp : _)
  setField #cache
  finishNoOp
    where
      ensureOldContract = do
        getField #fields
        toField #oldVersion
        sender
        if IsEq then
          nop
        else failUsing MigrationBadOrigin


forwardTokens ::
  Entrypoint ForwardTokenParams cp
forwardTokens = undefined

-- | Finish with an empty list of operations
finishNoOp :: '[st] :-> (ContractOut st)
finishNoOp = do nil;pair

mkStorage :: Address -> ContractAddr cp -> Storage cp
mkStorage a1 a2 = Storage mempty (StorageFields a1 a2)

-- | A migration agent that can migrate to any contract as long as the
-- target contract have a constructor/entrypoint `MintForMigration` with field
-- ("to" :! Address, "value" :! Natural)
--agentContract
--  :: forall parameter.
--  ( InstrWrapC parameter "cMintForMigration"
--  , AppendCtorField
--      (GetCtorField parameter "cMintForMigration") '[]
--    ~ '[("to" :! Address, "value" :! Natural)]
--  , KnownValue parameter, NoOperation parameter, NoBigMap parameter
--  ) => Contract Parameter (StorageFields parameter)
--agentContract = do
--  -- Pack input into parameter of target contract
--  stackType @('[((Address, Natural), StorageFields parameter)])
--  unpair
--  dip ensureOldContract
--  stackType @('[(Address, Natural), StorageFields parameter])
--  unpair
--  toNamed #to
--  dip $ toNamed #value
--  pair
--  stackType @('[("to" :! Address, "value" :! Natural), StorageFields parameter])
--  swap
--  dip $ do
--    wrap_ @parameter #cMintForMigration
--    stackType @('[parameter])
--  swap
--  stackType @('[parameter, StorageFields parameter])
--  -- Get new contract address from storage
--  -- and call it passing the input paramter
--  dip $ do
--    dup
--    toField #newVersion
--    push (toMutez 0)
--  stackType @('[parameter, Mutez, ContractAddr parameter, StorageFields parameter])
--  transferTokens
--  dip nil; cons;
--  pair
--    where
--      ensureOldContract = do
--        getField #oldVersion
--        sender
--        if IsEq then
--          nop
--        else failUsing MigrationBadOrigin
