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
  | NoCache
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

-- | Cache tokens from old contract in first step of migration.
cacheTokens ::
  forall cp.  Entrypoint CacheTokenParams cp
cacheTokens = do
  dip $ do
    ensureOldContract
  -- Check if an entry exists for user
  -- if yes, throw error or else fetch cache from storage
  dup
  dip $ do
    car
    dip $ getField #cache
    get
    if IsSome
      then failUsing CacheExists
      else do
        getField #cache
  -- Set token count for address
  unpair
  dip some
  update
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

-- | Forward cached tokens from first step of migration and mint
-- them in new version of contract.
forwardTokens
  :: forall parameter.
     ( InstrWrapC parameter "cMintForMigration"
     , AppendCtorField
         (GetCtorField parameter "cMintForMigration") '[]
       ~ '[("to" :! Address, "value" :! Natural)]
     , KnownValue parameter, NoOperation parameter, NoBigMap parameter
     ) => Entrypoint ForwardTokenParams parameter
forwardTokens = do
  drop
  sender
  dup
  dip $ do
    dip $ getField #cache
    get
    stackType @(Maybe Natural : Storage parameter: '[])
    if IsSome
      then do
        stackType @(Natural : Storage parameter: '[])
      else failUsing NoCache
  stackType @(Address : Natural : Storage parameter: '[])
  -- burn all cached tokens for sender
  dup
  stackType @(Address : Address : Natural : Storage parameter: '[])
  dip $ do
    swap
    stackType @(Natural : Address : Storage parameter: '[])
    dip $ do
      stackType @(Address : Storage parameter: '[])
      dip $ do
        getField #cache
        none
      stackType @(Address : Maybe Natural : BigMap Address Natural : Storage parameter: '[])
      update
      stackType @(BigMap Address Natural : Storage parameter: '[])
      setField #cache
  -- Mint tokens in new version
  stackType @(Address : Natural : Storage parameter: '[])
  toNamed #to
  dip $ toNamed #value
  pair
  stackType @(("to" :! Address , "value" :! Natural) : Storage parameter: '[])
  swap
  stackType @(Storage parameter: ("to" :! Address , "value" :! Natural) : '[])
  dip $ do
    wrap_ @parameter #cMintForMigration
    stackType @('[parameter])
  dup
  stackType @('[Storage parameter, Storage parameter, parameter])
  dip $ do
    swap
    dip $ do
      toField #fields
      toField #newVersion
    stackType @(parameter ': ContractAddr parameter ': '[])
    dip $ do
      push (toMutez 0)
    stackType @(parameter ': Mutez ': ContractAddr parameter ': '[])
    transferTokens
    dip nil; cons;
  swap
  pair

-- | Finish with an empty list of operations
finishNoOp :: '[st] :-> (ContractOut st)
finishNoOp = do nil;pair

-- | Make storage for migration manager contract
mkStorage :: Address -> ContractAddr cp -> Storage cp
mkStorage a1 a2 = Storage mempty (StorageFields a1 a2)
