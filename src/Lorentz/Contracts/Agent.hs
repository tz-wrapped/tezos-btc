{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.Agent
  ( Parameter
  , StorageFields(..)
  , agentContract
  , Error(..)
  ) where

import Lorentz
import Michelson.Typed.Haskell.Instr.Sum

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------
type Parameter = (Address, Natural)

data StorageFields tp = StorageFields
  { oldVersion :: Address
  , newVersion :: ContractAddr tp
  } deriving stock Generic
    deriving anyclass IsoValue

data Error
  = MigrationBadOrigin
  deriving stock (Eq, Generic)

-- | A migration agent that can migrate to any contract as long as the
-- target contract have a constructor/entrypoint `MintForMigration` with field
-- ("to" :! Address, "value" :! Natural)
agentContract
  :: forall parameter.
  ( InstrWrapC parameter "cMintForMigration"
  , AppendCtorField
      (GetCtorField parameter "cMintForMigration") '[]
    ~ '[("to" :! Address, "value" :! Natural)]
  , KnownValue parameter, NoOperation parameter, NoBigMap parameter
  ) => Contract Parameter (StorageFields parameter)
agentContract = do
  -- Pack input into parameter of target contract
  stackType @('[((Address, Natural), StorageFields parameter)])
  unpair
  dip ensureOldContract
  stackType @('[(Address, Natural), StorageFields parameter])
  unpair
  toNamed #to
  dip $ toNamed #value
  pair
  stackType @('[("to" :! Address, "value" :! Natural), StorageFields parameter])
  swap
  dip $ do
    wrap_ @parameter #cMintForMigration
    stackType @('[parameter])
  swap
  stackType @('[parameter, StorageFields parameter])
  -- Get new contract address from storage
  -- and call it passing the input paramter
  dip $ do
    dup
    toField #newVersion
    push (toMutez 0)
  stackType @('[parameter, Mutez, ContractAddr parameter, StorageFields parameter])
  transferTokens
  dip nil; cons;
  pair
    where
      ensureOldContract = do
        getField #oldVersion
        sender
        if IsEq then
          nop
        else failUsing MigrationBadOrigin

deriveCustomError ''Error
