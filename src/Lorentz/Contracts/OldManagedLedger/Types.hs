{- SPDX-FileCopyrightText: 2020 Tocqueville Group
 -
 - SPDX-License-Identifier: LicenseRef-MIT-TQ
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.OldManagedLedger.Types
    ( module Lorentz.Contracts.OldManagedLedger.Types
    ) where

import Lorentz

import Lorentz.Contracts.Spec.AbstractLedgerInterface ()

type LedgerValue =
  ("balance" :! Natural, "approvals" :! Map Address Natural)

type LedgerC store = StorageContains store
    [ "totalSupply" := Natural
    , "ledger" := Address ~> LedgerValue
    ]

type StorageC store =
  ( LedgerC store
  , StorageContains store
   [ "admin" := Address
   , "paused" := Bool
   ]
  )

type TransferParams = ("from" :! Address, "to" :! Address, "value" :! Natural)
type GetBalanceParams = ("owner" :! Address)

type GetBalanceArg = View GetBalanceParams Natural
type GetTotalSupplyArg = View () Natural

type ApproveParams = ("spender" :! Address, "value" :! Natural)
type GetAllowanceParams = ("owner" :! Address, "spender" :! Address)
type GetAllowanceArg = View GetAllowanceParams Natural

type ApproveCasParams =
  ("spender" :! Address, "value" :! Natural, "expected" :! Natural)

type AllowanceParams =
  ("owner" :! Address, "spender" :! Address, "value" :! Natural)

type MintParams =
  ("to" :! Address, "value" :! Natural)

type BurnParams =
  ("from" :! Address, "value" :! Natural)
