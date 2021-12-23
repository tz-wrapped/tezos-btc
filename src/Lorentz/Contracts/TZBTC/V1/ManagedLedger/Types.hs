{- SPDX-FileCopyrightText: 2020 Tocqueville Group
 -
 - SPDX-License-Identifier: LicenseRef-MIT-TQ
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.V1.ManagedLedger.Types
    ( module Lorentz.Contracts.TZBTC.V1.ManagedLedger.Types
    , Common.GetAllowanceParams
    , Common.TransferParams
    , Common.ApproveParams
    , Common.AllowanceParams
    , Common.MintParams
    ) where

import Lorentz

import Lorentz.Contracts.Spec.AbstractLedgerInterface ()
import qualified Lorentz.Contracts.TZBTC.Common.Types as Common

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
  , Dupable store
  )

type GetBalanceArg = View_ Common.GetBalanceParams Natural
type GetTotalSupplyArg = View_ () Natural

type GetAllowanceArg = View_ Common.GetAllowanceParams Natural

type ApproveCasParams =
  ("spender" :! Address, "value" :! Natural, "expected" :! Natural)

type BurnParams =
  ("from" :! Address, "value" :! Natural)
