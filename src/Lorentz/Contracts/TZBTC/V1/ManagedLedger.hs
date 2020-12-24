{- SPDX-FileCopyrightText: 2020 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

-- | Some code of ManagedLedger copy-pasted from morley-ledgers package.
--
-- The lattest version of morley-ledgers is not compatible with V1 of our
-- contract so we keep the old pieces of ManagedLedger contract here.
module Lorentz.Contracts.TZBTC.V1.ManagedLedger
  ( module X
  ) where

import Lorentz.Contracts.TZBTC.V1.ManagedLedger.Impl as X
import Lorentz.Contracts.TZBTC.V1.ManagedLedger.Types as X
