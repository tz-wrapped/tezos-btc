{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Lorentz.Contracts.TZBTC.Types
  ( BurnParams
  , OperatorParams
  , GetBalanceParams
  , SetRedeemAddressParams
  , PauseParams
  , AcceptOwnershipParams
  , TransferOwnershipParams
  , StartMigrateToParams
  , StartMigrateFromParams
  , MigrateParams
  , Storage
  , mkStorage
  , ManagedLedger.LedgerValue
  , ManagedLedger.Error (..)
  , ManagedLedger.Storage' (..)
  , ManagedLedger.TransferParams
  , ManagedLedger.ApproveParams
  , ManagedLedger.AllowanceParams
  , ManagedLedger.GetAllowanceParams
  , ManagedLedger.MintParams
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Lorentz
import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import Lorentz.Contracts.ManagedLedger.Types (Storage', mkStorage')
import Util.Instances ()

type BurnParams = ("from" :! Address, "value" :! Natural)
type OperatorParams = ("operator" :! Address)
type GetBalanceParams = Address
type SetRedeemAddressParams = ("redeem" :! Address)
type PauseParams = Bool
type TransferOwnershipParams = ("newowner" :! Address)
type StartMigrateToParams = ("migrateto" :! Address)
type StartMigrateFromParams = ("migratefrom" :! Address)
type AcceptOwnershipParams = ()
type MigrateParams = ()

data StorageFields = StorageFields
  { admin       :: Address
  , paused      :: Bool
  , totalSupply :: Natural
  , totalBurned :: Natural
  , totalMinted :: Natural
  , newOwner    :: Maybe Address
  , operators   :: Set Address
  , migrateFrom :: Maybe Address
  , migrateTo   :: Maybe Address
  } deriving stock Generic -- @TODO Is TokenName required here?
    deriving anyclass IsoValue

type Storage = Storage' StorageFields

-- | Create a default storage with ability to set some balances to
-- non-zero values.
mkStorage :: Address -> Map Address Natural -> Storage
mkStorage adminAddress balances = mkStorage' balances $
  StorageFields
  { admin = adminAddress
  , paused = False
  , totalSupply = sum balances
  , totalBurned = 0
  , totalMinted = sum balances
  , newOwner = Nothing
  , operators = Set.empty
  , migrateFrom = Nothing
  , migrateTo = Nothing
  }
