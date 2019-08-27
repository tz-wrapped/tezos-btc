{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Lorentz.Contracts.TZBTC.Types
  ( BurnParams
  , AcceptOwnershipParams
  , Error(..)
  , GetBalanceParams
  , ManagedLedger.AllowanceParams
  , ManagedLedger.ApproveParams
  , ManagedLedger.GetAllowanceParams
  , ManagedLedger.LedgerValue
  , ManagedLedger.MintParams
  , ManagedLedger.Storage' (..)
  , ManagedLedger.TransferParams
  , MigrateParams
  , OperatorParams
  , PauseParams
  , SetRedeemAddressParams
  , StartMigrateFromParams
  , StartMigrateToParams
  , Storage
  , StorageFields(..)
  , TransferOwnershipParams
  , mkStorage
  ) where

import Data.Set (Set)

import Lorentz
import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import Lorentz.Contracts.ManagedLedger.Types (Storage'(..), mkStorage')
import Util.Instances ()

type BurnParams = ("value" :! Natural)
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
  , redeemAddress :: Address
  } deriving stock Generic -- @TODO Is TokenName required here?
    deriving anyclass IsoValue

data Error
  = UnsafeAllowanceChange Natural
    -- ^ Attempt to change allowance from non-zero to a non-zero value.
  | SenderIsNotAdmin
    -- ^ Contract initiator has not enough rights to perform this operation.
  | NotEnoughBalance ("required" :! Natural, "present" :! Natural)
    -- ^ Insufficient balance.
  | NotEnoughAllowance ("required" :! Natural, "present" :! Natural)
    -- ^ Insufficient allowance to transfer foreign funds.
  | OperationsArePaused
    -- ^ Operation is unavailable until resume by token admin.
  | NotInTransferOwnershipMode
    -- ^ For the `acceptOwnership` entry point, if the contract's `newOwner`
    -- field is None
  | SenderIsNotNewOwner
    -- ^ For the `acceptOwnership` entry point, if the sender is not the
    -- address in the `newOwner` field
  | SenderIsNotOperator
    -- ^ For the burn/mint/pause entry point, if the sender is not the
    -- address in the `newOwner` field
  deriving stock (Eq, Generic)

deriveCustomError ''Error

type Storage = Storage' StorageFields

-- | Create a default storage with ability to set some balances to
-- non-zero values.
mkStorage :: Address -> Address -> Map Address Natural -> Set Address -> Storage
mkStorage adminAddress redeem balances operators = mkStorage' balances $
  StorageFields
  { admin = adminAddress
  , paused = False
  , totalSupply = sum balances
  , totalBurned = 0
  , totalMinted = sum balances
  , newOwner = Nothing
  , operators = operators
  , migrateFrom = Nothing
  , migrateTo = Nothing
  , redeemAddress = redeem
  }
