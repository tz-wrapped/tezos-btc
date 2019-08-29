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
  , MigrateFromParams
  , Storage
  , StorageFields(..)
  , TransferOwnershipParams
  , mkStorage
  ) where

import Fmt (Buildable(..))
import Data.Set (Set)

import Lorentz
import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import Lorentz.Contracts.ManagedLedger.Types (Storage'(..), mkStorage')
import Lorentz.Contracts.UserUpgradeable.Migrations (MigrationScript)
import Util.Instances ()

type BurnParams = ("value" :! Natural)
type OperatorParams = ("operator" :! Address)
type GetBalanceParams = Address
type SetRedeemAddressParams = ("redeem" :! Address)
type PauseParams = Bool
type TransferOwnershipParams = ("newOwner" :! Address)
type StartMigrateToParams = ("startMigrateTo" :! MigrationScript)
type StartMigrateFromParams = ("startMigrateFrom" :! Address)
type MigrateFromParams = ("migrateMintTo" :! Address, "value" :! Natural)
type AcceptOwnershipParams = ()
type MigrateParams = ()

instance Buildable MigrationScript where
  build = undefined

data StorageFields = StorageFields
  { admin       :: Address
  , paused      :: Bool
  , totalSupply :: Natural
  , totalBurned :: Natural
  , totalMinted :: Natural
  , newOwner    :: Maybe Address
  , operators   :: Set Address
  , previousVersion :: Maybe Address
  , redeemAddress :: Address
  , migrationScript :: Maybe MigrationScript
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
    -- ^ Insufficient allowance to transfer funds.
  | OperationsArePaused
    -- ^ Operation is unavailable until resume by token admin.
  | NotInTransferOwnershipMode
    -- ^ For the `acceptOwnership` entry point, if the contract's `newOwner`
    -- field is None.
  | SenderIsNotNewOwner
    -- ^ For the `acceptOwnership` entry point, if the sender is not the
    -- address in the `newOwner` field.
  | SenderIsNotOperator
    -- ^ For the burn/mint/pause entry point, if the sender is not one
    -- of the operators.
  | UnauthorizedMigrateFrom
    -- ^ For migration calls if the contract does not have previous
    -- version field set.
  deriving stock (Eq, Generic)

deriveCustomError ''Error

type Storage = Storage' StorageFields

-- | Create a default storage with ability to set some balances to
-- non-zero values.
mkStorage :: Address -> Address -> Map Address Natural -> Set Address -> Maybe Address -> Storage
mkStorage adminAddress redeem balances operators prev = mkStorage' balances $
  StorageFields
  { admin = adminAddress
  , paused = False
  , totalSupply = sum balances
  , totalBurned = 0
  , totalMinted = sum balances
  , newOwner = Nothing
  , operators = operators
  , previousVersion = prev
  , redeemAddress = redeem
  , migrationScript = Nothing
  }
