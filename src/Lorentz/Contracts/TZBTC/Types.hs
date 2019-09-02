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
  , MigrationManager
  , OperatorParams
  , Parameter(..)
  , PauseParams
  , SetRedeemAddressParams
  , SetMigrationAgentParams
  , StartMigrateFromParams
  , StartMigrateToParams
  , MintForMigrationParams
  , Storage
  , StorageFields(..)
  , TransferOwnershipParams
  , mkStorage
  ) where

import Fmt (Buildable(..), (+|), (|+))
import Data.Set (Set)

import Lorentz
import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import Lorentz.Contracts.ManagedLedger.Types (Storage'(..), mkStorage')
import Util.Instances ()

type MigrationManager = ContractAddr (Address, Natural)
type BurnParams = ("value" :! Natural)
type OperatorParams = ("operator" :! Address)
type GetBalanceParams = Address
type SetRedeemAddressParams = ("redeem" :! Address)
type PauseParams = Bool
type TransferOwnershipParams = ("newOwner" :! Address)
type StartMigrateToParams = ("migrationManager" :! MigrationManager)
type StartMigrateFromParams = ("migrationManager" :! MigrationManager)
type MintForMigrationParams = ("to" :! Address, "value" :! Natural)
type AcceptOwnershipParams = ()
type MigrateParams = ()
type SetMigrationAgentParams = ("migrationAgent" :! MigrationManager)

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

data Parameter
  = Transfer            !ManagedLedger.TransferParams
  | Approve             !ManagedLedger.ApproveParams
  | GetAllowance        !(View ManagedLedger.GetAllowanceParams Natural)
  | GetBalance          !(View Address Natural)
  | GetTotalSupply      !(View () Natural)
  | GetTotalMinted      !(View () Natural)
  | GetTotalBurned      !(View () Natural)
  | SetAdministrator    !Address
  | GetAdministrator    !(View () Address)
  | Mint                !ManagedLedger.MintParams
  | Burn                !BurnParams
  | AddOperator         !OperatorParams
  | RemoveOperator      !OperatorParams
  | SetRedeemAddress    !SetRedeemAddressParams
  | Pause               !()
  | Unpause             !()
  | TransferOwnership   !TransferOwnershipParams
  | AcceptOwnership     !AcceptOwnershipParams
  | StartMigrateTo      !StartMigrateToParams
  | StartMigrateFrom    !StartMigrateFromParams
  | MintForMigration    !MintForMigrationParams
  | Migrate             !MigrateParams
  deriving stock Generic
  deriving anyclass IsoValue

----------------------------------------------------------------------------
-- Storage
----------------------------------------------------------------------------

data StorageFields = StorageFields
  { admin       :: Address
  , paused      :: Bool
  , totalSupply :: Natural
  , totalBurned :: Natural
  , totalMinted :: Natural
  , newOwner    :: Maybe Address
  , operators   :: Set Address
  , redeemAddress :: Address
  , code :: MText
  , tokenname :: MText
  , migrationManagerIn :: Maybe MigrationManager
  , migrationManagerOut :: Maybe MigrationManager
  } deriving stock Generic
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
  | NoBalanceToMigrate
    -- ^ For migration calls if there is nothing to migrate.
  | MigrationNotEnabled
    -- ^ For migrate calls to contracts don't have migration manager set.
  | SenderIsNotAgent
    -- ^ For `mintForMigration` calls from address other than that of the
    -- migration agent.
  deriving stock (Eq, Generic)

instance Buildable Parameter where
  build = \case
    Transfer (arg #from -> from, arg #to -> to, arg #value -> value) ->
      "Transfer from " +| from |+ " to " +| to |+ ", value = " +| value |+ ""
    Approve (arg #spender -> spender, arg #value -> value) ->
      "Approve for " +| spender |+ ", value = " +| value |+ ""
    GetAllowance (View (arg #owner -> owner, arg #spender -> spender) _) ->
      "Get allowance for " +| owner |+ " from " +| spender |+ ""
    GetBalance (View addr _) ->
      "Get balance for " +| addr |+ ""
    GetTotalSupply _ ->
      "Get total supply"
    GetTotalMinted _ ->
      "Get total minted"
    GetTotalBurned _ ->
      "Get total burned"
    SetAdministrator addr ->
      "Set administrator to " +| addr |+ ""
    GetAdministrator _ ->
      "Get administrator"
    Mint (arg #to -> to, arg #value -> value) ->
      "Mint to " +| to |+ ", value = " +| value |+ ""
    MintForMigration (arg #to -> to, arg #value -> value) ->
      "MintForMigration to " +| to |+ ", value = " +| value |+ ""
    Burn (arg #value -> value) ->
      "Burn, value = " +| value |+ ""
    AddOperator (arg #operator -> operator) ->
      "Add operator " +| operator |+ ""
    RemoveOperator (arg #operator -> operator) ->
      "Remove operator " +| operator |+ ""
    SetRedeemAddress (arg #redeem -> redeem) ->
      "Set redeem address to " +| redeem |+ ""
    Pause _ ->
      "Pause"
    Unpause _ ->
      "Unpause"
    TransferOwnership (arg #newOwner -> newOwner) ->
      "Transfer ownership to " +| newOwner |+ ""
    AcceptOwnership _ ->
      "Accept ownership"
    StartMigrateTo (arg #migrationManager -> migrationMangerAddress) ->
      "Start migrate to " +| migrationMangerAddress |+ ""
    StartMigrateFrom (arg #migrationManager -> migrationAgent) ->
      "Start migrate from " +| migrationAgent |+ ""
    Migrate _ ->
      "Migrate"

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
  , redeemAddress = redeem
  , code = [mt|ZBTC|]
  , tokenname = [mt|TZBTC|]
  , migrationManagerOut = Nothing
  , migrationManagerIn = Nothing
  }
