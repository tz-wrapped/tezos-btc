{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lorentz.Contracts.TZBTC.Types
  ( AcceptOwnershipParams
  , ApproveViaProxyParams
  , BurnParams
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
  , MigrationManagerCType
  , MintForMigrationParams
  , OperatorParams
  , Parameter(..)
  , ParameterWithView(..)
  , ParameterWithoutView(..)
  , PauseParams
  , SetMigrationAgentParams
  , SetProxyParams
  , SetRedeemAddressParams
  , StartMigrateFromParams
  , StartMigrateToParams
  , Storage
  , StorageFields(..)
  , TransferOwnershipParams
  , TransferViaProxyParams
  , mkStorage
  ) where

import Fmt (Buildable(..), (+|), (|+))
import Data.Set (Set)

import Lorentz
import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import Lorentz.Contracts.ManagedLedger.Types (Storage'(..), mkStorage')
import Util.Instances ()

type MigrationManager = Address
type MigrationManagerCType = ContractAddr (Address, Natural)
type BurnParams = ("value" :! Natural)
type OperatorParams = ("operator" :! Address)
type TransferViaProxyParams = ("sender" :! Address, ManagedLedger.TransferParams)
type ApproveViaProxyParams = ("sender" :! Address, ManagedLedger.ApproveParams)
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
type SetProxyParams = Address


----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

data Parameter
  = EntrypointsWithView ParameterWithView
  | EntrypointsWithoutView ParameterWithoutView
  deriving stock (Eq, Show, Generic)
  deriving anyclass IsoValue

data ParameterWithView
  = GetAllowance        !(View ManagedLedger.GetAllowanceParams Natural)
  | GetBalance          !(View Address Natural)
  | GetTotalSupply      !(View () Natural)
  | GetTotalMinted      !(View () Natural)
  | GetTotalBurned      !(View () Natural)
  | GetAdministrator    !(View () Address)
  deriving stock (Eq, Show, Generic)
  deriving anyclass IsoValue

data ParameterWithoutView
  = Transfer            !ManagedLedger.TransferParams
  | TransferViaProxy    !TransferViaProxyParams
  | Approve             !ManagedLedger.ApproveParams
  | ApproveViaProxy     !ApproveViaProxyParams
  | SetAdministrator    !Address
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
  | SetProxy            !SetProxyParams
  deriving stock (Eq, Show, Generic)
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
  , proxy :: Either Address Address
  } deriving stock Generic
    deriving anyclass IsoValue

instance HasFieldOfType StorageFields name field =>
         StoreHasField StorageFields name field where
  storeFieldOps = storeFieldOpsADT

instance Buildable Parameter where
  build = \case
    EntrypointsWithView param -> case param of
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
      GetAdministrator _ ->
        "Get administrator"

    EntrypointsWithoutView param -> case param of
      Transfer (arg #from -> from, arg #to -> to, arg #value -> value) ->
        "Transfer from " +| from |+ " to " +| to |+ ", value = " +| value |+ ""
      TransferViaProxy
        (arg #sender -> sender_, (arg #from -> from, arg #to -> to, arg #value -> value)) ->
        "Transfer via proxy from sender " +| sender_ |+ ", from" +| from |+ " to "
        +| to |+ ", value = " +| value |+ ""
      Approve (arg #spender -> spender, arg #value -> value) ->
        "Approve for " +| spender |+ ", value = " +| value |+ ""
      ApproveViaProxy (arg #sender -> sender_, (arg #spender -> spender, arg #value -> value)) ->
        "Approve via proxy for sender "
        +| sender_ |+ ", spender =" +| spender |+ ", value = " +| value |+ ""
      SetAdministrator addr ->
        "Set administrator to " +| addr |+ ""
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
      StartMigrateTo (arg #migrationManager -> migrateTo) ->
        "Start migrate to " +| migrateTo |+ ""
      StartMigrateFrom (arg #migrationManager -> migrateFrom) ->
        "Start migrate from " +| migrateFrom |+ ""
      SetProxy address_ ->
        "Set proxy " +| address_ |+ ""
      Migrate _ ->
        "Migrate"

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
  , proxy = Left adminAddress
  }

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

-- | For the `acceptOwnership` entry point, if the contract's `newOwner`
-- field is None.
type instance ErrorArg "notInTransferOwnershipMode" = ()

-- | For the `acceptOwnership` entry point, if the sender is not the
-- address in the `newOwner` field.
type instance ErrorArg "senderIsNotNewOwner" = ()

-- | For the burn/mint/pause entry point, if the sender is not one
-- of the operators.
type instance ErrorArg "senderIsNotOperator" = ()

-- | For migration calls if the contract does not have previous
-- version field set.
type instance ErrorArg "unauthorizedMigrateFrom" = ()

-- | For migration calls if there is nothing to migrate.
type instance ErrorArg "noBalanceToMigrate" = ()

-- | For migrate calls to contracts don't have migration manager set.
type instance ErrorArg "migrationNotEnabled" = ()

-- | For `mintForMigration` calls from address other than that of the
-- migration agent.
type instance ErrorArg "senderIsNotAgent" = ()

-- | For `startMigrateTo` calls when the contract is in a running state
type instance ErrorArg "tokenOperationsAreNotPaused" = ()

-- | For FA1.2.1 compliance endpoints that are callable via a proxy
type instance ErrorArg "proxyIsNotSet" = ()

-- | For FA1.2.1 compliance endpoints that are callable via a proxy
type instance ErrorArg "callerIsNotProxy" = ()

-- | For setProxy entry point if Left value in `proxy` field does not
-- match the sender's address
type instance ErrorArg "notAllowedToSetProxy" = ()

-- | For setProxy entry point if Proxy is set already
type instance ErrorArg "proxyAlreadySet" = ()

-- | If migration manager was found to be ill-typed
type instance ErrorArg "illTypedMigrationManager" = ()

instance CustomErrorHasDoc "notInTransferOwnershipMode" where
  customErrDocMdCause =
    "Cannot accept ownership before transfer process has been initiated \
    \by calling transferOwnership entrypoint"

instance CustomErrorHasDoc "senderIsNotNewOwner" where
  customErrDocMdCause =
    "Cannot accept ownership because the sender address is different from \
    \the address passed to the transferOwnership entrypoint previously"

instance CustomErrorHasDoc "senderIsNotOperator" where
  customErrDocMdCause =
    "Sender has to be an operator to call this entrypoint"

instance CustomErrorHasDoc "unauthorizedMigrateFrom" where
  customErrDocMdCause =
    "Previous contract version address hasn't been set up"

instance CustomErrorHasDoc "noBalanceToMigrate" where
  customErrDocMdCause =
    "Cannot migrate zero tokens to the new contract version"

instance CustomErrorHasDoc "migrationNotEnabled" where
  customErrDocMdCause =
    "Cannot migrate when migration manager hasn't been set up"

instance CustomErrorHasDoc "senderIsNotAgent" where
  customErrDocMdCause =
    "Sender has to be a migration manager to call this entrypoint"

instance CustomErrorHasDoc "tokenOperationsAreNotPaused" where
  customErrDocMdCause =
    "This operation is only available when token operations are paused"

instance CustomErrorHasDoc "proxyIsNotSet" where
  customErrDocMdCause =
    "Cannot call proxy entrypoint because proxy address is not set"

instance CustomErrorHasDoc "callerIsNotProxy" where
  customErrDocMdCause =
    "Sender has to be the proxy to call proxy entrypoints"

instance CustomErrorHasDoc "notAllowedToSetProxy" where
  customErrDocMdCause =
    "Cannot set proxy address because Left value in `proxy` field does not \
    \match the sender's address"

instance CustomErrorHasDoc "proxyAlreadySet" where
  customErrDocMdCause =
    "Cannot set proxy address because it was already set up"

instance CustomErrorHasDoc "illTypedMigrationManager" where
  customErrDocMdCause =
    "Type checking on the stored migration manager address failed"
