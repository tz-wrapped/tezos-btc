{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lorentz.Contracts.TZBTC.Types
  ( AcceptOwnershipParams
  , ApproveViaProxyParams
  , BurnParams
  , SaneParameter(..)
  , StoredEntrypointsParam(..)
  , StoredHandlerField
  , Entrypoint
  , GetBalanceParams
  , Handler
  , ManagedLedger.AllowanceParams
  , ManagedLedger.ApproveParams
  , ManagedLedger.GetAllowanceParams
  , ManagedLedger.LedgerValue
  , ManagedLedger.MintParams
  , ManagedLedger.TransferParams
  , MigrateParams
  , MigrationManager
  , MintForMigrationParams
  , OperatorParams
  , Parameter(..)
  , PauseParams
  , SetMigrationAgentParams
  , SetProxyParams
  , SetRedeemAddressParams
  , StartMigrateFromParams
  , StartMigrateToParams
  , Storage'(..)
  , Storage
  , StorageFields(..)
  , StorageTemplate(..)
  , StoreEntrypointParameter
  , ToUnpackEnv(..)
  , TransferOwnershipParams
  , TransferViaProxyParams
  , mkEnv
  , toParameter
  ) where

import Fmt (Buildable(..), (+|), (|+))
import Data.Set (Set)
import Data.Singletons
import qualified Data.Map as Map

import Lorentz
import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import Lorentz.Contracts.ManagedLedger.Types ()
import Util.Instances ()
import Michelson.TypeCheck.TypeCheck (TcOriginatedContracts)
import Michelson.Typed.Extract
import Michelson.Typed.Haskell.Doc
import Michelson.Typed.Sing (Sing(..), fromSingT)
import Tezos.Address

type MigrationManager = ContractAddr (Address, Natural)
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
type StoreEntrypointParameter = ("entrypointCode" :! ByteString)

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

data Parameter
  = Mint                !ManagedLedger.MintParams
  | Burn                !BurnParams
  | AddOperator         !OperatorParams
  | Pause               !()
  | Unpause             !()
  | StartMigrateTo      !StartMigrateToParams
  | StartMigrateFrom    !StartMigrateFromParams
  | MintForMigration    !MintForMigrationParams
  | Migrate             !MigrateParams
  | StoreEntrypoint     !StoreEntrypointParameter
  | StoredEntrypoints   !StoredEntrypointsParam
  deriving stock (Eq, Show, Generic)
  deriving anyclass IsoValue

data StoredEntrypointsParam
  = StoredTransfer           !ManagedLedger.TransferParams
  | StoredTransferViaProxy   !TransferViaProxyParams
  | StoredApprove            !ManagedLedger.ApproveParams
  | StoredApproveViaProxy    !ApproveViaProxyParams
  | StoredGetAllowance       !(View ManagedLedger.GetAllowanceParams Natural)
  | StoredGetBalance         !(View Address Natural)
  | StoredGetTotalSupply     !(View () Natural)
  | StoredGetTotalMinted     !(View () Natural)
  | StoredGetTotalBurned     !(View () Natural)
  | StoredSetAdministrator   !Address
  | StoredGetAdministrator   !(View () Address)
  | StoredTransferOwnership  !TransferOwnershipParams
  | StoredAcceptOwnership    !AcceptOwnershipParams
  | StoredSetProxy           !SetProxyParams
  | StoredRemoveOperator     !OperatorParams
  | StoredSetRedeemAddress   !SetRedeemAddressParams
  deriving stock (Eq, Show, Generic)
  deriving anyclass IsoValue

data SaneParameter
  = Transfer           !ManagedLedger.TransferParams
  | TransferViaProxy   !TransferViaProxyParams
  | Approve            !ManagedLedger.ApproveParams
  | ApproveViaProxy    !ApproveViaProxyParams
  | GetAllowance       !(View ManagedLedger.GetAllowanceParams Natural)
  | GetBalance         !(View Address Natural)
  | GetTotalSupply     !(View () Natural)
  | GetTotalMinted     !(View () Natural)
  | GetTotalBurned     !(View () Natural)
  | SetAdministrator   !Address
  | GetAdministrator   !(View () Address)
  | TransferOwnership  !TransferOwnershipParams
  | AcceptOwnership    !AcceptOwnershipParams
  | SetProxy           !SetProxyParams
  | RemoveOperator     !OperatorParams
  | SetRedeemAddress   !SetRedeemAddressParams
  deriving stock (Eq, Show, Generic)
  deriving anyclass IsoValue

instance TypeHasDoc StoredEntrypointsParam where
  typeDocMdDescription =
    homomorphicTypeDocMdReference
      (Proxy @StoredEntrypointsParam) (WithinParens False)

toParameter :: SaneParameter -> Parameter
toParameter bp = case bp of
  Transfer a -> StoredEntrypoints $ StoredTransfer a
  TransferViaProxy a -> StoredEntrypoints $ StoredTransferViaProxy a
  Approve a -> StoredEntrypoints $ StoredApprove a
  ApproveViaProxy a -> StoredEntrypoints $ StoredApproveViaProxy a
  GetAllowance a -> StoredEntrypoints $ StoredGetAllowance a
  GetBalance a -> StoredEntrypoints $ StoredGetBalance a
  GetTotalSupply a -> StoredEntrypoints $ StoredGetTotalSupply a
  GetTotalMinted a -> StoredEntrypoints $ StoredGetTotalMinted a
  GetTotalBurned a -> StoredEntrypoints $ StoredGetTotalBurned a
  SetAdministrator a -> StoredEntrypoints $ StoredSetAdministrator a
  GetAdministrator a -> StoredEntrypoints $ StoredGetAdministrator a
  TransferOwnership a -> StoredEntrypoints $ StoredTransferOwnership a
  AcceptOwnership a -> StoredEntrypoints $ StoredAcceptOwnership a
  SetProxy a -> StoredEntrypoints $ StoredSetProxy a
  RemoveOperator a -> StoredEntrypoints $ StoredRemoveOperator a
  SetRedeemAddress a -> StoredEntrypoints $ StoredSetRedeemAddress a

----------------------------------------------------------------------------
-- Storage
----------------------------------------------------------------------------

type Entrypoint param store
  = '[ param, store ] :-> ContractOut store

type Handler a store = Entrypoint a store

data StorageFields = StorageFields
  { admin       :: Address
  , paused      :: Bool
  , totalSupply :: Natural
  , totalBurned :: Natural
  , totalMinted :: Natural
  , newOwner    :: (Maybe Address)
  , operators   :: (Set Address)
  , redeemAddress :: Address
  , code :: MText
  , tokenname :: MText
  , migrationManagerIn :: (Maybe MigrationManager)
  , migrationManagerOut :: (Maybe MigrationManager)
  , proxy :: (Either Address Address)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass IsoValue

type StoredHandlerField = Either Address ByteString

data StorageTemplate = StorageTemplate
  { ledger :: Address |~> ManagedLedger.LedgerValue
  , packedHandler :: UStoreField StoredHandlerField
  } deriving stock Generic

data Storage' a = Storage'
  { dataMap :: UStore StorageTemplate
  , fields :: a
  } deriving stock (Eq, Show, Generic)
    deriving anyclass IsoValue

type Storage = Storage' StorageFields

instance
  HasFieldOfType StorageFields name field =>
  StoreHasField StorageFields name field where
  storeFieldOps = storeFieldOpsADT

instance {-# OVERLAPPABLE #-}
  (StoreHasField fields fname ftype, IsoValue fields) =>
  StoreHasField (Storage' fields) fname ftype where
  storeFieldOps = storeFieldOpsTopLevelStorage #fields

instance
  IsoValue fields =>
  StoreHasField (Storage' fields) "packedHandler" StoredHandlerField where
  storeFieldOps = storeFieldOpsTopLevelStorage #dataMap

instance
  IsoValue fields =>
  StoreHasSubmap (Storage' fields) "ledger" Address ManagedLedger.LedgerValue where
  storeSubmapOps = storeSubmapOpsTopLevelStorage #dataMap

instance Buildable Parameter where
  build = \case
    StoredEntrypoints sp ->
      case sp of
        StoredTransfer (arg #from -> from, arg #to -> to, arg #value -> value) ->
          "Transfer from " +| from |+ " to " +| to |+ ", value = " +| value |+ ""
        StoredTransferViaProxy
          (arg #sender -> sender_, (arg #from -> from, arg #to -> to, arg #value -> value)) ->
          "Transfer via proxy from sender " +| sender_ |+ ", from" +| from |+ " to "
          +| to |+ ", value = " +| value |+ ""
        StoredApprove (arg #spender -> spender, arg #value -> value) ->
          "Approve for " +| spender |+ ", value = " +| value |+ ""
        StoredApproveViaProxy (arg #sender -> sender_, (arg #spender -> spender, arg #value -> value)) ->
          "Approve via proxy for sender "
            +| sender_ |+ ", spender =" +| spender |+ ", value = " +| value |+ ""
        StoredGetAllowance (View (arg #owner -> owner, arg #spender -> spender) _) ->
          "Get allowance for " +| owner |+ " from " +| spender |+ ""
        StoredGetBalance (View addr _) ->
          "Get balance for " +| addr |+ ""
        StoredGetTotalSupply _ ->
          "Get total supply"
        StoredGetTotalMinted _ ->
          "Get total minted"
        StoredGetTotalBurned _ ->
          "Get total burned"
        StoredSetAdministrator addr ->
          "Set administrator to " +| addr |+ ""
        StoredGetAdministrator _ ->
          "Get administrator"
        StoredRemoveOperator (arg #operator -> operator) ->
          "Remove operator " +| operator |+ ""
        StoredSetRedeemAddress (arg #redeem -> redeem) ->
          "Set redeem address to " +| redeem |+ ""
        StoredTransferOwnership (arg #newOwner -> newOwner) ->
          "Transfer ownership to " +| newOwner |+ ""
        StoredAcceptOwnership _ ->
          "Accept ownership"
        StoredSetProxy address_ ->
          "Set proxy " +| address_ |+ ""
    StoreEntrypoint (arg #entrypointCode -> _)  ->
      "Store entrypoint"
    Mint (arg #to -> to, arg #value -> value) ->
      "Mint to " +| to |+ ", value = " +| value |+ ""
    MintForMigration (arg #to -> to, arg #value -> value) ->
      "MintForMigration to " +| to |+ ", value = " +| value |+ ""
    Burn (arg #value -> value) ->
      "Burn, value = " +| value |+ ""
    AddOperator (arg #operator -> operator) ->
      "Add operator " +| operator |+ ""
    Pause _ ->
      "Pause"
    Unpause _ ->
      "Unpause"
    StartMigrateTo (arg #migrationManager -> migrateTo) ->
      "Start migrate to " +| migrateTo |+ ""
    StartMigrateFrom (arg #migrationManager -> migrateFrom) ->
      "Start migrate from " +| migrateFrom |+ ""
    Migrate _ ->
      "Migrate"

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

-- | For dynamically loaded entrypoints
type instance ErrorArg "entrypointCodeNotFound" = ()

-- | For dynamically loaded entrypoints
type instance ErrorArg "entrypointUnpackError" = ()

-- | For setProxy entry point if Proxy is set already
type instance ErrorArg "proxyAlreadySet" = ()

-- | For storeEntrypoint handler if it is set already
type instance ErrorArg "packedHandlerExists" = ()

-- | For storeEntrypoint handler the sender is bad
type instance ErrorArg "notAllowedToSetHandler" = ()

instance CustomErrorHasDoc "notAllowedToSetHandler" where
  customErrDocMdCause =
    "Sender is not allowed to set the stored entry point\
     \because the address is different from what is in the Left\
     \value in packedHandler field."

instance CustomErrorHasDoc "packedHandlerExists" where
  customErrDocMdCause = "Cannot overwrite existing handler"

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

instance CustomErrorHasDoc "entrypointCodeNotFound" where
  customErrDocMdCause =
    "Requested stored entry point was not found"

instance CustomErrorHasDoc "entrypointUnpackError" where
  customErrDocMdCause =
    "There was an error in unpacking a stored entrypoint"

class ToUnpackEnv a where
  toUnpackEnv :: ContractAddr a -> a -> TcOriginatedContracts

instance ToUnpackEnv Parameter where
  toUnpackEnv = mkEnv

mkEnv :: ContractAddr Parameter -> Parameter -> TcOriginatedContracts
mkEnv caddr param = case param of
  StoredEntrypoints sp ->
    case sp of
      StoredGetAllowance v -> addToEnv v
      StoredGetBalance v -> addToEnv v
      StoredGetTotalSupply v -> addToEnv v
      StoredGetTotalMinted v -> addToEnv v
      StoredGetTotalBurned v -> addToEnv v
      StoredGetAdministrator v -> addToEnv v
      _ -> defEnv
  _ -> defEnv
  where
    contractAddrToHash x = case unContractAddress x of
      ContractAddress hsh -> hsh
      _ -> error "mkEnv : Unexpected non-contract address"
    addToEnv :: forall a r. (SingI (ToT r)) => View a r -> TcOriginatedContracts
    addToEnv (View _ ca) =
      Map.insert
        (contractAddrToHash ca)
        (toUType $ fromSingT (sing :: Sing (ToT r))) defEnv
    defEnv = Map.singleton
      (contractAddrToHash caddr)
      (toUType $ fromSingT (sing :: Sing (ToT Parameter)))
