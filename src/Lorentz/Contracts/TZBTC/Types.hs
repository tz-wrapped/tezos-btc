{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lorentz.Contracts.TZBTC.Types
  ( AcceptOwnershipParams
  , BurnParams
  , Entrypoint
  , GetBalanceParams
  , Interface
  , ManagedLedger.AllowanceParams
  , ManagedLedger.ApproveParams
  , ManagedLedger.GetAllowanceParams
  , ManagedLedger.LedgerValue
  , ManagedLedger.MintParams
  , ManagedLedger.TransferParams
  , OperatorParams
  , OriginationParameters(..)
  , Parameter(..)
  , SafeParameter(..)
  , PauseParams
  , SafeView
  , SetRedeemAddressParams
  , Storage(..)
  , StorageFields(..)
  , StoreTemplate(..)
  , TransferOwnershipParams
  , TZBTCPartInstr
  , TZBTCParameter
  , TZBTCStorage
  , UpgradeParameters
  , UStoreV1
  ) where

import Fmt ((+|), (|+), Buildable(..))

import Lorentz
import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import Lorentz.EntryPoints (ParameterEntryPoints(..), pepRecursive)
import Lorentz.Contracts.Upgradeable.Common (UContractRouter(..), MigrationScript)
import Lorentz.Contracts.Upgradeable.EntryPointWise
import Util.Instances ()

type BurnParams = ("value" :! Natural)
type OperatorParams = ("operator" :! Address)
type GetBalanceParams = ("owner" :! Address)
type SetRedeemAddressParams = ("redeem" :! Address)
type PauseParams = Bool
type TransferOwnershipParams = ("newOwner" :! Address)
type AcceptOwnershipParams = ()
type Entrypoint param store
  = '[ param, store ] :-> ContractOut store

type UpgradeParameters interface =
  ( "newVersion" :! Natural
  , "migrationScript" :! MigrationScript
  , "newCode" :! UContractRouter interface
  )

deriving instance Eq (UContractRouter interface)
deriving instance Show (UContractRouter interface)

deriving instance Eq MigrationScript

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------
-- | This is a type that is supposed to wrap all the 'safe' entrypoints ie the
-- ones that have arguments that does not contain things like raw contract
-- values that are forbidden in operations that would end up in the chain.
data SafeParameter (interface :: [EntryPointKind])
  = Run (UParam interface)
  | Upgrade (UpgradeParameters interface)

  -- Entrypoint-wise upgrades are currently not protected from version mismatch
  -- in subsequent transactions, so the user ought to be careful with them.
  -- This behavior may change in future if deemed desirable.
  | EpwBeginUpgrade Natural  -- version
  | EpwApplyMigration ("migrationscript" :! MigrationScript)
  | EpwSetCode ("contractcode" :! (UContractRouter interface))
  | EpwFinishUpgrade
  -- TZBTC Entrypoints
  | Transfer            !ManagedLedger.TransferParams
  | Approve             !ManagedLedger.ApproveParams
  | Mint                !ManagedLedger.MintParams
  | Burn                !BurnParams
  | AddOperator         !OperatorParams
  | RemoveOperator      !OperatorParams
  | SetRedeemAddress    !SetRedeemAddressParams
  | Pause               !()
  | Unpause             !()
  | TransferOwnership   !TransferOwnershipParams
  | AcceptOwnership     !AcceptOwnershipParams
  deriving stock (Eq, Generic, Show)
  deriving anyclass IsoValue

instance (Typeable interface) => TypeHasDoc (SafeParameter interface) where
  typeDocName _ = "Parameter.SafeParameter"
  typeDocMdDescription = "Parameter which does not have unsafe arguments, for example raw `Contract p` values."
  typeDocMdReference tp =
    customTypeDocMdReference ("Parameter.SafeParameter", DType tp) []
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  typeDocDependencies = genericTypeDocDependencies

-- | The actual parameter of the main TZBTC contract.
data Parameter (interface :: [EntryPointKind])
  = GetVersion (View () Natural)
  -- TZBTC Entrypoints
  | GetAllowance        !(View ManagedLedger.GetAllowanceParams Natural)
  | GetBalance          !(View GetBalanceParams Natural)
  | GetTotalSupply      !(View () Natural)
  | GetTotalMinted      !(View () Natural)
  | GetTotalBurned      !(View () Natural)
  | GetAdministrator    !(View () Address)
  | SafeEntrypoints (SafeParameter interface)
  deriving stock (Eq, Generic, Show)
  deriving anyclass IsoValue

instance ParameterEntryPoints (Parameter s) where
  parameterEntryPoints = pepRecursive

instance Buildable (Parameter s) where
  build = \case
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
    GetVersion _ ->
      "Get contract version"
    SafeEntrypoints param -> case param of
      Run _ ->
        "Run a stored entrypoint"
      Upgrade _ ->
        "Upgrade contract"
      EpwBeginUpgrade _ ->
        "Start contract upgrade process"
      EpwApplyMigration _ ->
        "Run a complete migration"
      EpwFinishUpgrade ->
        "Finish upgrade process"
      EpwSetCode _ ->
        "Set entrypoint dispatch code"
      Transfer (arg #from -> from, arg #to -> to, arg #value -> value) ->
        "Transfer from " +| from |+ " to " +| to |+ ", value = " +| value |+ ""
      Approve (arg #spender -> spender, arg #value -> value) ->
        "Approve for " +| spender |+ ", value = " +| value |+ ""
      Mint (arg #to -> to, arg #value -> value) ->
        "Mint to " +| to |+ ", value = " +| value |+ ""
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


---------------------------------------------------------------------------
-- Storage
---------------------------------------------------------------------------

-- | The concrete fields of the contract
data StorageFields interface = StorageFields
  { contractRouter  :: UContractRouter interface
  , currentVersion :: Natural
  , migrating :: Bool
  } deriving stock (Generic, Show)
    deriving anyclass IsoValue

-- | The concrete storage of the contract
data Storage interface store = Storage
  { dataMap :: UStore store
  , fields :: StorageFields interface
  } deriving stock (Generic, Show)
    deriving anyclass IsoValue

-- | Template for the wrapped UStore which will hold the upgradeable code
-- and fields
data StoreTemplate = StoreTemplate
  { admin         :: UStoreField Address
  , paused        :: UStoreField Bool
  , totalSupply   :: UStoreField Natural
  , totalBurned   :: UStoreField Natural
  , totalMinted   :: UStoreField Natural
  , newOwner      :: UStoreField (Maybe Address)
  , operators     :: UStoreField (Set Address)
  , redeemAddress :: UStoreField Address
  , tokenname     :: UStoreField MText
  , tokencode     :: UStoreField MText
  , code          :: MText |~> EntryPointImpl StoreTemplate
  , fallback      :: UStoreField $ EpwFallback StoreTemplate
  , ledger        :: Address |~> ManagedLedger.LedgerValue
  } deriving stock Generic

type UStoreV1 = UStore StoreTemplate

-- The data required to initialize the V1 version of the contract storage.
data OriginationParameters = OriginationParameters
  { opMaster :: !Address
  , opRedeemAddress :: !Address
  , opBalances :: !(Map Address Natural)
  , opTokenname :: !MText
  , opTokencode :: !MText
  }

-- | A safe view to act as argument to the inner stored procedures that
-- accept a callback contract. The main entrypoint will recieve a `View`
-- in its arguments, and convert the view to a `SafeView` before calling
-- the stored view entry point
newtype SafeView i o = SafeView { unSafeView :: (i, Address) }
   deriving stock (Generic, Show)
   deriving anyclass IsoValue

-- | Interface of the V1 contract.
type Interface =
  [ "callGetAllowance" ?: (SafeView ManagedLedger.GetAllowanceParams Natural)
  , "callGetBalance" ?: (SafeView GetBalanceParams Natural)
  , "callGetTotalSupply" ?: (SafeView () Natural)
  , "callGetTotalMinted" ?: (SafeView () Natural)
  , "callGetTotalBurned" ?: (SafeView () Natural)
  , "callGetAdministrator" ?: (SafeView () Address)
  , "callTransfer" ?: ManagedLedger.TransferParams
  , "callApprove" ?: ManagedLedger.ApproveParams
  , "callMint" ?: ManagedLedger.MintParams
  , "callBurn" ?: BurnParams
  , "callAddOperator" ?: OperatorParams
  , "callRemoveOperator" ?: OperatorParams
  , "callSetRedeemAddress" ?: SetRedeemAddressParams
  , "callPause" ?: ()
  , "callUnpause" ?: ()
  , "callTransferOwnership" ?: TransferOwnershipParams
  , "callAcceptOwnership" ?: AcceptOwnershipParams
  ]

-- | Type of instruction which implements a part of TZBTC Protocol.
type TZBTCPartInstr param store =
  '[param, UStore store] :->
  '[[Operation], UStore store]

type TZBTCStorage = Storage Interface StoreTemplate
type TZBTCParameter = Parameter Interface

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------
--
-- | For addresses that fails contract typecheck
type instance ErrorArg "unexpectedContractType" = ()

-- | For the `acceptOwnership` entry point, if the contract's `newOwner`
-- field is None.
type instance ErrorArg "notInTransferOwnershipMode" = ()

-- | For the `acceptOwnership` entry point, if the sender is not the
-- address in the `newOwner` field.
type instance ErrorArg "senderIsNotNewOwner" = ()

-- | For the burn/mint/pause entry point, if the sender is not one
-- of the operators.
type instance ErrorArg "senderIsNotOperator" = ()

-- | For calls that can only be run during a migration
type instance ErrorArg "upgContractIsNotMigrating" = ()

-- | If contract is in a migrating state and the call
-- requires it to be in a non migrating state.
type instance ErrorArg "upgContractIsMigrating" = ()

instance CustomErrorHasDoc "upgContractIsMigrating" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "An operation was requested when contract is in a state of migration"

instance CustomErrorHasDoc "upgContractIsNotMigrating" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "An migration related operation was requested when contract is not in a state of migration"

instance CustomErrorHasDoc "unexpectedContractType" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Typechecking contract at the given address failed"

instance CustomErrorHasDoc "notInTransferOwnershipMode" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Cannot accept ownership before transfer process has been initiated \
    \by calling transferOwnership entrypoint"

instance CustomErrorHasDoc "senderIsNotNewOwner" where
  customErrClass = ErrClassBadArgument
  customErrDocMdCause =
    "Cannot accept ownership because the sender address is different from \
    \the address passed to the transferOwnership entrypoint previously"

instance CustomErrorHasDoc "senderIsNotOperator" where
  customErrClass = ErrClassBadArgument
  customErrDocMdCause =
    "Sender has to be an operator to call this entrypoint"
