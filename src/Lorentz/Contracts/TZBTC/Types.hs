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
  , V1Parameters(..)
  , V1DeployParameters (..)
  , Parameter(..)
  , SafeParameter(..)
  , PauseParams
  , SafeView
  , SetRedeemAddressParams
  , Storage(..)
  , StorageFields(..)
  , StoreTemplateWithCommons(..)
  , StoreTemplate(..)
  , StoreTemplateV1
  , TZBTCv1
  , TZBTCVersionC
  , TransferOwnershipParams
  , TZBTCPartInstr
  , TZBTCParameter
  , TZBTCStorage
  , OneShotUpgradeParameters
  , Upgradeable.makeOneShotUpgradeParameters
  , UStoreV1
  ) where

import Fmt (Buildable(..), (+|), (|+))

import Lorentz
import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import Lorentz.Contracts.Metadata
import Lorentz.Contracts.Upgradeable.Common
  (KnownContractVersion(..), MigrationScript, MigrationScriptFrom, OneShotUpgradeParameters,
  PermanentImpl, SomeUContractRouter, UContractRouter(..), VerParam, VerUStore, Version,
  VersionKind)
import qualified Lorentz.Contracts.Upgradeable.Common as Upgradeable
import Lorentz.Contracts.Upgradeable.EntryPointWise
import Lorentz.EntryPoints (EpdDelegate, EpdRecursive, ParameterHasEntryPoints(..))
import Util.Instances ()

type BurnParams = ("value" :! Natural)
type OperatorParams = ("operator" :! Address)
type GetBalanceParams = ("owner" :! Address)
type SetRedeemAddressParams = ("redeem" :! Address)
type PauseParams = Bool
type TransferOwnershipParams = ("newOwner" :! Address)
type AcceptOwnershipParams = ()

deriving instance Eq (UContractRouter ver)

deriving instance Eq (MigrationScript from to)

deriving instance Eq (PermanentImpl ver)

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------
-- | This is a type that is supposed to wrap all the 'safe' entrypoints ie the
-- ones that have arguments that does not contain things like raw contract
-- values that are forbidden in operations that would end up in the chain.
data SafeParameter (ver :: VersionKind)
  = Run (VerParam ver)
  | Upgrade (OneShotUpgradeParameters ver)

  -- Entrypoint-wise upgrades are currently not protected from version mismatch
  -- in subsequent transactions, so the user ought to be careful with them.
  -- This behavior may change in future if deemed desirable.
  | EpwBeginUpgrade ("current" :! Version, "new" :! Version)
  | EpwApplyMigration ("migrationscript" :! MigrationScriptFrom (VerUStoreTemplate ver))
  | EpwSetCode ("contractcode" :! SomeUContractRouter)
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
  deriving anyclass (IsoValue)

instance HasTypeAnn (VerPermanent ver) => HasTypeAnn (SafeParameter ver)

instance ( Typeable ver
         , Typeable (VerInterface ver), Typeable (VerUStoreTemplate ver)
         , Typeable (VerPermanent ver)
         , KnownValue (VerPermanent ver), TypeHasDoc (VerPermanent ver)
         ) => TypeHasDoc (SafeParameter ver) where
  typeDocName _ = "Parameter.SafeParameter"
  typeDocMdDescription = "Parameter which does not have unsafe arguments, like raw `Contract p` values."
  typeDocMdReference tp =
    customTypeDocMdReference ("Parameter.SafeParameter", DType tp) []
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  typeDocDependencies = genericTypeDocDependencies

-- | The actual parameter of the main TZBTC contract.
data Parameter (ver :: VersionKind)
  = GetVersion (View () Version)
  -- TZBTC Entrypoints
  | GetAllowance        !(View ManagedLedger.GetAllowanceParams Natural)
  | GetBalance          !(View GetBalanceParams Natural)
  | GetTotalSupply      !(View () Natural)
  | GetTotalMinted      !(View () Natural)
  | GetTotalBurned      !(View () Natural)
  | GetOwner            !(View () Address)
  | GetRedeemAddress    !(View () Address)
  | GetTokenMetadata    !(View [TokenId] [TokenMetadata])
  | SafeEntrypoints !(SafeParameter ver)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (IsoValue)

instance HasTypeAnn (VerPermanent ver) => HasTypeAnn (Parameter ver)

-- We need the following two instances for the `SafeEntrypoint` entrypoint
-- to show up with an entrypoint annotation. This is required for the multisig
-- call to work, because the multisig contract calls the `SafeEntrypoint`
-- named entrypoint in this contract.
instance (VerPermanent ver ~ Empty) => ParameterHasEntryPoints (Parameter ver) where
  type ParameterEntryPointsDerivation (Parameter ver) = EpdDelegate

instance (VerPermanent ver ~ Empty) => ParameterHasEntryPoints (SafeParameter ver) where
  type ParameterEntryPointsDerivation (SafeParameter ver) = EpdRecursive

instance Buildable (Parameter ver) where
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
    GetOwner _ ->
      "Get owner"
    GetRedeemAddress _ ->
      "Get redeem address"
    GetTokenMetadata _ ->
      "Get token metadata"
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
data StorageFields (ver :: VersionKind) = StorageFields
  { contractRouter :: UContractRouter ver
  , currentVersion :: Version
  , migrating :: Bool
  } deriving stock (Generic, Show)
    deriving anyclass IsoValue

-- | The concrete storage of the contract
data Storage (ver :: VersionKind) = Storage
  { dataMap :: VerUStore ver
  , fields :: StorageFields ver
  } deriving stock (Generic, Show)
    deriving anyclass IsoValue

type NiceDocVersion ver =
  (Typeable ver, Typeable (VerUStoreTemplate ver), Typeable (VerInterface ver))

instance NiceDocVersion ver => TypeHasDoc (StorageFields ver) where
  typeDocName _ = "StorageFields"
  typeDocMdDescription =
    "StorageFields of upgradeable contract.\n\
    \This type keeps general information about upgradeable \
    \contract and the logic responsible for calling entrypoints \
    \implementations kept in UStore."
  typeDocMdReference tp =
    customTypeDocMdReference ("StorageFields", DType tp) []
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

instance NiceDocVersion ver => TypeHasDoc (Storage ver) where
  typeDocName _ = "Storage"
  typeDocMdDescription =
    "Type which defines storage of the upgradeable contract.\n\
    \It contains UStore with data related to actual contract logic \
    \and fields which relate to upgradeability logic."
  typeDocMdReference tp =
    customTypeDocMdReference ("Storage", DType tp) []
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

-- | Template wrapper which keeps contract owner.
-- It is common for all contract versions.
data StoreTemplateWithCommons s = StoreTemplateWithCommons
  { owner :: UStoreField Address
  , stCustom :: s
  } deriving stock Generic

-- | Template for the wrapped UStore which will hold the upgradeable code
-- and fields
data StoreTemplate = StoreTemplate
  { paused        :: UStoreField Bool
  , totalSupply   :: UStoreField Natural
  , totalBurned   :: UStoreField Natural
  , totalMinted   :: UStoreField Natural
  , newOwner      :: UStoreField (Maybe Address)
  , operators     :: UStoreField Operators
  , redeemAddress :: UStoreField Address
  , tokenMetadata :: UStoreField TokenMetadata
  , code          :: MText |~> EntryPointImpl StoreTemplateV1
  , fallback      :: UStoreField $ EpwFallback StoreTemplateV1
  , ledger        :: Address |~> ManagedLedger.LedgerValue
  } deriving stock Generic

type StoreTemplateV1 = StoreTemplateWithCommons StoreTemplate

type UStoreV1 = UStore StoreTemplateV1

type Operators = Set Address

instance Buildable Operators where
  build = mconcat . intersperse ", " . Prelude.map build . toList

-- | The data required to upgrade contract storage from V0 to V1.
data V1Parameters = V1Parameters
  { v1RedeemAddress :: !Address
  , v1TokenMetadata :: !TokenMetadata
  , v1Balances :: !(Map Address Natural)
  }

-- | The data required to initialize the V1 version of the contract storage
-- from scratch.
data V1DeployParameters = V1DeployParameters
  { v1Owner :: Address
  , v1MigrationParams :: V1Parameters
  }

-- | A safe view to act as argument to the inner stored procedures that
-- accept a callback contract. The main entrypoint will recieve a `View`
-- in its arguments, and convert the view to a `SafeView` before calling
-- the stored view entry point
newtype SafeView i o = SafeView { unSafeView :: (i, Address) }
   deriving stock (Generic, Show)
   deriving anyclass IsoValue

instance Wrapped (SafeView i o)

-- | Interface of the V1 contract.
type Interface =
  [ "callGetAllowance" ?: (SafeView ManagedLedger.GetAllowanceParams Natural)
  , "callGetBalance" ?: (SafeView GetBalanceParams Natural)
  , "callGetTotalSupply" ?: (SafeView () Natural)
  , "callGetTotalMinted" ?: (SafeView () Natural)
  , "callGetTotalBurned" ?: (SafeView () Natural)
  , "callGetOwner" ?: (SafeView () Address)
  , "callGetRedeemAddress" ?: (SafeView () Address)
  , "callGetTokenMetadata" ?: (SafeView [TokenId] [TokenMetadata])
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

data TZBTCv1 :: VersionKind
instance KnownContractVersion TZBTCv1 where
  type VerInterface TZBTCv1 = Interface
  type VerUStoreTemplate TZBTCv1 = StoreTemplateV1
  contractVersion _ = 1

-- | Type of instruction which implements a part of TZBTC Protocol.
type TZBTCPartInstr param store =
  '[param, UStore store] :->
  '[[Operation], UStore store]

type TZBTCStorage = Storage TZBTCv1
type TZBTCParameter = Parameter TZBTCv1

-- | Common for all TZBTC versions.
type TZBTCVersionC ver = (VerPermanent ver ~ Empty, Typeable ver)

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

-- | For `add/removeOperator`, `setRedeemAddress`, `pause/unpause` entrypoints,
-- if the sender is not the owner of the contract.
type instance ErrorArg "senderIsNotOwner" = ()

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

instance CustomErrorHasDoc "senderIsNotOwner" where
  customErrClass = ErrClassBadArgument
  customErrDocMdCause =
    "Sender has to be an owner to call this entrypoint"
