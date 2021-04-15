{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO: Replace 'Empty' with 'Never' from morley
{-# OPTIONS_GHC -Wno-deprecations #-}

module Lorentz.Contracts.TZBTC.Common.Types
  ( AcceptOwnershipParams
  , BurnParams
  , Entrypoint
  , GetBalanceParams
  , ManagedLedger.AllowanceParams
  , ManagedLedger.ApproveParams
  , ManagedLedger.GetAllowanceParams
  , ManagedLedger.MintParams
  , ManagedLedger.TransferParams
  , OperatorParams
  , Parameter(..)
  , SafeParameter(..)
  , PauseParams
  , SafeView
  , SetRedeemAddressParams
  , Storage(..)
  , StorageFields(..)
  , StoreTemplateWithCommons(..)
  , Operators
  , TZBTCVersionC
  , TransferOwnershipParams
  , SomeTZBTCVersion
  , TZBTCPartInstr
  , OneShotUpgradeParameters
  , Upgradeable.makeOneShotUpgradeParameters
  ) where

import Fmt (Buildable(..), (+|), (|+))

import Lorentz
import Lorentz.Contracts.Metadata
import qualified Lorentz.Contracts.Spec.AbstractLedgerInterface as ManagedLedger
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as ManagedLedger
import qualified Lorentz.Contracts.Spec.ManagedLedgerInterface as ManagedLedger
import Lorentz.Contracts.Upgradeable.Common
  (KnownContractVersion(..), MigrationScriptFrom, OneShotUpgradeParameters, PermanentImpl,
  SomeUContractRouter, UContractRouter(..), VerParam, VerUStore, Version, VersionKind)
import qualified Lorentz.Contracts.Upgradeable.Common as Upgradeable
import Lorentz.UStore
import Lorentz.UStore.Migration (SomeUTemplate)
import Util.Instances ()

type BurnParams = ("value" :! Natural)
type OperatorParams = ("operator" :! Address)
type GetBalanceParams = ("owner" :! Address)
type SetRedeemAddressParams = ("redeem" :! Address)
type PauseParams = Bool
type TransferOwnershipParams = ("newOwner" :! Address)
type AcceptOwnershipParams = ()

deriving stock instance Eq (UContractRouter ver)

deriving stock instance Eq (MigrationScript from to)

deriving stock instance Eq (PermanentImpl ver)

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

deriving anyclass instance IsoValue (VerPermanent ver) => IsoValue (SafeParameter ver)

instance HasAnnotation (VerPermanent ver) => HasAnnotation (SafeParameter ver)

instance ( Typeable ver
         , Typeable (VerInterface ver), Typeable (VerUStoreTemplate ver)
         , Typeable (VerPermanent ver)
         , KnownValue (VerPermanent ver), TypeHasDoc (VerPermanent ver)
         , UStoreTemplateHasDoc (VerUStoreTemplate ver)
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

deriving anyclass instance IsoValue (VerPermanent ver) => IsoValue (Parameter ver)

instance HasAnnotation (VerPermanent ver) => HasAnnotation (Parameter ver)

-- We need the following two instances for the `SafeEntrypoint` entrypoint
-- to show up with an entrypoint annotation. This is required for the multisig
-- call to work, because the multisig contract calls the `SafeEntrypoint`
-- named entrypoint in this contract.
instance (Typeable ver, VerPermanent ver ~ Empty) => ParameterHasEntrypoints (Parameter ver) where
  type ParameterEntrypointsDerivation (Parameter ver) = EpdDelegate

instance (VerPermanent ver ~ Empty) => ParameterHasEntrypoints (SafeParameter ver) where
  type ParameterEntrypointsDerivation (SafeParameter ver) = EpdRecursive

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
    deriving anyclass (IsoValue, HasAnnotation)

-- | The concrete storage of the contract
data Storage (ver :: VersionKind) = Storage
  { dataMap :: VerUStore ver
  , fields :: StorageFields ver
  } deriving stock (Generic, Show)
    deriving anyclass (IsoValue, HasAnnotation)

type NiceDocVersion ver =
  ( Typeable ver, Typeable (VerUStoreTemplate ver), Typeable (VerInterface ver)
  , UStoreTemplateHasDoc (VerUStoreTemplate ver)
  )

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

instance CanCastTo a b =>
         StoreTemplateWithCommons a `CanCastTo` StoreTemplateWithCommons b where
  castDummy = castDummyG

type Operators = Set Address

instance Buildable Operators where
  build = mconcat . intersperse ", " . Prelude.map build . toList

---------------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------------

-- Use this when don't need particular version but still want to point out
-- that version is TZBTC-specific and thus have empty permanent part.
data SomeTZBTCVersion :: VersionKind
instance KnownContractVersion SomeTZBTCVersion where
  type VerInterface SomeTZBTCVersion = SomeInterface
  type VerUStoreTemplate SomeTZBTCVersion = SomeUTemplate
  contractVersion _ = 999

-- | A safe view to act as argument to the inner stored procedures that
-- accept a callback contract. The main entrypoint will recieve a `View`
-- in its arguments, and convert the view to a `SafeView` before calling
-- the stored view entry point
newtype SafeView i o = SafeView { unSafeView :: (i, Address) }
   deriving stock (Generic, Show)
   deriving anyclass IsoValue

instance Wrappable (SafeView i o)

-- | Type of instruction which implements a part of TZBTC Protocol.
type TZBTCPartInstr param store =
  '[param, UStore store] :->
  '[[Operation], UStore store]

-- | Common for all TZBTC versions.
type TZBTCVersionC ver = (VerPermanent ver ~ Empty, Typeable ver)
