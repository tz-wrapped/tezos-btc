{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lorentz.Contracts.TZBTC.V1.Types
  ( module Lorentz.Contracts.TZBTC.Common.Types
  , Interface
  , ManagedLedger.LedgerValue
  , V1Parameters(..)
  , V1DeployParameters (..)
  , Parameter(..)
  , SafeParameter(..)
  , Storage(..)
  , StorageFields(..)
  , StoreTemplateWithCommons(..)
  , StoreTemplate(..)
  , StoreTemplateV1
  , TZBTCv1
  , TZBTCParameter
  , TZBTCStorage
  , UStoreV1
  ) where

import Lorentz
import Lorentz.Contracts.Metadata
import Lorentz.Contracts.TZBTC.Common.Types
import qualified Lorentz.Contracts.TZBTC.V1.ManagedLedger as ManagedLedger
import Lorentz.Contracts.Upgradeable.Common (KnownContractVersion(..), VersionKind)
import Lorentz.Contracts.Upgradeable.EntrypointWise
import Util.Instances ()
import Util.Markdown (md)

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
  , code          :: MText |~> EntrypointImpl StoreTemplateV1
  , fallback      :: UStoreField $ EpwFallback StoreTemplateV1
  , ledger        :: Address |~> ManagedLedger.LedgerValue
  } deriving stock Generic

type StoreTemplateV1 = StoreTemplateWithCommons StoreTemplate

type UStoreV1 = UStore StoreTemplateV1


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


type TZBTCStorage = Storage TZBTCv1
type TZBTCParameter = Parameter TZBTCv1

instance UStoreTemplateHasDoc StoreTemplateV1 where
  ustoreTemplateDocName = "Store template V1"
  ustoreTemplateDocDescription = [md|
    Contains all the storage entries for V1 contract.
    |]

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
