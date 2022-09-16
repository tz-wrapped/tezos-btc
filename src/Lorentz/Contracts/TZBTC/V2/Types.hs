{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}
module Lorentz.Contracts.TZBTC.V2.Types
  ( module Lorentz.Contracts.TZBTC.Common.Types
  , Interface
  , LedgerValue
  , V2Parameters
  , V2ParametersFromV1(..)
  , V2DeployParameters (..)
  , Parameter(..)
  , SafeParameter(..)
  , Storage(..)
  , StorageFields(..)
  , StoreTemplateWithCommons(..)
  , StoreTemplate(..)
  , StoreTemplateV2
  , TZBTCv2
  , TZBTCParameter
  , TZBTCStorage
  , UStoreV2
  ) where

import Lorentz
import Lorentz.Contracts.ManagedLedger (LedgerValue)
import Lorentz.Contracts.Metadata
import Lorentz.Contracts.TZBTC.Common.Types
import Lorentz.Contracts.TZBTC.V1.Types qualified as V1
import Lorentz.Contracts.Upgradeable.Common (KnownContractVersion(..), VersionKind)
import Lorentz.Contracts.Upgradeable.EntrypointWise
import Lorentz.UStore
import Lorentz.UStore.Instances
import Morley.Util.Instances ()
import Morley.Util.Markdown (md)

-- | Template for the wrapped UStore which will hold the upgradeable code
-- and fields.
--
-- This differs from 'V1.StoreTemplate' in that storage over it has different
-- @StoreHasSubmap@ instances - we want to work with @ledger@ submap
-- as if it was splitted into two separate @ledger@ and @approvals@ submaps.
data StoreTemplate = StoreTemplate
  { paused        :: UStoreField Bool
  , totalSupply   :: UStoreField Natural
  , totalBurned   :: UStoreField Natural
  , totalMinted   :: UStoreField Natural
  , newOwner      :: UStoreField (Maybe Address)
  , operators     :: UStoreField Operators
  , redeemAddress :: UStoreField Address
  , tokenMetadata :: UStoreField TokenMetadata
  , code          :: MText |~> EntrypointImpl StoreTemplateV2
  , fallback      :: UStoreField $ EpwFallback StoreTemplateV2
  , ledger        :: Address |~> V1.LedgerValue
  } deriving stock Generic

instance CanCastTo V1.StoreTemplate StoreTemplate where
  castDummy = castDummyG

type StoreTemplateV2 = StoreTemplateWithCommons StoreTemplate

type UStoreV2 = UStore StoreTemplateV2

instance {-# OVERLAPPING #-}
         StoreHasSubmap UStoreV2 "ledger" Address LedgerValue where
  storeSubmapOps =
    mapStoreSubmapOpsValue (namedIso #balance) $
    zoomStoreSubmapOps #ledger nonDefIso nonDefIso
      ustoreSubmapOps
      (storeFieldOpsReferTo #balance storeFieldOpsADT)

instance {-# OVERLAPPING #-}
         StoreHasSubmap UStoreV2 "approvals" GetAllowanceParams Natural where
  storeSubmapOps =
    sequenceStoreSubmapOps #approvals nonDefIso
      ( mapStoreSubmapOpsKey (fromNamed #owner) $
          zoomStoreSubmapOps #ledger nonDefIso nonDefIso ustoreSubmapOps storeFieldOpsADT
      )
      ( mapStoreSubmapOpsKey (fromNamed #spender) (storeSubmapOpsReferTo this storeSubmapOps) )

-- | The data required to upgrade contract storage from V1 to V2.
data V2ParametersFromV1 = V2ParametersFromV1

-- | The data required to upgrade contract storage from V0 to V2.
type V2Parameters = V1.V1Parameters

-- | The data required to initialize the V2 version of the contract storage
-- from scratch.
data V2DeployParameters = V2DeployParameters
  { v2Owner :: Address
  , v2MigrationParams :: V2Parameters
  }

-- | Interface of the V2 contract.
type Interface = V1.Interface

data TZBTCv2 :: VersionKind
instance KnownContractVersion TZBTCv2 where
  type VerInterface TZBTCv2 = Interface
  type VerUStoreTemplate TZBTCv2 = StoreTemplateV2
  type VerPermanent _ = Empty -- legacy compatibility
  contractVersion _ = 2


type TZBTCStorage = Storage TZBTCv2
type TZBTCParameter = Parameter TZBTCv2

instance UStoreTemplateHasDoc StoreTemplateV2 where
  ustoreTemplateDocName = "Store template V2"
  ustoreTemplateDocDescription = [md|
    Contains all the storage entries for V2 contract.
    |]
