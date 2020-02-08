{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}

module Lorentz.Contracts.Multisig
  ( Counter(..)
  , ErrorsKind(..)
  , ErrorHandler
  , GenericMultisigAction(..)
  , Keys(..)
  , MSigParameter
  , MSigParamMain
  , MSigPayload
  , MSigStorage
  , NiceMultisigParam
  , Payload
  , Threshold(..)
  , tzbtcMultisigContract
  , mkStorage
  ) where

import Lorentz

import Lorentz.Contracts.Multisig.Specialized as SM
import qualified Lorentz.Contracts.TZBTC as TZBTC
import Lorentz.Contracts.TZBTC hiding (Parameter, Storage)
import Lorentz.Contracts.Multisig.Generic (ErrorHandler, ErrorsKind(..), Keys(..))

type Version = SomeTZBTCVersion

type MSigParameter = Parameter (SafeParameter Version) (TZBTC.Parameter Version)
type MSigParamMain = MainParams (SafeParameter Version) (TZBTC.Parameter Version)
type MSigPayload = SM.Payload (SafeParameter Version) (TZBTC.Parameter Version)
type MSigStorage = SM.Storage

tzbtcMultisigContract
  :: forall e.  (ErrorHandler e, Typeable e)
  => Contract MSigParameter Storage
tzbtcMultisigContract =
  SM.specializedMultisigContract
    @_
    @_
    @e
    (Call @"SafeEntrypoints")
