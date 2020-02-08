{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.MultiSig
  ( ParamAction(..)
  , ParamPayload
  , Parameter(..)
  , ParamMain
  , Storage
  , mkStorage
  , mkSpParamMain
  )
where

import Prelude hiding (drop, toStrict, (>>))

import Lorentz
import Lorentz.Value

import Michelson.Text (mkMTextUnsafe)
import Lorentz.Contracts.Upgradeable.Common (VersionKind)

import Lorentz.Contracts.Upgradeable.Common (KnownContractVersion(..))
import Lorentz.Contracts.TZBTC as TZBTC hiding (Parameter, Storage)
import qualified Lorentz.Contracts.TZBTC.Types as TZBTC (Parameter(..))
import qualified Lorentz.Contracts.Multisig.Specialized.Type as SpMSig

data Parameter (v :: VersionKind)
  = Default ()
  | Main (ParamMain v)
  deriving stock Generic
  deriving anyclass IsoValue

type Counter = Natural
type Threshold = Natural

type Storage
  = (Counter, (Threshold, [PublicKey]))

type ParamMain (v :: VersionKind)
  = (ParamPayload v, ParamSignatures)

type ParamPayload (v :: VersionKind)
  = (Counter, ParamAction v)

data ParamAction (v :: VersionKind)
  = ParamAction (SafeParameter v, TAddress (TZBTC.Parameter v))
  | ParamManage ParamManage
  deriving stock Generic
  deriving anyclass IsoValue

type ParamManage
  = (Natural, [PublicKey])

type ParamSignatures = [Maybe Signature]

mkStorage :: Natural -> Natural -> [PublicKey] -> Storage
mkStorage counter threshold keys_ = (counter, (threshold, keys_))

mkSpParamMain :: forall v. (Typeable v, VerPermanent v ~ Empty) =>  ParamMain v -> SpMSig.MainParams (TZBTC.SafeParameter v) (TZBTC.Parameter v)
mkSpParamMain ((counter, pa), sigs) = ((counter, convertAction pa), sigs)
  where
    convertAction :: ParamAction v -> SpMSig.GenericMultisigAction (TZBTC.SafeParameter v) (TZBTC.Parameter v)
    convertAction = \case
      ParamManage pm -> SpMSig.ChangeKeys pm
      ParamAction (sp, ta) -> SpMSig.Operation (sp, ta)
