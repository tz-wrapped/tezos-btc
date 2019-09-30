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
  , Storage
  , contractToLambda
  , mkStorage
  )
where

import Prelude hiding (drop, toStrict, (>>))

import Lorentz

data Parameter
  = Default ()
  | ParameterMain ParamMain
  deriving stock Generic
  deriving anyclass IsoValue

type Counter = Natural
type Threshold = Natural

type Storage
  = (Counter, (Threshold, [PublicKey]))

type ParamMain
  = (ParamPayload, ParamSignatures)

type ParamPayload
  = (Counter, ParamAction)

data ParamAction
  = ParamLambda (Lambda () [Operation])
  | ParamManage ParamManage
  deriving stock Generic
  deriving anyclass IsoValue

type ParamManage
  = (Natural, [PublicKey])

type ParamSignatures = [Maybe Signature]

type ParamConstraints parameter =
  ( KnownValue parameter
  , NoOperation parameter
  , IsoValue parameter
  , NoBigMap parameter)

contractToLambda
  :: forall parameter. ParamConstraints parameter
  => ContractAddr parameter -> parameter -> Lambda () [Operation]
contractToLambda caddr param = do
  drop
  push caddr
  push param
  dip $ push $ toMutez 0
  transferTokens
  dip nil
  cons

mkStorage :: Natural -> Natural -> [PublicKey] -> Storage
mkStorage counter threshold keys_ = (counter, (threshold, keys_))
