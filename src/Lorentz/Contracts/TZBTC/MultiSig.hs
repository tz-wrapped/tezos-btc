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

import Data.Vinyl.Derived (Label)

import Lorentz

import Michelson.Text (mkMTextUnsafe)
import Michelson.Typed.Haskell.Instr.Sum

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
  :: forall parameterRaw parameter constructorName.
     ( ParamConstraints parameterRaw, ParamConstraints parameter
     , InstrWrapC parameter constructorName
     , CtorHasOnlyField constructorName parameter parameterRaw
     )
  => Address -> parameterRaw -> Label constructorName -> Lambda () [Operation]
contractToLambda addr paramRaw constrName = do
  drop
  push addr
  contract
  if IsNone
  then do push (mkMTextUnsafe "Invalid contract type"); failWith
  else do
    push paramRaw
    stackType @(parameterRaw : ContractAddr parameter : '[])
    wrap_ @parameter @constructorName constrName
    dip $ push $ toMutez 0
    transferTokens @parameter
    dip nil
    cons

mkStorage :: Natural -> Natural -> [PublicKey] -> Storage
mkStorage counter threshold keys_ = (counter, (threshold, keys_))
