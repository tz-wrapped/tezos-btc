{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.Proxy
  ( mkStorage
  , tzbtcProxyContract
  , MLProxy.Parameter
  , MLProxy.SaneParameter(..)
  , MLProxy.fromSaneParameter
  )
where

import Lorentz
import qualified Lorentz.Contracts.TZBTC.Types as TZBTC
import qualified Lorentz.Contracts.ManagedLedger.Proxy as MLProxy

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

type Storage = ContractAddr TZBTC.Parameter

tzbtcProxyContract :: Contract MLProxy.Parameter Storage
tzbtcProxyContract = do
  unpair
  caseT @MLProxy.Parameter
    ( #cTransfer /-> do
        sender; toNamed #sender
        pair
        wrap_ #cTransferViaProxy
        callTzbtc
    , #cParameter0 /-> caseT
        ( #cApprove /-> do
            sender; toNamed #sender
            pair
            wrap_ #cApproveViaProxy
            callTzbtc
        , #cParameter1 /-> caseT
          ( #cGetAllowance /-> wrap_ #cGetAllowance >> callTzbtc
          , #cParameter2 /-> caseT
            ( #cGetBalance /-> wrap_ #cGetBalance >> callTzbtc
            , #cGetTotalSupply /-> wrap_ #cGetTotalSupply >> callTzbtc
            )
          )
        )
    )

callTzbtc :: '[ TZBTC.Parameter, Storage ] :-> ContractOut Storage
callTzbtc = do
  dip $ dup >> amount
  transferTokens; nil; swap; cons
  pair

mkStorage ::  Address -> Storage
mkStorage = ContractAddr
