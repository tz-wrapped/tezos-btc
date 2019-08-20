{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC
  ( mkStorage
  , Parameter(..)
  , Storage
  , tzbtcContract
  ) where

import Lorentz

import Lorentz.Contracts.TZBTC.Impl
import Lorentz.Contracts.TZBTC.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

data Parameter
  = Transfer            !TransferParams
  | Approve             !ApproveParams
  | GetAllowance        !(View GetAllowanceParams Natural)
  | GetBalance          !(View Address Natural)
  | GetTotalSupply      !(View () Natural)
  | SetPause            !Bool
  | SetAdministrator    !Address
  | GetAdministrator    !(View () Address)
  | Mint                !MintParams
  | Burn                !BurnParams
  | AddOperator         !OperatorParams
  | RemoveOperator      !OperatorParams
  | SetRedeemAddress    !SetRedeemAddressParams
  | Pause               !PauseParams
  | TransferOwnership   !TransferOwnershipParams
  | AcceptOwnership     !AcceptOwnershipParams
  | StartMigrateTo      !StartMigrateToParams
  | StartMigrateFrom    !StartMigrateFromParams
  | Migrate             !MigrateParams
  deriving stock Generic
  deriving anyclass IsoValue

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------


tzbtcContract :: Contract Parameter Storage
tzbtcContract = do
  unpair
  caseT @Parameter
    ( #cTransfer /-> transfer
    , #cApprove /-> approve
    , #cGetAllowance /-> getAllowance
    , #cGetBalance /-> getBalance
    , #cGetTotalSupply /-> getTotalSupply
    , #cSetPause /-> setPause
    , #cSetAdministrator /-> setAdministrator
    , #cGetAdministrator /-> getAdministrator
    , #cMint /-> mint
    , #cBurn /-> burn
    , #cAddOperator /-> addOperator
    , #cRemoveOperator /-> removeOperator
    , #cSetRedeemAddress /-> setRedeemAddress
    , #cPause /-> setPause
    , #cTransferOwnership /-> transferOwnership
    , #cAcceptOwnership /-> acceptOwnership
    , #cStartMigrateTo /-> startMigrateTo
    , #cStartMigrateFrom /-> startMigrateFrom
    , #cMigrate /-> migrate
    )
