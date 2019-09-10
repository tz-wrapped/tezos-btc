{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC
  ( mkStorage
  , mkMigrationScriptFor
  , Parameter(..)
  , Storage
  , StorageFields(..)
  , agentContract
  , tzbtcContract
  , tzbtcCompileWay
  ) where


import Lorentz

import Lorentz.Contracts.TZBTC.Agent (agentContract)
import Lorentz.Contracts.TZBTC.Impl
import Lorentz.Contracts.TZBTC.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

tzbtcContract :: Contract Parameter Storage
tzbtcContract = do
  unpair
  caseT @Parameter
    ( #cTransfer /-> transfer
    , #cTransferViaProxy /-> transferViaProxy
    , #cApprove /-> approve
    , #cApproveViaProxy /-> approveViaProxy
    , #cGetAllowance /-> getAllowance
    , #cGetBalance /-> getBalance
    , #cGetTotalSupply /-> getTotal #totalSupply
    , #cGetTotalMinted /-> getTotal #totalMinted
    , #cGetTotalBurned /-> getTotal #totalBurned
    , #cSetAdministrator /-> setAdministrator
    , #cGetAdministrator /-> getAdministrator
    , #cMint /-> mint
    , #cBurn /-> burn
    , #cAddOperator /-> addOperator
    , #cRemoveOperator /-> removeOperator
    , #cSetRedeemAddress /-> setRedeemAddress
    , #cPause /-> pause
    , #cUnpause /-> unpause
    , #cTransferOwnership /-> transferOwnership
    , #cAcceptOwnership /-> acceptOwnership
    , #cStartMigrateTo /-> startMigrateTo
    , #cStartMigrateFrom /-> startMigrateFrom
    , #cMintForMigration /-> mintForMigration
    , #cMigrate /-> migrate
    , #cSetProxy /-> setProxy
    )

tzbtcCompileWay :: LorentzCompilationWay Parameter Storage
tzbtcCompileWay = lcwEntryPoints
