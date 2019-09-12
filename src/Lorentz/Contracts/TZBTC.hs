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
  , StorageFields(..)
  , agentContract
  , tzbtcContract
  , tzbtcCompileWay
  , tzbtcDoc
  ) where

import Prelude (LText)
import Lorentz

import qualified Data.Text as T (concat)

import Michelson.Typed.Doc
  (DComment(..), DDescription(..), DName(..), SomeDocItem(..), contractDocToMarkdown)

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
  entryCase @Parameter (Proxy @TzbtcEntryPointKind)
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

data TzbtcEntryPointKind

instance DocItem (DEntryPoint TzbtcEntryPointKind) where
  type DocItemPosition (DEntryPoint TzbtcEntryPointKind) = 1001
  docItemSectionName = Just "Entry-points of TZBTC contract"
  docItemSectionDescription = Nothing
  docItemToMarkdown = diEntryPointToMarkdown

tzbtcDoc :: LText
tzbtcDoc = contractDocToMarkdown . buildLorentzDoc $ do
  -- License info
  doc $ DComment $ T.concat
    [ "- SP"
    , "DX-FileCopyrightText: 2019 Bitcoin Suisse\n"
    , "-\n"
    , "- SP"
    , "DX-License-Identifier: LicenseRef-Proprietary"
    ]
  docGroup (SomeDocItem . DName "TZBTC") $ do
    doc $ DDescription "This contract is implemented using Lorentz language"
    tzbtcContract
