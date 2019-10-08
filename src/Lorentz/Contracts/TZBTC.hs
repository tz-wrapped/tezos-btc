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

import qualified Data.Text as T
import Fmt (build, fmt)

import Michelson.Typed (untypeValue)
import Michelson.Typed.Doc
  (DComment(..), DDescription(..), DName(..), SomeDocItem(..), contractDocToMarkdown)
import Lorentz.Contracts.ManagedLedger.Doc (getTotalSupplyDoc)
import Lorentz.Test.Integrational (genesisAddress)
import Util.Markdown

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
    ( #cEntrypointsWithView /->
        entrypointsWithView
    , #cEntrypointsWithoutView /->
        entrypointsWithoutView
    )

tzbtcCompileWay :: LorentzCompilationWay Parameter Storage
tzbtcCompileWay = lcwEntryPoints

entrypointsWithView :: Entrypoint ParameterWithView Storage
entrypointsWithView =
  entryCase @ParameterWithView (Proxy @TzbtcEntryPointWithViewKind)
    ( #cGetAllowance /-> getAllowance
    , #cGetBalance /-> getBalance
    , #cGetTotalSupply /-> getTotal #totalSupply
      getTotalSupplyDoc
    , #cGetTotalMinted /-> getTotal #totalMinted
      "Return total number of minted tokens"
    , #cGetTotalBurned /-> getTotal #totalBurned
      "Return total number of burned tokens"
    , #cGetAdministrator /-> getAdministrator
    )

entrypointsWithoutView :: Entrypoint ParameterWithoutView Storage
entrypointsWithoutView =
  entryCase @ParameterWithoutView (Proxy @TzbtcEntryPointWithoutViewKind)
    ( #cTransfer /-> transfer
    , #cTransferViaProxy /-> transferViaProxy
    , #cApprove /-> approve
    , #cApproveViaProxy /-> approveViaProxy
    , #cSetAdministrator /-> setAdministrator
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

data TzbtcEntryPointWithoutViewKind

instance DocItem (DEntryPoint TzbtcEntryPointWithoutViewKind) where
  type DocItemPosition (DEntryPoint TzbtcEntryPointWithoutViewKind) = 1001
  docItemSectionName = Just "Non-View entry-points of TZBTC contract"
  docItemSectionDescription = Nothing
  docItemToMarkdown = diEntryPointToMarkdown

pbsContainedInEntrypointsWithoutView :: ParamBuildingStep
pbsContainedInEntrypointsWithoutView = ParamBuildingStep
  { pbsEnglish = "Wrap into " <> mdTicked "EntrypointsWithoutView" <> " constructor"
  , pbsHaskell = \p -> "EntrypointsWithoutView (" <> p <> ")"
  , pbsMichelson = \p ->
      let paramDumb = Pause ()
      in build $ T.replace
         (fmt . build . untypeValue $ toVal paramDumb)
         ("(" <> fmt p <> ")")
         (fmt . build . untypeValue $ toVal $ EntrypointsWithoutView paramDumb)
  }

pbsContainedInEntrypointsWithView :: ParamBuildingStep
pbsContainedInEntrypointsWithView = ParamBuildingStep
  { pbsEnglish = "Wrap into " <> mdTicked "EntrypointsWithView" <> " constructor"
  , pbsHaskell = \p -> "EntrypointsWithView (" <> p <> ")"
  , pbsMichelson = \p ->
      let paramDumb = GetTotalSupply $
            View {viewParam = (), viewCallbackTo = ContractAddr genesisAddress}
      in build $ T.replace
         (fmt . build . untypeValue $ toVal paramDumb)
         ("(" <> fmt p <> ")")
         (fmt . build . untypeValue $ toVal $ EntrypointsWithView paramDumb)
  }

data TzbtcEntryPointWithViewKind

instance DocItem (DEntryPoint TzbtcEntryPointWithViewKind) where
  type DocItemPosition (DEntryPoint TzbtcEntryPointWithViewKind) = 937
  docItemSectionName = Just "View entry-points of TZBTC contract"
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
    clarifyParamBuildingSteps pbsContainedInEntrypointsWithoutView $
      entrypointsWithoutView
    fakeCoerce
    clarifyParamBuildingSteps pbsContainedInEntrypointsWithView $
      entrypointsWithView
