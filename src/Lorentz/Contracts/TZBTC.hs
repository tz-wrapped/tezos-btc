{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC
  ( mkStorage
  , mkPackedEntrypoint
  , SaneParameter(..)
  , Parameter(..)
  , StoreEntrypointParameter
  , Storage
  , Storage'(..)
  , StorageFields(..)
  , agentContract
  , storedEntrypointsHandler
  , toParameter
  , tzbtcContract
  , tzbtcCompileWay
  , tzbtcDoc
  ) where

import Prelude hiding ((>>))

import qualified Data.Text as T (concat)

import Michelson.Typed.Doc
import Lorentz
import Lorentz.Contracts.TZBTC.Agent (agentContract)
import Lorentz.Contracts.TZBTC.Impl
import Lorentz.Contracts.TZBTC.Types
import Lorentz.Contracts.ManagedLedger.Doc (getTotalSupplyDoc)
import Util.Named ((.!))

{-# ANN module ("HLint: ignore Reduce duplication" :: LText) #-}

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

tzbtcContract :: Contract Parameter Storage
tzbtcContract = do
  unpair
  entryCase @Parameter (Proxy @TzbtcEntryPointKind)
    ( #cMint /-> mint
    , #cBurn /-> burn
    , #cAddOperator /-> addOperator
    , #cPause /-> pause
    , #cUnpause /-> unpause
    , #cStartMigrateTo /-> startMigrateTo
    , #cStartMigrateFrom /-> startMigrateFrom
    , #cMintForMigration /-> mintForMigration
    , #cMigrate /-> migrate
    , #cStoreEntrypoint /-> storeEntryPoint
    , #cStoredEntrypoints /-> fetchFromStorageAndExec
    )

storedEntrypointsHandler :: Entrypoint StoredEntrypointsParam Storage
storedEntrypointsHandler = caseT @StoredEntrypointsParam
  ( #cStoredTransfer /-> transfer
  , #cStoredTransferViaProxy /-> transferViaProxy
  , #cStoredApprove /-> approve
  , #cStoredApproveViaProxy /-> approveViaProxy
  , #cStoredGetAllowance /-> getAllowance
  , #cStoredGetBalance /-> getBalance
  , #cStoredGetTotalSupply /-> getTotal #totalSupply
    getTotalSupplyDoc
  , #cStoredGetTotalMinted /-> getTotal #totalMinted
    "Return total number of minted tokens"
  , #cStoredGetTotalBurned /-> getTotal #totalBurned
    "Return total number of burned tokens"
  , #cStoredSetAdministrator /-> setAdministrator
  , #cStoredGetAdministrator /-> getAdministrator
  , #cStoredTransferOwnership /-> transferOwnership
  , #cStoredAcceptOwnership /-> acceptOwnership
  , #cStoredSetProxy /-> setProxy
  , #cStoredRemoveOperator /-> removeOperator
  , #cStoredSetRedeemAddress /-> setRedeemAddress
  )

tzbtcCompileWay :: LorentzCompilationWay Parameter Storage
tzbtcCompileWay = lcwEntryPoints

-- | Create a default storage with ability to set some balances to
-- non-zero values.
mkStorage :: Address -> Address -> Map Address Natural -> Set Address -> Storage
mkStorage adminAddress redeem balances operators = Storage'
  { dataMap = mkUStore $ StorageTemplate
      { ledger = UStoreSubMap $ toLedgerValue <$> balances
      , packedHandler =
          UStoreField $ Right $ mkPackedEntrypoint storedEntrypointsHandler
      }
  , fields = StorageFields
      { admin = adminAddress
      , paused = False
      , totalSupply = sum $ balances
      , totalBurned = 0
      , totalMinted = sum balances
      , newOwner = Nothing
      , operators = operators
      , redeemAddress = redeem
      , code = [mt|ZBTC|]
      , tokenname = [mt|TZBTC|]
      , migrationManagerOut = Nothing
      , migrationManagerIn = Nothing
      , proxy = Left adminAddress
      }
  }
  where
    toLedgerValue initBal = (#balance .! initBal, #approvals .! mempty)

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
