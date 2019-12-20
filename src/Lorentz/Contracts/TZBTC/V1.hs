{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.V1
  ( Interface
  , StoreTemplate(..)
  , migrationScriptsRaw
  , tzbtcContractRouterRaw
  )
where

import Prelude hiding (drop, (>>))

import qualified Data.Map as M

import Lorentz
import Lorentz.Contracts.Upgradeable.Common.Base
import Lorentz.Contracts.Upgradeable.EntryPointWise
import Util.Named
import Util.TypeTuple.Class

import qualified Lorentz.Contracts.TZBTC.Impl as TZBTC
import Lorentz.Contracts.TZBTC.Types

v1Impl :: Rec (EpwCaseClause StoreTemplate) Interface
v1Impl = recFromTuple
  ( #callGetAllowance //==> toSafeView TZBTC.getAllowance
  , #callGetBalance //==> toSafeView TZBTC.getBalance
  , #callGetTotalSupply //==> toSafeView TZBTC.getTotalSupply
  , #callGetTotalMinted //==> toSafeView TZBTC.getTotalMinted
  , #callGetTotalBurned //==> toSafeView TZBTC.getTotalBurned
  , #callGetOwner //==> toSafeView TZBTC.getOwner
  , #callGetTokenName //==> toSafeView TZBTC.getTokenName
  , #callGetTokenCode //==> toSafeView TZBTC.getTokenCode
  , #callGetRedeemAddress //==> toSafeView TZBTC.getRedeemAddress
  , #callTransfer //==> TZBTC.transfer
  , #callApprove //==> TZBTC.approve
  , #callMint //==> TZBTC.mint
  , #callBurn //==> TZBTC.burn
  , #callAddOperator //==> TZBTC.addOperator
  , #callRemoveOperator //==> TZBTC.removeOperator
  , #callSetRedeemAddress //==> TZBTC.setRedeemAddress
  , #callPause //==> TZBTC.pause
  , #callUnpause //==> TZBTC.unpause
  , #callTransferOwnership //==> TZBTC.transferOwnership
  , #callAcceptOwnership //==> TZBTC.acceptOwnership
  )
  where
    -- 'TZBTCPartInstr' slightly differs from what '/==>' expects, this
    -- function takes care of that.
    callPart ::
      forall arg.
      TZBTCPartInstr arg StoreTemplate ->
      Lambda (arg, UStore StoreTemplate) ([Operation], UStore StoreTemplate)
    callPart part = unpair # part # pair

    -- We convert an entry point from storage, that has an input of
    -- `SafeView` to an entry point that can accept a `View`.
    toSafeView
      :: forall a b. (NiceParameter b)
      => Entrypoint (View a b) (UStore StoreTemplate)
      -> Entrypoint (SafeView a b) (UStore StoreTemplate)
    toSafeView ep = do
      coerce_ @(SafeView a b) @((a, Address))
      unpair
      dip $ do
        contractCallingUnsafe DefEpName
        if IsSome then nop else failCustom_ #unexpectedContractType
      pair
      coerce_ @((a, ContractRef b)) @(View a b)
      ep

    -- Helper operator which is essentially the same as `/==>` but
    -- takes 'TZBTCPartInstr' so that we don't have to write 'callPart'
    -- for almost each method.
    label //==> part = label /==> callPart (part # unpair)

-- | Contract router before preprocessing.
tzbtcContractRouterRaw :: UContractRouter Interface StoreTemplate
tzbtcContractRouterRaw = epwServe epwContract

-- | Migrations to version 1 before preprocessing.
migrationScriptsRaw :: OriginationParameters -> [MigrationScript]
migrationScriptsRaw op = migrateStorage op : epwCodeMigrations epwContract

epwContract :: EpwContract Interface StoreTemplate
epwContract = mkEpwContract v1Impl epwFallbackFail

originationParamsToStoreTemplate :: OriginationParameters -> StoreTemplate
originationParamsToStoreTemplate OriginationParameters {..} = let
  total = Prelude.sum $ M.elems opBalances
  in StoreTemplate
    { owner = UStoreField opOwner
    , paused = UStoreField False
    , totalSupply = UStoreField total
    , totalMinted = UStoreField total
    , totalBurned = UStoreField 0
    , newOwner = UStoreField Nothing
    , operators = UStoreField mempty
    , redeemAddress = UStoreField opRedeemAddress
    , tokenName = UStoreField opTokenName
    , tokenCode = UStoreField opTokenCode
    , code = UStoreSubMap mempty
    , fallback = UStoreField epwFallbackFail
    , ledger = UStoreSubMap $ toLedgerValue <$> opBalances
    }
  where
    toLedgerValue i = (#balance .! i, #approvals .! mempty)

migrateStorage :: OriginationParameters -> MigrationScript
migrateStorage op =
  templateToMigration $ originationParamsToStoreTemplate op
  where
    templateToMigration :: StoreTemplate -> MigrationScript
    templateToMigration template = migrationToScript $ fillUStore template
