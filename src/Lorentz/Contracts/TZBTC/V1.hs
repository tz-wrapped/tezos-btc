{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.V1
  ( Interface
  , StoreTemplate(..)
  , StoreTemplateV1
  , TZBTCv1
  , migrationScriptsRaw
  , tzbtcContractRouterRaw
  )
where

import Prelude hiding (drop, (>>))

import qualified Data.Map as M

import Lorentz
import Lorentz.Contracts.Upgradeable.Common.Base
import Lorentz.Contracts.Upgradeable.EntrypointWise
import Lorentz.UStore.Haskell
import Util.Named
import Util.TypeTuple.Class

import qualified Lorentz.Contracts.TZBTC.Impl as TZBTC
import Lorentz.Contracts.TZBTC.Types
import Lorentz.Contracts.TZBTC.V0 (StoreTemplateV0)

v1Impl :: Rec (EpwCaseClause StoreTemplateV1) Interface
v1Impl = recFromTuple
  ( #callGetAllowance //==> toSafeView TZBTC.getAllowance
  , #callGetBalance //==> toSafeView TZBTC.getBalance
  , #callGetTotalSupply //==> toSafeView TZBTC.getTotalSupply
  , #callGetTotalMinted //==> toSafeView TZBTC.getTotalMinted
  , #callGetTotalBurned //==> toSafeView TZBTC.getTotalBurned
  , #callGetOwner //==> toSafeView TZBTC.getOwner
  , #callGetRedeemAddress //==> toSafeView TZBTC.getRedeemAddress
  , #callGetTokenMetadata //==> toSafeView TZBTC.getTokenMetadata
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
      TZBTCPartInstr arg StoreTemplateV1 ->
      Lambda (arg, UStore StoreTemplateV1) ([Operation], UStore StoreTemplateV1)
    callPart part = unpair # part # pair

    -- We convert an entry point from storage, that has an input of
    -- `SafeView` to an entry point that can accept a `View`.
    toSafeView
      :: forall a b. (NiceParameter b)
      => Entrypoint (View a b) (UStore StoreTemplateV1)
      -> Entrypoint (SafeView a b) (UStore StoreTemplateV1)
    toSafeView ep = do
      coerceUnwrap
      unpair
      dip $ do
        contractCallingUnsafe DefEpName
        if IsSome then nop else failCustom_ #unexpectedContractType
      pair
      wrapView
      ep

    -- Helper operator which is essentially the same as `/==>` but
    -- takes 'TZBTCPartInstr' so that we don't have to write 'callPart'
    -- for almost each method.
    label //==> part = label /==> callPart (part # unpair)

-- | Contract router before preprocessing.
tzbtcContractRouterRaw :: UContractRouter TZBTCv1
tzbtcContractRouterRaw = epwServe epwContract

-- | Migrations to version 1 before preprocessing.
migrationScriptsRaw :: V1Parameters -> [MigrationScript StoreTemplateV0 StoreTemplateV1]
migrationScriptsRaw v1p = migrateStorage v1p : epwCodeMigrations epwContract

epwContract :: EpwContract TZBTCv1
epwContract = mkEpwContract v1Impl epwFallbackFail

originationParamsToStoreTemplate :: V1Parameters -> StoreTemplate
originationParamsToStoreTemplate V1Parameters {..} = let
  total = Prelude.sum $ M.elems v1Balances
  in StoreTemplate
    { paused = UStoreField False
    , totalSupply = UStoreField total
    , totalMinted = UStoreField total
    , totalBurned = UStoreField 0
    , newOwner = UStoreField Nothing
    , operators = UStoreField mempty
    , redeemAddress = UStoreField v1RedeemAddress
    , tokenMetadata = UStoreField v1TokenMetadata
    , code = UStoreSubMap mempty
    , fallback = UStoreField epwFallbackFail
    , ledger = UStoreSubMap $ toLedgerValue <$> v1Balances
    }
  where
    toLedgerValue i = (#balance .! i, #approvals .! mempty)

migrateStorage :: V1Parameters -> MigrationScript StoreTemplateV0 StoreTemplateV1
migrateStorage v1p =
  templateToMigration $ originationParamsToStoreTemplate v1p
  where
    templateToMigration :: StoreTemplate -> MigrationScript StoreTemplateV0 StoreTemplateV1
    templateToMigration template =
      migrationToScript $ mkUStoreMigration $ migrateFillUStore template
