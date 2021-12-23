{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.V2.Contract
  ( Interface
  , StoreTemplate(..)
  , StoreTemplateV2
  , TZBTCv2
  , migrationScriptsRaw
  , migrationScriptsFromV1Raw
  , tzbtcContractRouterRaw
  , tzbtcDoc
  )
where

import Prelude hiding (drop, (>>))

import Lorentz
import Lorentz.Contracts.Upgradeable.Common.Base
import Lorentz.Contracts.Upgradeable.Common.Doc as U
import Lorentz.Contracts.Upgradeable.EntrypointWise
import Lorentz.UStore
import Lorentz.UStore.Doc (DUStoreTemplate(..))
import Lorentz.UStore.Migration
import qualified Morley.Michelson.Typed as T
import Morley.Util.TypeTuple.Class

import Lorentz.Contracts.TZBTC.Common.Doc
import Lorentz.Contracts.TZBTC.V0 (StoreTemplateV0)
import qualified Lorentz.Contracts.TZBTC.V0 as V0
import qualified Lorentz.Contracts.TZBTC.V1.Contract as V1
import qualified Lorentz.Contracts.TZBTC.V2.Impl as TZBTC
import Lorentz.Contracts.TZBTC.V2.Types

v2Impl :: Rec (EpwCaseClause StoreTemplateV2) Interface
v2Impl = recFromTuple
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
      TZBTCPartInstr arg StoreTemplateV2 ->
      Lambda (arg, UStore StoreTemplateV2) ([Operation], UStore StoreTemplateV2)
    callPart part = unpair # part # pair

    -- We convert an entry point from storage, that has an input of
    -- `SafeView` to an entry point that can accept a `View_`.
    toSafeView
      :: forall a b. (NiceParameter b)
      => Entrypoint (View_ a b) (UStore StoreTemplateV2)
      -> Entrypoint (SafeView a b) (UStore StoreTemplateV2)
    toSafeView ep = do
      coerceUnwrap
      unpair
      dip $ do
        unsafeContractCalling DefEpName
        if IsSome then nop else failCustom_ #unexpectedContractType
      pair
      wrapView_
      ep

    -- Helper operator which is essentially the same as `/==>` but
    -- takes 'TZBTCPartInstr' so that we don't have to write 'callPart'
    -- for almost each method.
    label //==> part = label /==> callPart (part # unpair)

-- | Contract router before preprocessing.
tzbtcContractRouterRaw :: UContractRouter TZBTCv2
tzbtcContractRouterRaw = epwServe epwContract

-- | Migrations to version 2 before preprocessing.
migrationScriptsRaw :: V2Parameters -> [MigrationScript StoreTemplateV0 StoreTemplateV2]
migrationScriptsRaw v2p = migrateStorage v2p : epwCodeMigrations epwContract

-- | Migrations to version 2 from version 1 before preprocessing.
migrationScriptsFromV1Raw :: V2ParametersFromV1 -> [MigrationScript V1.StoreTemplateV1 StoreTemplateV2]
migrationScriptsFromV1Raw V2ParametersFromV1 = migrateStorageFromV1 : epwCodeMigrations epwContract

epwContract :: EpwContract TZBTCv2
epwContract = mkEpwContract v2Impl epwFallbackFail

migrateStorage :: V2Parameters -> MigrationScript StoreTemplateV0 StoreTemplateV2
migrateStorage v1p = checkedCoerce $ V1.migrateStorage v1p

migrateStorageFromV1 :: MigrationScript V1.StoreTemplateV1 StoreTemplateV2
migrateStorageFromV1 =
  migrationToScript . mkUStoreMigration $ do
    -- We update code manually
    migrateCoerceUnsafe #code
    migrateCoerceUnsafe #fallback

    migrationFinish

tzbtcDoc :: Lambda () ()
tzbtcDoc = fakeCoercing $ do
  doc licenseInfoDoc
  docGroup "TZBTC" $ do
    contractGeneralDefault
    doc V1.tzbtcContractDesc
    doc $ DUpgradeability $ U.contractDoc <> "\n" <> additionalDeployNotes
    doc $ T.DStorageType $ DType $ Proxy @UStoreV2
    doc $ DUStoreTemplate (Proxy @(VerUStoreTemplate TZBTCv2))
    V0.tzbtcContractRaw
