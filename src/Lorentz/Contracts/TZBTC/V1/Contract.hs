{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.V1.Contract
  ( Interface
  , StoreTemplate(..)
  , StoreTemplateV1
  , TZBTCv1
  , migrationScriptsRaw
  , tzbtcContractRouterRaw
  , tzbtcDoc

    -- * Helpers
  , migrateStorage
  , tzbtcContractDesc
  )
where

import Prelude hiding (drop, (>>))

import Data.Map qualified as M

import Lorentz
import Lorentz.Contracts.Upgradeable.Common.Base
import Lorentz.Contracts.Upgradeable.Common.Doc as U
import Lorentz.Contracts.Upgradeable.EntrypointWise
import Lorentz.UStore
import Lorentz.UStore.Doc (DUStoreTemplate(..))
import Lorentz.UStore.Haskell
import Morley.Michelson.Typed qualified as T
import Morley.Util.Named
import Morley.Util.TypeTuple.Class

import Lorentz.Contracts.TZBTC.Common.Doc
import Lorentz.Contracts.TZBTC.V0 (StoreTemplateV0)
import Lorentz.Contracts.TZBTC.V0 qualified as V0
import Lorentz.Contracts.TZBTC.V1.Impl qualified as TZBTC
import Lorentz.Contracts.TZBTC.V1.Types

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
    -- `SafeView` to an entry point that can accept a `View_`.
    toSafeView
      :: forall a b. (NiceParameter b)
      => Entrypoint (View_ a b) (UStore StoreTemplateV1)
      -> Entrypoint (SafeView a b) (UStore StoreTemplateV1)
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
    toLedgerValue i = (#balance :! i, #approvals :! mempty)

migrateStorage :: V1Parameters -> MigrationScript StoreTemplateV0 StoreTemplateV1
migrateStorage v1p =
  templateToMigration $ originationParamsToStoreTemplate v1p
  where
    templateToMigration :: StoreTemplate -> MigrationScript StoreTemplateV0 StoreTemplateV1
    templateToMigration template =
      migrationToScript $ mkUStoreMigration $ migrateFillUStore template

tzbtcDoc :: Lambda () ()
tzbtcDoc = fakeCoercing $ do
  doc licenseInfoDoc
  docGroup (DName "TZBTC") $ do
    contractGeneralDefault
    doc tzbtcContractDesc
    doc $ DUpgradeability $ U.contractDoc <> "\n" <> additionalDeployNotes
    doc $ T.DStorageType $ DType $ Proxy @UStoreV1
    doc $ DUStoreTemplate (Proxy @(VerUStoreTemplate TZBTCv1))
    V0.tzbtcContractRaw

tzbtcContractDesc :: DDescription
tzbtcContractDesc = DDescription
  "This contract is implemented using Lorentz language.\n\
  \Basically, this contract is [FA1.2](https://gitlab.com/serokell/morley/tzip/-/blob/be56b0322363259af938e102ac84b0f488695872/Proposals/TZIP-0007/FA1.2.md)\
  \-compatible approvable ledger that maps user addresses to their token balances. \
  \The main idea of this token contract is to provide 1-to-1 correspondance with BTC.\n\
  \There are two special entities for this contract:\n\
  \* `owner` -- owner of the TZBTC contract, capable in unpausing contract, \
  \adding/removing operators, transfering ownership and upgrading contract. \
  \There is only one owner of the contract.\n\
  \* `operator` -- entity which is capable in pausing the contract \
  \minting and burning tokens. There may be several operators added by the owner."
