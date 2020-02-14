{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE DerivingVia, RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.V0
  ( Parameter(..)
  , Interface
  , Storage(..)
  , StoreTemplateV0
  , UStoreV0
  , TZBTCv0
  , SomeTZBTCVersion
  , mkEmptyStorageV0
  , tzbtcContractRaw
  , tzbtcDoc
  ) where

import Prelude hiding (drop, swap, (>>))

import qualified Data.Text as T
import Data.Vinyl.Derived (Label)
import Fmt (Buildable(..), fmt)

import Lorentz
import Lorentz.Contracts.Upgradeable.Common hiding (Parameter(..), Storage)
import Lorentz.Contracts.Upgradeable.Common.Doc as U
import Lorentz.UStore.Migration (SomeUTemplate)
import Michelson.Doc (DComment(..), DDescription(..))
import Michelson.Text
import qualified Michelson.Typed as T
import Util.Markdown
import Util.TypeLits

import qualified Lorentz.Contracts.TZBTC.Impl as Impl
import Lorentz.Contracts.TZBTC.Types as Types

-- | Template for the wrapped UStore which will hold the owner address
-- only
data StoreTemplateV0 = StoreTemplateV0
  { owner :: UStoreField Address
  } deriving stock Generic

type UStoreV0 = Storage TZBTCv0

mkEmptyStorageV0 :: Address -> UStoreV0
mkEmptyStorageV0 owner = Storage
  { dataMap = mkUStore (StoreTemplateV0 $ UStoreField owner)
  , fields = StorageFields
    { contractRouter = emptyCode
    , currentVersion = 0
    , migrating = False
    }
  }

emptyCode :: UContractRouter ver
emptyCode = UContractRouter $ cdr # nil # pair

data TZBTCv0 :: VersionKind
instance KnownContractVersion TZBTCv0 where
  type VerInterface TZBTCv0 = Interface
  type VerUStoreTemplate TZBTCv0 = StoreTemplateV0
  contractVersion _ = 0

-- Use this when don't need particular version but still want to point out
-- that version is TZBTC-specific and thus have empty permanent part.
data SomeTZBTCVersion :: VersionKind
instance KnownContractVersion SomeTZBTCVersion where
  type VerInterface SomeTZBTCVersion = SomeInterface
  type VerUStoreTemplate SomeTZBTCVersion = SomeUTemplate
  contractVersion _ = 999

-- | Entry point of upgradeable contract.
data UpgradeableEntryPointKind

-- | Safe entry points of contract.
data SafeEntryPointKind

instance DocItem (DEntryPoint UpgradeableEntryPointKind) where
  type DocItemPosition (DEntryPoint UpgradeableEntryPointKind) = 1002
  docItemSectionName = Just "Top-level entry points of upgradeable contract."
  docItemSectionDescription = Just
    "These are entry points of the contract."
  docItemToMarkdown = diEntryPointToMarkdown

instance DocItem (DEntryPoint SafeEntryPointKind) where
  type DocItemPosition (DEntryPoint SafeEntryPointKind) = 1003
  docItemSectionName = Nothing
  docItemSectionDescription = Nothing
  docItemToMarkdown = diEntryPointToMarkdown

safeEntrypoints :: Entrypoint (SafeParameter TZBTCv0) UStoreV0
safeEntrypoints = entryCase @(SafeParameter TZBTCv0) (Proxy @SafeEntryPointKind)
  ( #cRun /-> do
      doc $ DDescription runDoc
      executeRun
  , #cUpgrade /-> do
      doc $ DDescription upgradeDoc
      dip (ensureMaster # ensureNotMigrating)
      dup; dip (toField #currentVersion >> toNamed #current >> checkVersion)
      dup; dip (toField #newVersion >> toNamed #new >> updateVersion)
      getField #migrationScript; swap; dip applyMigration
      toField #newCode; whenSome migrateCode
      nil; pair
  , #cEpwBeginUpgrade /-> do
      doc $ DDescription epwBeginUpgradeDoc
      dip (ensureMaster # ensureNotMigrating)
      dup; dip (toFieldNamed #current >> checkVersion)
      toFieldNamed #new >> updateVersion
      setMigrating True
      nil; pair
  , #cEpwApplyMigration /-> do
      doc $ DDescription epwApplyMigrationDoc
      fromNamed #migrationscript
      dip (ensureMaster # ensureMigrating)
      applyMigration
      nil; pair
  , #cEpwSetCode /-> do
      doc $ DDescription epwSetCodeDoc
      fromNamed #contractcode
      dip (ensureMaster # ensureMigrating)
      endWithMigration
  , #cEpwFinishUpgrade /-> do
      doc $ DDescription epwFinishUpgradeDoc
      ensureMaster
      ensureMigrating
      setMigrating False
      nil; pair
  , #cTransfer /-> do
      cutLorentzNonDoc (Impl.transfer @(UStore StoreTemplate))
      callUEp #callTransfer
  , #cApprove /-> do
      cutLorentzNonDoc (Impl.approve @(UStore StoreTemplate))
      callUEp #callApprove
  , #cMint /-> do
      cutLorentzNonDoc (Impl.mint @(UStore StoreTemplate))
      callUEp #callMint
  , #cBurn /-> do
      cutLorentzNonDoc (Impl.burn @(UStore StoreTemplate))
      callUEp #callBurn
  , #cAddOperator /-> do
      doc $ DDescription
        "This entry point is used to add a new operator."
      cutLorentzNonDoc (Impl.addOperator @(UStore StoreTemplate))
      callUEp #callAddOperator
  , #cRemoveOperator /-> do
      doc $ DDescription
        "This entry point is used to remove an operator."
      cutLorentzNonDoc (Impl.removeOperator @(UStore StoreTemplate))
      callUEp #callRemoveOperator
  , #cSetRedeemAddress /-> do
      doc $ DDescription
        "This entry point is used to set the redeem address."
      cutLorentzNonDoc (Impl.setRedeemAddress @(UStore StoreTemplate))
      callUEp #callSetRedeemAddress
  , #cPause /-> do
      doc $ DDescription
        "This entry point is used to pause the contract."
      cutLorentzNonDoc (Impl.pause @(UStore StoreTemplate))
      callUEp #callPause
  , #cUnpause /-> do
      doc $ DDescription
        "This entry point is used to resume the contract during a paused state."
      cutLorentzNonDoc (Impl.unpause @(UStore StoreTemplate))
      callUEp #callUnpause
  , #cTransferOwnership /-> do
      doc $ DDescription
        "This entry point is used to transfer ownership to a new owner."
      cutLorentzNonDoc (Impl.transferOwnership @(UStore StoreTemplate))
      callUEp #callTransferOwnership
  , #cAcceptOwnership /-> do
      doc $ DDescription
        "This entry point is used to accept ownership by a new owner."
      cutLorentzNonDoc (Impl.acceptOwnership @(UStore StoreTemplate))
      callUEp #callAcceptOwnership
  )
  where
    endWithMigration = migrateCode # nil # pair

-- | Version 0 of TZBTC contract as written in Lorentz.
-- It generally should not be used because we preprocess it before
-- actually using. See 'Lorentz.Contracts.TZBTC.Preprocess'.
tzbtcContractRaw :: Contract (Parameter TZBTCv0) UStoreV0
tzbtcContractRaw = do
  doc $ T.DStorageType $ DType $ Proxy @UStoreV0
  unpair
  entryCase @(Parameter TZBTCv0) (Proxy @UpgradeableEntryPointKind)
    ( #cGetVersion /-> do
        doc $ DDescription
          "This entry point is used to get contract version."
        view_ (do cdr; toField #fields; toField #currentVersion)
    , #cGetAllowance /-> do
        cutLorentzNonDoc (Impl.getAllowance @(UStore StoreTemplate))
        callUSafeViewEP #callGetAllowance
    , #cGetBalance /-> do
        cutLorentzNonDoc (Impl.getBalance @(UStore StoreTemplate))
        callUSafeViewEP #callGetBalance
    , #cGetTotalSupply /-> do
        cutLorentzNonDoc (Impl.getTotalSupply @(UStore StoreTemplate))
        callUSafeViewEP #callGetTotalSupply
    , #cGetTotalMinted /-> do
        cutLorentzNonDoc (Impl.getTotalMinted @(UStore StoreTemplate))
        callUSafeViewEP #callGetTotalMinted
    , #cGetTotalBurned /-> do
        cutLorentzNonDoc (Impl.getTotalBurned @(UStore StoreTemplate))
        callUSafeViewEP #callGetTotalBurned
    , #cGetOwner /-> do
        cutLorentzNonDoc (Impl.getOwner @(UStore StoreTemplate))
        callUSafeViewEP #callGetOwner
    , #cGetTokenName /-> do
        cutLorentzNonDoc Impl.getTokenName
        callUSafeViewEP #callGetTokenName
    , #cGetTokenCode /-> do
        cutLorentzNonDoc Impl.getTokenCode
        callUSafeViewEP #callGetTokenCode
    , #cGetRedeemAddress /-> do
        cutLorentzNonDoc (Impl.getRedeemAddress @(UStore StoreTemplate))
        callUSafeViewEP #callGetRedeemAddress
    , #cSafeEntrypoints /-> do
        doc $ DDescription
          "This entry point is used to call the safe entrypoints of the contract. \
          \Entrypoints are 'safe' because they don't have unsafe arguments, such \
          \as arguments with type `contract p` so that they can be safely used in operations \
          \that add them to the chain (since in babylon values with type `contract p` are prohibited in \
          \storage and code constants), in contrast to various Get* entrypoints, which have \
          \`contract p` and have to be handled additionally (basically, we have to pass simple \
          \`address` instead of `contract p` and call `CONTRACT`, which can fail)."
        safeEntrypoints
    )

tzbtcDoc :: ContractDoc
tzbtcDoc = buildLorentzDoc $ do
  -- License info
  doc $ DComment $ T.concat
    [ "- SP"
    , "DX-FileCopyrightText: 2019 Bitcoin Suisse\n"
    , "-\n"
    , "- SP"
    , "DX-License-Identifier: LicenseRef-Proprietary"
    ]
  contractName "TZBTC" $ do
    doc $ $mkDGitRevision $ GitRepoSettings $
      mappend "https://github.com/serokell/tezos-btc/commit/"
    doc $ DDescription
      "This contract is implemented using Lorentz language.\n\
      \Basically, this contract is [FA1.2](https://gitlab.com/serokell/morley/tzip/blob/master/A/FA1.2.md)\
      \-compatible approvable ledger that maps user addresses to their token balances. \
      \The main idea of this token contract is to provide 1-to-1 correspondance with BTC.\n\
      \There are two special entities for this contract:\n\
      \* `owner` -- owner of the TZBTC contract, capable in unpausing contract, \
      \adding/removing operators, transfering ownership and upgrading contract. \
      \There is only one owner of the contract.\n\
      \* `operator` -- entity which is capable in pausing the contract \
      \minting and burning tokens. There may be several operators added by the owner."
    doc $ DUpgradeability U.contractDoc
    tzbtcContractRaw
    fakeCoerce
    clarifyParamBuildingSteps pbsContainedInSafeEntrypoints safeEntrypoints

pbsContainedInSafeEntrypoints :: ParamBuildingStep
pbsContainedInSafeEntrypoints = ParamBuildingStep
  { pbsEnglish = "Wrap into " <> mdTicked "SafeEntrypoints" <> " constructor."
  , pbsHaskell = \p -> "Run (" <> p <> ")"
  , pbsMichelson = \p ->
      let param = Types.Pause @(EmptyContractVersion Empty) ()
      in build $
         T.replace (fmt . build . T.untypeValue $ toVal param)
                   ("(" <> fmt p <> ")")
                   (fmt . build . T.untypeValue . toVal $ SafeEntrypoints param)
  }

executeRun :: Entrypoint (VerParam ver) (Storage ver)
executeRun = do
  dip $ do
    ensureNotMigrating
    getField #dataMap
    dip $ do
      getField #fields
      toField #contractRouter
      coerceUnwrap
  pair
  exec
  unpair
  dip $ setField #dataMap
  pair

callUEp
  :: forall ep ver interface.
     ( interface ~ VerInterface ver
     , NicePackedValue (LookupEntryPoint ep interface)
     , KnownSymbol ep)
  => Label ep
  -> Entrypoint (LookupEntryPoint ep interface) (Storage ver)
callUEp epName = do
  pack
  push (labelToMText epName)
  pair
  stackType @'[(MText, ByteString), Storage _]
  coerceWrap
  stackType @'[UParam interface, Storage _]
  executeRun

callUSafeViewEP
  :: forall ep vi vo ver interface.
     ( interface ~ VerInterface ver
     , LookupEntryPoint ep interface ~ (SafeView vi vo)
     , NicePackedValue vi
     , KnownSymbol ep)
  => Label ep
  -> Entrypoint (View vi vo) (Storage ver)
callUSafeViewEP epName = do
  unwrapView
  unpair
  dip address
  pair
  coerceWrap
  callUEp epName

ensureMaster
  :: (HasUField "owner" Address (VerUStoreTemplate ver))
  => '[Storage ver] :-> '[Storage ver]
ensureMaster = do
  getField #dataMap;
  ustoreToField #owner
  sender; eq
  if_ (nop) (failCustom_ #senderIsNotOwner)

ensureNotMigrating :: '[Storage ver] :-> '[Storage ver]
ensureNotMigrating = do
  getField #fields; toField #migrating
  if_ (failCustom_ #upgContractIsMigrating) (nop)

checkVersion :: forall ver. '["current" :! Version, Storage ver] :-> '[Storage ver]
checkVersion = do
  fromNamed #current; toNamed #expectedCurrent
  dip (getField #fields >> toField #currentVersion >> toNamed #actualCurrent)
  if keepIfArgs (#expectedCurrent ==. #actualCurrent)
  then nop
  else do pair; failCustom #upgVersionMismatch

updateVersion :: forall ver. '["new" :! Version, Storage ver] :-> '[Storage ver]
updateVersion = do
  fromNamed #new
  dip $ getField #fields
  setField #currentVersion; setField #fields


applyMigration
  :: '[MigrationScriptFrom store, Storage ver] :-> '[Storage ver]
applyMigration = do
  coerceUnwrap
  dip $ getField #dataMap
  checkedCoerce_
  swap
  exec
  setField #dataMap

migrateCode :: '[SomeUContractRouter, Storage ver] :-> '[Storage ver]
migrateCode = do
  dip (getField #fields)
  checkedCoerce_
  setField #contractRouter
  setField #fields

setMigrating :: Bool -> '[Storage ver] :-> '[Storage ver]
setMigrating newState = do
  getField #fields
  push newState
  setField #migrating
  setField #fields

ensureMigrating :: '[Storage ver] :-> '[Storage ver]
ensureMigrating = do
  getField #fields; toField #migrating
  if_ (nop) (failCustom_ #upgContractIsNotMigrating)
