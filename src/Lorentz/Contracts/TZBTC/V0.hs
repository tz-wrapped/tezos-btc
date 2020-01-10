{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.V0
  ( Parameter(..)
  , Interface
  , Storage(..)
  , StoreTemplateV0
  , UStoreV0
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

mkEmptyStorageV0 :: Address -> UStoreV0
mkEmptyStorageV0 owner = Storage
  { dataMap = mkUStore (StoreTemplateV0 $ UStoreField owner)
  , fields = StorageFields
    { contractRouter = emptyCode
    , currentVersion = 0
    , migrating = False
    }
  }

type UStoreV0 = Storage Interface StoreTemplateV0

emptyCode :: UContractRouter interface store
emptyCode = UContractRouter $ cdr # nil # pair

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

safeEntrypoints :: Entrypoint (SafeParameter Interface StoreTemplateV0) UStoreV0
safeEntrypoints = entryCase @(SafeParameter Interface StoreTemplateV0) (Proxy @SafeEntryPointKind)
  ( #cRun /-> do
      doc $ DDescription
        "This entry point is used to call the packed entrypoints in the contract."
      executeRun
  , #cUpgrade /-> do
      doc $ DDescription
        "This entry point is used to update the contract to a new version."
      dip (ensureMaster # ensureNotMigrating)
      dup; dip (toField #newVersion # checkVersion # bumpVersion)
      getField #migrationScript; swap; dip (applyMigration)
      toField #newCode;
      endWithMigration
  , #cEpwBeginUpgrade /-> do
      doc $ DDescription
        "This entry point is used to start an entrypoint wise upgrade of the contract."
      dip (ensureMaster # ensureNotMigrating)
      checkVersion
      setMigrating True
      nil; pair
  , #cEpwApplyMigration /-> do
      doc $ DDescription
        "This entry point is used to apply an migration script as part of an upgrade."
      fromNamed #migrationscript
      dip (ensureMaster # ensureMigrating)
      applyMigration
      nil; pair
  , #cEpwSetCode /-> do
      doc $ DDescription
        "This entry point is used to set the dispatching code that calls the packed entrypoints."
      fromNamed #contractcode
      dip (ensureMaster # ensureMigrating)
      endWithMigration
  , #cEpwFinishUpgrade /-> do
      doc $ DDescription
        "This entry point is used to mark that an upgrade has been finsihed."
      ensureMaster
      ensureMigrating
      bumpVersion
      setMigrating False
      nil; pair
  , #cTransfer /-> do
      doc $ DDescription
        "This entry point is used transfer tokens from one account to another."
      cutLorentzNonDoc (Impl.transfer @(UStore StoreTemplate))
      callUEp #callTransfer
  , #cApprove /-> do
      doc $ DDescription
        "This entry point is used approve transfer of tokens from one account to another."
      cutLorentzNonDoc (Impl.approve @(UStore StoreTemplate))
      callUEp #callApprove
  , #cMint /-> do
      doc $ DDescription
        "This entry point is used mint new tokes for an account."
      cutLorentzNonDoc (Impl.mint @(UStore StoreTemplate))
      callUEp #callMint
  , #cBurn /-> do
      doc $ DDescription
        "This entry point is used burn tokes from the redeem address."
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
tzbtcContractRaw :: Contract (Parameter Interface StoreTemplateV0) UStoreV0
tzbtcContractRaw = do
  unpair
  entryCase @(Parameter Interface StoreTemplateV0) (Proxy @UpgradeableEntryPointKind)
    ( #cGetVersion /-> do
        doc $ DDescription
          "This entry point is used to get contract version."
        view_ (do cdr; toField #fields; toField #currentVersion)
    , #cGetAllowance /-> do
        doc $ DDescription
          "This entry point is used to get allowance for an account."
        cutLorentzNonDoc (Impl.getAllowance @(UStore StoreTemplate))
        callUSafeViewEP #callGetAllowance
    , #cGetBalance /-> do
        doc $ DDescription
          "This entry point is used to get balance in an account."
        cutLorentzNonDoc (Impl.getBalance @(UStore StoreTemplate))
        callUSafeViewEP #callGetBalance
    , #cGetTotalSupply /-> do
        doc $ DDescription
          "This entry point is used to get total number of tokens."
        cutLorentzNonDoc (Impl.getTotalSupply @(UStore StoreTemplate))
        callUSafeViewEP #callGetTotalSupply
    , #cGetTotalMinted /-> do
        doc $ DDescription
          "This entry point is used to get total number of minted tokens."
        cutLorentzNonDoc (Impl.getTotalMinted @(UStore StoreTemplate))
        callUSafeViewEP #callGetTotalMinted
    , #cGetTotalBurned /-> do
        doc $ DDescription
          "This entry point is used to get total number of burned tokens."
        cutLorentzNonDoc (Impl.getTotalBurned @(UStore StoreTemplate))
        callUSafeViewEP #callGetTotalBurned
    , #cGetOwner /-> do
        doc $ DDescription
          "This entry point is used to get current owner."
        cutLorentzNonDoc (Impl.getOwner @(UStore StoreTemplate))
        callUSafeViewEP #callGetOwner
    , #cGetTokenName /-> do
        doc $ DDescription
          "This entry point is used to get token name."
        cutLorentzNonDoc Impl.getTokenName
        callUSafeViewEP #callGetTokenName
    , #cGetTokenCode /-> do
        doc $ DDescription
          "This entry point is used to get token code."
        cutLorentzNonDoc Impl.getTokenCode
        callUSafeViewEP #callGetTokenCode
    , #cGetRedeemAddress /-> do
        doc $ DDescription
          "This entry point is used to get redeem address."
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
    tzbtcContractRaw
    fakeCoerce
    clarifyParamBuildingSteps pbsContainedInSafeEntrypoints safeEntrypoints

pbsContainedInSafeEntrypoints :: ParamBuildingStep
pbsContainedInSafeEntrypoints = ParamBuildingStep
  { pbsEnglish = "Wrap into " <> mdTicked "SafeEntrypoints" <> " constructor."
  , pbsHaskell = \p -> "Run (" <> p <> ")"
  , pbsMichelson = \p ->
      let param = Types.Pause ()
      in build $
         T.replace (fmt . build . T.untypeValue $ toVal param)
                   ("(" <> fmt p <> ")")
                   (fmt . build . T.untypeValue . toVal $ SafeEntrypoints param)
  }

executeRun :: Entrypoint (UParam a) (Storage interface store)
executeRun = do
  dip $ do
    ensureNotMigrating
    getField #dataMap
    dip $ do
      getField #fields
      toField #contractRouter
      coerce_
  unwrapUParam
  pair
  exec
  unpair
  dip $ setField #dataMap
  pair

callUEp
  :: forall ep interface store.
     ( NicePackedValue (LookupEntryPoint ep interface)
     , KnownSymbol ep)
  => Label ep
  -> Entrypoint (LookupEntryPoint ep interface) (Storage interface store)
callUEp epName = do
  pack
  push (labelToMText epName)
  pair
  stackType @'[(MText, ByteString), Storage interface store]
  coerce_
  stackType @'[UParam interface, Storage interface store]
  executeRun

callUSafeViewEP
  :: forall ep vi vo interface store.
     ( LookupEntryPoint ep interface ~ (SafeView vi vo)
     , NicePackedValue vi
     , KnownSymbol ep)
  => Label ep
  -> Entrypoint (View vi vo) (Storage interface store)
callUSafeViewEP epName = do
  coerce_ @(View vi vo) @(vi, ContractRef vo)
  unpair
  dip address
  pair
  coerce_ @((vi, Address)) @(SafeView vi vo)
  callUEp epName

ensureMaster :: (HasUField "owner" Address store) => '[Storage interface store] :-> '[Storage interface store]
ensureMaster = do
  getField #dataMap;
  ustoreToField #owner
  sender; eq
  if_ (nop) (failCustom_ #senderIsNotOwner)

ensureNotMigrating :: '[Storage interface store] :-> '[Storage interface store]
ensureNotMigrating = do
  getField #fields; toField #migrating
  if_ (failCustom_ #upgContractIsMigrating) (nop)

checkVersion
  :: forall interface store. '[Natural, Storage interface store] :-> '[Storage interface store]
checkVersion = do
  duupX @2; toField #fields; toField #currentVersion
  push @Natural 1
  add
  stackType @('[Natural, Natural, Storage interface store])
  pair
  assertVersionsEqual
  where
    assertVersionsEqual = do
      dup
      unpair
      if IsEq
      then drop
      else do
        unpair
        toNamed #expected
        dip $ toNamed #actual
        pair
        failCustom #upgVersionMismatch

bumpVersion :: '[Storage interface store] :-> '[Storage interface store]
bumpVersion = do
  getField #fields
  getField #currentVersion
  push @Natural 1
  add
  setField #currentVersion
  setField #fields

applyMigration
  :: '[MigrationScript, Storage interface store] :-> '[Storage interface store]
applyMigration = do
  coerce_
  dip $ getField #dataMap
  swap
  exec
  setField #dataMap

migrateCode :: '[UContractRouter interface store, Storage interface store] :-> '[Storage interface store]
migrateCode = do
  dip (getField #fields)
  gcoerce_
  setField #contractRouter
  setField #fields

setMigrating :: Bool -> '[Storage interface store] :-> '[Storage interface store]
setMigrating newState = do
  getField #fields
  push newState
  setField #migrating
  setField #fields

ensureMigrating :: '[Storage interface store] :-> '[Storage interface store]
ensureMigrating = do
  getField #fields; toField #migrating
  if_ (nop) (failCustom_ #upgContractIsNotMigrating)
