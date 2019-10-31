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
  , UStoreV0
  , mkEmptyStorageV0
  , tzbtcContract
  , tzbtcDoc
  ) where

import Prelude hiding (drop, swap, (>>))

import qualified Data.Text as T
import Data.Vinyl.Derived (Label)
import Fmt (Buildable (..), fmt)

import Lorentz
import Lorentz.Contracts.Upgradeable.Common hiding (Parameter(..), Storage)
import Michelson.Text
import Michelson.Typed.Doc
  (DComment(..), DDescription(..), contractDocToMarkdown)
import qualified Michelson.Typed as T
import Util.TypeLits
import Util.Markdown

import Lorentz.Contracts.TZBTC.Types as Types

-- | Template for the wrapped UStore which will hold the admin address
-- only
data StoreTemplateV0 = StoreTemplateV0
  { admin         :: UStoreField Address
  } deriving stock Generic

mkEmptyStorageV0 :: Address -> UStoreV0
mkEmptyStorageV0 admin = Storage
  { dataMap = mkUStore (StoreTemplateV0 $ UStoreField admin)
  , fields = StorageFields
    { contractRouter  = emptyCode
    , currentVersion = 0
    , migrating = False
    }
  }

type UStoreV0 = Storage Interface StoreTemplateV0

emptyCode :: UContractRouter interface
emptyCode = UContractRouter $ cdr # nil # pair

-- | Entry point of upgradeable contract.
data UpgradeableEntryPointKind

instance DocItem (DEntryPoint UpgradeableEntryPointKind) where
  type DocItemPosition (DEntryPoint UpgradeableEntryPointKind) = 1002
  docItemSectionName = Just "Top-level entry points of upgradeable contract."
  docItemSectionDescription = Just
    "These are entry points of the contract."
  docItemToMarkdown = diEntryPointToMarkdown

safeEntrypoints :: Entrypoint (SafeParameter Interface) UStoreV0
safeEntrypoints = entryCase @(SafeParameter Interface) (Proxy @UpgradeableEntryPointKind)
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
      callUEp #callTransfer
  , #cApprove /-> do
      doc $ DDescription
        "This entry point is used approve transfer of tokens from one account to another."
      callUEp #callApprove
  , #cMint /-> do
      doc $ DDescription
        "This entry point is used mint new tokes for an account."
      callUEp #callMint
  , #cBurn /-> do
      doc $ DDescription
        "This entry point is used burn tokes from the redeem address."
      callUEp #callBurn
  , #cAddOperator /-> do
      doc $ DDescription
        "This entry point is used to add a new operator."
      callUEp #callAddOperator
  , #cRemoveOperator /-> do
      doc $ DDescription
        "This entry point is used to remove an operator."
      callUEp #callRemoveOperator
  , #cSetRedeemAddress /-> do
      doc $ DDescription
        "This entry point is used to set the redeem address."
      callUEp #callSetRedeemAddress
  , #cPause /-> do
      doc $ DDescription
        "This entry point is used to pause the contract."
      callUEp #callPause
  , #cUnpause /-> do
      doc $ DDescription
        "This entry point is used to resume the contract during a paused state."
      callUEp #callUnpause
  , #cTransferOwnership /-> do
      doc $ DDescription
        "This entry point is used to transfer ownership to a new owner."
      callUEp #callTransferOwnership
  , #cAcceptOwnership /-> do
      doc $ DDescription
        "This entry point is used to accept ownership by a new owner."
      callUEp #callAcceptOwnership
  )
  where
    endWithMigration = migrateCode # nil # pair

tzbtcContract :: Contract (Parameter Interface) UStoreV0
tzbtcContract = do
  unpair
  entryCase @(Parameter Interface) (Proxy @UpgradeableEntryPointKind)
    ( #cGetVersion /-> do
        doc $ DDescription
          "This entry point is used to get contract version."
        view_ (do cdr; toField #fields; toField #currentVersion)
    , #cGetAllowance /-> do
        doc $ DDescription
          "This entry point is used to get allowance for an account."
        callUSafeViewEP #callGetAllowance
    , #cGetBalance /-> do
        doc $ DDescription
          "This entry point is used to get balance in an account."
        callUSafeViewEP #callGetBalance
    , #cGetTotalSupply /-> do
        doc $ DDescription
          "This entry point is used to get total number of tokens."
        callUSafeViewEP #callGetTotalSupply
    , #cGetTotalMinted /-> do
        doc $ DDescription
          "This entry point is used to get total number of minted tokens."
        callUSafeViewEP #callGetTotalMinted
    , #cGetTotalBurned /-> do
        doc $ DDescription
          "This entry point is used to get total number of burned tokens."
        callUSafeViewEP #callGetTotalBurned
    , #cGetAdministrator /-> do
        doc $ DDescription
          "This entry point is used to get current administrator."
        callUSafeViewEP #callGetAdministrator
    , #cSafeEntrypoints /-> do
        doc $ DDescription
          "This entry point is used call the safe entrypoints of the contract."
        safeEntrypoints
    )

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
  contractName "TZBTC" $ do
    doc $ DDescription "This contract is implemented using Lorentz language"
    tzbtcContract
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
  coerce_ @(View vi vo) @(vi, ContractAddr vo)
  unpair
  dip address
  pair
  coerce_ @((vi, Address)) @(SafeView vi vo)
  callUEp epName

ensureMaster :: (HasUField "admin" Address store) => '[Storage interface store] :-> '[Storage interface store]
ensureMaster = do
  getField #dataMap;
  ustoreToField #admin
  sender; eq
  if_ (nop) (failCustom_ #senderIsNotAdmin)

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

migrateCode :: '[UContractRouter interface, Storage interface store] :-> '[Storage interface store]
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