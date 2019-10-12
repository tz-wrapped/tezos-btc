-- | Initial (0) version of DS protocol contract. It's just an empty
-- entrypoint-wise upgradeable contract. In order to use it one should
-- upgrade to V1.
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.V0
  ( Parameter(..)
  , ViewParameter(..)
  , Storage(..)
  , mkEmptyStorage

  , tzbtcContract
  , tzbtcCompilationWay
  ) where

import Prelude hiding (drop, swap, (>>))

import Data.Vinyl.Derived (Label)
import qualified Data.Map as M

import Lorentz
import Lorentz.Contracts.Upgradeable.Common hiding (Parameter, Storage, mkEmptyStorage)
import Lorentz.Contracts.Upgradeable.Common.Base
import Michelson.Text
import qualified Michelson.Typed as T
import qualified Lorentz.Contracts.TZBTC.Types as TZBTC
import Util.TypeLits
import GHC.TypeLits (Symbol)
import Michelson.Typed.Haskell.Instr.Product (GetFieldType)

type UpgradeParameters =
  ( "newVersion" :! Natural
  , "migrationScript" :! MigrationScript
  , "newCode" :! ContractCode
  )

data ViewParameter (interface :: [EntryPointKind])
  = GetVersion (View () Natural)
  -- TZBTC Entrypoints
  | GetAllowance        !(View TZBTC.GetAllowanceParams Natural)
  | GetBalance          !(View Address Natural)
  | GetTotalSupply      !(View () Natural)
  | GetTotalMinted      !(View () Natural)
  | GetTotalBurned      !(View () Natural)
  | GetAdministrator    !(View () Address)
  deriving stock Generic
  deriving anyclass IsoValue

instance TypeHasDoc (ViewParameter interface) where
  typeDocName _ = "TODO"
  typeDocMdDescription = "TODO"
  typeDocMdReference tp =
    customTypeDocMdReference ("TODO", DType tp) []
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

data Parameter (interface :: [EntryPointKind])
  = Run (UParam interface)
  | Upgrade UpgradeParameters
  | SetAdministrator Address

  -- Entrypoint-wise upgrades are currently not protected from version mismatch
  -- in subsequent transactions, so the user ought to be careful with them.
  -- This behavior may change in future if deemed desirable.
  | EpwBeginUpgrade Natural  -- version
  | EpwApplyMigration MigrationScript
  | EpwSetCode ContractCode
  | EpwFinishUpgrade
  -- TZBTC Entrypoints
  | Transfer            !TZBTC.TransferParams
  | TransferViaProxy    !TZBTC.TransferViaProxyParams
  | Approve             !TZBTC.ApproveParams
  | ApproveViaProxy     !TZBTC.ApproveViaProxyParams
  | Mint                !TZBTC.MintParams
  | Burn                !TZBTC.BurnParams
  | AddOperator         !TZBTC.OperatorParams
  | RemoveOperator      !TZBTC.OperatorParams
  | SetRedeemAddress    !TZBTC.SetRedeemAddressParams
  | Pause               !()
  | Unpause             !()
  | TransferOwnership   !TZBTC.TransferOwnershipParams
  | AcceptOwnership     !TZBTC.AcceptOwnershipParams
  | SetProxy            !TZBTC.SetProxyParams
  -- View entrypoints
  | Views (ViewParameter interface)
  deriving stock Generic
  deriving anyclass IsoValue

--type Storage = Upgradeable.Storage

data StorageFields = StorageFields
  { code  :: ContractCode
  , admin :: Address
  , currentVersion :: Natural
  , paused :: Bool
  } deriving stock Generic
    deriving anyclass IsoValue

data Storage = Storage
  { dataMap :: UStore_
  , fields :: StorageFields
  } deriving stock Generic
    deriving anyclass IsoValue

mkEmptyStorage :: Address -> Storage
mkEmptyStorage admin = Storage
  { dataMap = T.BigMap $ M.fromList []
  , fields = StorageFields
    { code  = emptyCode
    , admin = admin
    , currentVersion = 0
    , paused = False
    }
  }

emptyCode :: ContractCode
emptyCode = unpair # drop # nil # pair

tzbtcCompilationWay :: LorentzCompilationWay (Parameter '[]) Storage
tzbtcCompilationWay = lcwEntryPoints

-- | Entry point of upgradeable contract.
data UpgradeableEntryPointKind

instance DocItem (DEntryPoint UpgradeableEntryPointKind) where
  type DocItemPosition (DEntryPoint UpgradeableEntryPointKind) = 1002
  docItemSectionName = Just "Top-level entry points of upgradeable contract"
  docItemSectionDescription = Just
    "These are top-level service entry points of the contract.\n\
    \For entry points containing the logic of the contract see sections below.\n\
    \<Description of Run and migrations-related entry points>"
  docItemToMarkdown = diEntryPointToMarkdown

tzbtcContract :: forall interface. Contract (Parameter interface) Storage
tzbtcContract = do
  unpair
  entryCase @(Parameter interface) (Proxy @UpgradeableEntryPointKind)
    ( #cRun /-> executeRun
    , #cUpgrade /-> do
        dip (ensureAdmin # ensureNotPaused)
        dup; dip (toField #newVersion # checkVersion # bumpVersion)
        getField #migrationScript; swap; dip (applyMigration)
        toField #newCode; migrateCode
        nil; pair
    , #cSetAdministrator /-> do
        dip (ensureAdmin # getField #fields)
        setField #admin
        setField #fields
        nil; pair
    , #cEpwBeginUpgrade /-> do
        dip (ensureAdmin # ensureNotPaused)
        checkVersion
        setPaused True
        nil; pair
    , #cEpwApplyMigration /-> do
        dip (ensureAdmin # ensurePaused)
        applyMigration
        nil; pair
    , #cEpwSetCode /-> do
        dip (ensureAdmin # ensurePaused)
        migrateCode
        nil; pair
    , #cEpwFinishUpgrade /-> do
        ensureAdmin
        ensurePaused
        bumpVersion
        setPaused False
        nil; pair
    , #cTransfer /-> callUEp #callTransfer
    , #cTransferViaProxy /-> callUEp #callTransferViaProxy
    , #cApprove /-> callUEp #callApprove
    , #cApproveViaProxy /-> callUEp #callApproveViaProxy
    , #cMint /-> callUEp #callMint
    , #cBurn /-> callUEp #callBurn
    , #cAddOperator /-> callUEp #callAddOperator
    , #cRemoveOperator /-> callUEp #callRemoveOperator
    , #cSetRedeemAddress /-> callUEp #callSetRedeemAddress
    , #cPause /-> callUEp #callPause
    , #cUnpause /-> callUEp #callUnpause
    , #cTransferOwnership /-> callUEp #callTransferOwnership
    , #cAcceptOwnership /-> callUEp #callAcceptOwnership
    , #cSetProxy /-> callUEp #callSetProxy
    , #cViews /-> entryCase @(ViewParameter interface) (Proxy @UpgradeableEntryPointKind)
        ( #cGetVersion /-> view_ (do cdr; toField #fields; toField #currentVersion)
        , #cGetAllowance /-> callUEp #callGetAllowance
        , #cGetBalance /-> callUEp #callGetBalance
        , #cGetTotalSupply /-> callUEp #callGetTotalSupply
        , #cGetTotalMinted /-> callUEp #callGetTotalMinted
        , #cGetTotalBurned /-> callUEp #callGetTotalBurned
        , #cGetAdministrator /-> callUEp #callGetAdministrator
        )
    )

executeRun :: TZBTC.Entrypoint (UParam a) Storage
executeRun = do
  doc $ DDescription
    "This entry point is used to call all entry points carrying actual \
    \logic of the contract."
  dip $ do
    ensureNotPaused
    getField #dataMap
    dip $ do
      getField #fields
      toField #code
  unwrapUParam
  pair
  exec
  unpair
  dip $ setField #dataMap
  pair

callUEp
  :: forall ep inp. (KnownValue inp, NoOperation inp, NoBigMap inp, KnownSymbol ep)
  => Label ep
  -> TZBTC.Entrypoint inp Storage
callUEp epName = do
  pack
  push (labelToMText epName)
  pair
  stackType @'[(MText, ByteString), Storage]
  coerce_
  stackType @'[UParam _, Storage]
  executeRun

ensureAdmin :: '[Storage] :-> '[Storage]
ensureAdmin = do
  getField #fields; toField #admin
  sender; eq
  if_ (nop) (failCustom_ #senderIsNotAdmin)

ensureNotPaused :: '[Storage] :-> '[Storage]
ensureNotPaused = do
  getField #fields; toField #paused
  if_ (failCustom_ #upgContractIsPaused) (nop)

checkVersion :: '[Natural, Storage] :-> '[Storage]
checkVersion = do
  duupX @2; toField #fields; toField #currentVersion
  push @Natural 1
  add
  stackType @('[Natural, Natural, Storage])
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

bumpVersion :: '[Storage] :-> '[Storage]
bumpVersion = do
  getField #fields
  getField #currentVersion
  push @Natural 1
  add
  setField #currentVersion
  setField #fields

applyMigration
  :: '[MigrationScript, Storage] :-> '[Storage]
applyMigration = do
  dip $ getField #dataMap
  swap
  exec
  setField #dataMap

migrateCode :: '[ContractCode, Storage] :-> '[Storage]
migrateCode = do
  dip (getField #fields)
  setField #code
  setField #fields

setPaused :: Bool -> '[Storage] :-> '[Storage]
setPaused newState = do
  getField #fields
  push newState
  setField #paused
  setField #fields

ensurePaused :: '[Storage] :-> '[Storage]
ensurePaused = do
  getField #fields; toField #paused
  if_ (nop) (failCustom_ #upgContractIsNotPaused)
