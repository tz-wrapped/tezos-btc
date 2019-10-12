{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Lorentz.Contracts.TZBTC.V1 where

import Prelude hiding (drop, (>>))

import qualified Data.Map as M

import Lorentz
import Lorentz.Contracts.Upgradeable.Common (upgradeableContract)
import qualified Lorentz.Contracts.Upgradeable.Common as Upgradeable
import Lorentz.Contracts.Upgradeable.EntryPointWise
import Lorentz.Contracts.Upgradeable.Common.Base
import Lorentz.Contracts.TZBTC.Types
import Util.TypeTuple.Class
import qualified Lorentz.Contracts.ManagedLedger.Types as ManagedLedger
import qualified Lorentz.Contracts.ManagedLedger.Impl as ManagedLedger

import qualified Lorentz.Contracts.TZBTC.V0 as V0
import qualified Lorentz.Contracts.TZBTC.Impl as TZBTC
import Util.Named

version :: Natural
version = 1

data StoreTemplate = StoreTemplate
  { admin         :: UStoreField Address
  , paused        :: UStoreField Bool
  , totalSupply   :: UStoreField Natural
  , totalBurned   :: UStoreField Natural
  , totalMinted   :: UStoreField Natural
  , newOwner      :: UStoreField (Maybe Address)
  , operators     :: UStoreField (Set Address)
  , redeemAddress :: UStoreField Address
  , tokenname     :: UStoreField MText
  , tokencode     :: UStoreField MText
  , proxy         :: UStoreField (Either Address Address)
  , code          :: MText |~> EntryPointImpl StoreTemplate
  , fallback      :: UStoreField $ EpwFallback StoreTemplate
  , ledger        :: Address |~> ManagedLedger.LedgerValue
  } deriving stock Generic

type UStoreV1 = UStore StoreTemplate

data OriginationParameters = OriginationParameters
  { opMaster :: !Address
  , opRedeemAddress :: !Address
  , opInitialSupply :: !Natural
  , opTokenname :: !MText
  , opTokencode :: !MText
  }

type Interface =
  [ "callGetAllowance" ?: (View ManagedLedger.GetAllowanceParams Natural)
  , "callGetBalance" ?: (View Address Natural)
  , "callGetTotalSupply" ?: (View () Natural)
  , "callGetTotalMinted" ?: (View () Natural)
  , "callGetTotalBurned" ?: (View () Natural)
  , "callGetAdministrator" ?: (View () Address)
  , "callTransfer" ?: TransferParams
  , "callTransferViaProxy" ?: TransferViaProxyParams
  , "callApprove" ?: ApproveParams
  , "callApproveViaProxy" ?: ApproveViaProxyParams
  , "callSetAdministrator" ?: Address
  , "callMint" ?: ManagedLedger.MintParams
  , "callBurn" ?: BurnParams
  , "callAddOperator" ?: OperatorParams
  , "callRemoveOperator" ?: OperatorParams
  , "callSetRedeemAddress" ?: SetRedeemAddressParams
  , "callPause" ?: ()
  , "callUnpause" ?: ()
  , "callTransferOwnership" ?: TransferOwnershipParams
  , "callAcceptOwnership" ?: AcceptOwnershipParams
  , "callSetProxy" ?: SetProxyParams
  ]

type Parameter = V0.Parameter Interface

-- | Type of instruction which implements a part of TZBTC Protocol.
type TZBTCPartInstr param store =
  '[param, UStore store] :->
  '[[Operation], UStore store]

v1Impl :: Rec (EpwCaseClause StoreTemplate) Interface
v1Impl = recFromTuple
  ( #callGetAllowance //==> TZBTC.getAllowance
  , #callGetBalance //==> TZBTC.getBalance
  , #callGetTotalSupply //==> TZBTC.getTotalSupply
  , #callGetTotalMinted //==>
      TZBTC.getTotal #totalMinted "Return total number of minted tokens"
  , #callGetTotalBurned //==>
      TZBTC.getTotal #totalBurned "Return total number of burned tokens"
  , #callGetAdministrator //==> ManagedLedger.getAdministrator
  , #callTransfer //==> TZBTC.transfer
  , #callTransferViaProxy //==> TZBTC.transferViaProxy
  , #callApprove //==> TZBTC.approve
  , #callApproveViaProxy //==> TZBTC.approveViaProxy
  , #callSetAdministrator //==> TZBTC.setAdministrator
  , #callMint //==> TZBTC.mint
  , #callBurn //==> TZBTC.burn
  , #callAddOperator //==> TZBTC.addOperator
  , #callRemoveOperator //==> TZBTC.removeOperator
  , #callSetRedeemAddress //==> TZBTC.setRedeemAddress
  , #callPause //==> TZBTC.pause
  , #callUnpause //==> TZBTC.unpause
  , #callTransferOwnership //==> TZBTC.transferOwnership
  , #callAcceptOwnership //==> TZBTC.acceptOwnership
  , #callSetProxy //==> TZBTC.setProxy
  )
  where
    -- 'TZBTCPartInstr' slightly differs from what '/==>' expects, this
    -- function takes care of that.
    callPart ::
      forall arg.
      TZBTCPartInstr arg StoreTemplate ->
      Lambda (arg, UStore StoreTemplate) ([Operation], UStore StoreTemplate)
    callPart part = unpair # part # pair

    -- Helper operator which is essentially the same as `/==>` but
    -- takes 'TZBTCPartInstr' so that we don't have to write 'callPart'
    -- for almost each method.
    label //==> part = label /==> callPart (part # unpair)

tzbtcContractCode :: ContractCode
tzbtcContractCode = epwServe epwContract

originationParams :: Address -> Address -> Natural -> OriginationParameters
originationParams addr redeem initial =
  OriginationParameters
    { opMaster = addr
    , opRedeemAddress = redeem
    , opInitialSupply = initial
    , opTokenname = [mt|TZBTC|]
    , opTokencode = [mt|ZBTC|]
    }

migrationScripts :: OriginationParameters -> [MigrationScript]
migrationScripts op = migrateStorage op : epwCodeMigrations epwContract

epwContract :: EpwContract Interface StoreTemplate
epwContract = mkEpwContract v1Impl epwFallbackFail

migrateStorage :: OriginationParameters -> MigrationScript
migrateStorage OriginationParameters {..} =
  withUStoreV1 migrationScript
  where
    migrationScript :: Lambda UStoreV1 UStoreV1
    migrationScript = do
      push opTokencode; ustoreSetField #tokencode
      push opTokenname; ustoreSetField #tokenname
      push opMaster; ustoreSetField #admin
      push opInitialSupply; ustoreSetField #totalSupply
      push opInitialSupply; ustoreSetField #totalMinted
      push 0; ustoreSetField #totalBurned
      push opRedeemAddress; ustoreSetField #redeemAddress
      push mempty; ustoreSetField #operators
      push Nothing; ustoreSetField #newOwner
      push False; ustoreSetField #paused
      push $ Left opMaster ; ustoreSetField #proxy
      push $ toLedgerValue opInitialSupply
      push opRedeemAddress
      ustoreInsert #ledger
    withUStoreV1 :: Lambda UStoreV1 UStoreV1 -> Lambda UStore_ UStore_
    withUStoreV1 instr = do
      coerce_ @UStore_ @UStoreV1
      instr
      coerce_ @UStoreV1 @UStore_

    toLedgerValue initBal = (#balance .! initBal, #approvals .! mempty)

printContractCode :: LText
printContractCode = printLorentzValue False $ tzbtcContractCode

printMigrationCode :: OriginationParameters -> LText
printMigrationCode op = printLorentzValue False $ migrationScripts op

-- Implementation
