{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

-- We have to test some deprecated behaviour.
-- For V1 this is fine, for V2 this will be fixed.
{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}

module Test.TZBTC
  ( test_interface
  , test_acceptOwnership
  , test_burn
  , test_mint
  , test_approvableLedgerV1
  , test_approvableLedgerV2
  , test_pause
  , test_unpause
  , test_setRedeemAddress
  , test_transferOwnership
  , test_bookkeeping
  , test_addOperator
  , test_removeOperator
  , test_get_meta
  , test_documentation

  -- * Utilities
  , TestTZBTCVersion (checkField, getOperators)
  , coerceContractHandler
  , dummyV1Parameters
  , originateTzbtcV1ContractRaw
  ) where

import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Text.IO.Utf8 qualified as Utf8
import Data.Typeable (typeRep)
import Fmt (Builder, pretty, (+|), (|+))
import Test.Tasty (TestTree, testGroup)

import Lorentz (BigMapId, Contract(cMichelsonContract), EpName, IsoValue(..), Value, mkView_)
import Lorentz.Contracts.Metadata (singleTokenTokenId)
import Lorentz.Contracts.Spec.ApprovableLedgerInterface qualified as AL
import Lorentz.Contracts.Test.ApprovableLedger qualified as AL
import Lorentz.Contracts.Test.ManagedLedger (OriginationParams(..), originateManagedLedger)
import Lorentz.Contracts.Test.ManagedLedger qualified as ML
import Lorentz.Contracts.Upgradeable.Client (UStoreElemRef(..))
import Lorentz.Contracts.Upgradeable.Common
  (EpwUpgradeParameters(..), KnownContractVersion, VerPermanent, VerUStoreTemplate,
  emptyPermanentImplCompat)
import Lorentz.Contracts.Upgradeable.Common.Empty (Empty)
import Lorentz.UStore (DecomposeUStoreTW, KnownUStoreMarker(mkFieldMarkerUKey), UStoreTraversable)
import Lorentz.UStore.Migration (manualConcatMigrationScripts)
import Lorentz.UStore.Types (UMarkerPlainField, UStoreSubmapKeyT)
import Morley.Michelson.Interpret.Pack (packValue')
import Morley.Michelson.Interpret.Unpack (unpackValue')
import Morley.Michelson.Parser.Types (MichelsonSource(..))
import Morley.Michelson.Runtime (parseExpandContract)
import Morley.Michelson.Typed
  (SomeConstrainedValue(SomeConstrainedValue), UnpackedValScope, Value'(VPair), convertContract)
import Morley.Michelson.Untyped qualified as U
import Morley.Michelson.Untyped.Entrypoints (mkEntrypointsMap)
import Morley.Tezos.Address
import Test.Cleveland
import Test.Cleveland.Lorentz
  (contractConsumer, runDocTests, testContractCoversEntrypointsT, testLorentzDoc)
import Test.Cleveland.Michelson.Entrypoints (testContractCoversEntrypoints)

import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.Common.Types (Operators)
import Lorentz.Contracts.TZBTC.V1 qualified as V1
import Lorentz.Contracts.TZBTC.V2 qualified as V2
import Test.Smoke (TestUpgrade(..), testUpgradeToV1, testUpgradeToV2)

import Test.AsRPC (StorageRPC)
import Test.AsRPC qualified as RPC

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- This function is mostly copied from morley-upgradeable
-- Lorentz.Contracts.Upgradeable.Client.readContractUStore, but it accepts 'BigMapId' instead
-- of a contract address, and is adapted to run inside 'MonadCleveland'.
readUStore
  :: forall v m caps.
     (UnpackedValScope v, Typeable v, MonadCleveland caps m)
  => BigMapId ByteString ByteString -> UStoreElemRef -> m (Value v)
readUStore bmId ref = do
  let ukey = refToKey ref
  uval <- getBigMapValue bmId ukey
  unpackValue' uval
    & either (const (throwUnpackFailed uval)) pure
  where
    throwUnpackFailed uval =
      failure $ "UStore value unpack failed" +| uval |+ " :: " +| show @Builder (typeRep (Proxy @v)) |+ ""

    refToKey :: UStoreElemRef -> ByteString
    refToKey = \case
      UrField field ->
        mkFieldMarkerUKey @UMarkerPlainField field
      UrSubmap field (SomeConstrainedValue key) ->
        packValue' @(UStoreSubmapKeyT _) $ VPair (toVal field, key)

newtype TUStoreElemRef t = TUStoreElemRef UStoreElemRef

class KnownContractVersion ver => TestTZBTCVersion ver where
  checkField
    :: ( MonadCleveland caps m
       , UnpackedValScope (ToT v)
       , Typeable (ToT v)
       , IsoValue v
       )
    => TUStoreElemRef v
    -> (v -> Bool)
    -> StorageRPC ver
    -> Builder
    -> m ()
  checkField (TUStoreElemRef k) fn st msg = do
    val <- readUStore (RPC.dataMap st) k
    unless (fn (fromVal val)) $ failure msg

  getOperators :: TUStoreElemRef Operators
  getNewOwner :: TUStoreElemRef (Maybe Address)
  getPaused :: TUStoreElemRef Bool
  getOperators = TUStoreElemRef $ UrField "operators"
  getNewOwner = TUStoreElemRef $ UrField "newOwner"
  getPaused = TUStoreElemRef $ UrField "paused"

instance TestTZBTCVersion TZBTCv1
instance TestTZBTCVersion V2.TZBTCv2

type TestableTZBTCVersion ver =
  ( TestTZBTCVersion ver, Generic (VerUStoreTemplate ver), Typeable ver
  , VerPermanent ver ~ Empty, UStoreTraversable DecomposeUStoreTW (VerUStoreTemplate ver)
  )

dummyV1Parameters :: Address -> Map Address Natural -> V1Parameters
dummyV1Parameters redeem balances = V1Parameters
  { v1RedeemAddress = redeem
  , v1TokenMetadata = defaultTZBTCMetadata
  , v1Balances = balances
  }

dummyV2Parameters :: Address -> Map Address Natural -> V2Parameters
dummyV2Parameters = dummyV1Parameters


redeemAddress_ :: ImplicitAddress
redeemAddress_ = [ta|tz1Mdd7rL6jGFAWCMUqatQ64K9pTpe8kazfy|]

coerceContractHandler
  :: (Typeable ver2, VerPermanent ver2 ~ Empty)
  => ContractHandle (Parameter ver1) (Storage ver1) () -> ContractHandle (Parameter ver2) (Storage ver2) ()
coerceContractHandler c = ContractHandle { chContractName = chContractName c, chAddress = chAddress c }

originateTzbtcV1ContractRaw
  :: (HasCallStack, MonadCleveland caps m, MonadFail m)
  => ImplicitAddress -> OriginationParams
  -> m (ContractHandle (Parameter TZBTCv1) (Storage TZBTCv1) ())
originateTzbtcV1ContractRaw redeem op = do
  let balances = ML.opBalances op
  MkAddress owner@ImplicitAddress{} <- pure $ ML.opAdmin op
  c <- originate "TZBTC Contract" (mkEmptyStorageV0 $ toL1Address owner) tzbtcContract
  unTestUpgrade testUpgradeToV1 owner redeem balances c
  pure $ coerceContractHandler c

originateTzbtcV1Contract
  :: (HasCallStack, MonadCleveland caps m, MonadFail m)
  => Address -> m (ContractHandle (Parameter TZBTCv1) (Storage TZBTCv1) ())
originateTzbtcV1Contract admin = do
  originateTzbtcV1ContractRaw redeemAddress_ $ OriginationParams
    { opAdmin = admin
    , opBalances = M.fromList [(toAddress redeemAddress_, initialSupply)]
    }

originateTzbtcV2ContractRaw
  :: (HasCallStack, MonadCleveland caps m, MonadFail m)
  => ImplicitAddress -> OriginationParams
  -> m (ContractHandle (Parameter V2.TZBTCv2) (Storage V2.TZBTCv2) ())
originateTzbtcV2ContractRaw redeem op = do
  let balances = ML.opBalances op
  MkAddress owner@ImplicitAddress{} <- pure $ ML.opAdmin op
  c <- originate "TZBTC Conctact" (mkEmptyStorageV0 $ toL1Address owner) tzbtcContract
  unTestUpgrade testUpgradeToV2 owner redeem balances c
  pure $ coerceContractHandler c

originateTzbtcV2Contract
  :: (HasCallStack, MonadCleveland caps m, MonadFail m)
  => Address -> m (ContractHandle (Parameter V2.TZBTCv2) (Storage V2.TZBTCv2) ())
originateTzbtcV2Contract admin = do
  originateTzbtcV2ContractRaw redeemAddress_ $ OriginationParams
    { opAdmin = admin
    , opBalances = M.fromList [((toAddress redeemAddress_), initialSupply)]
    }

-- Some constants

initialSupply :: Natural
initialSupply = 500

-- We have the header of the actual tzbtc contract (parameter, storage types with
-- some nop code) in ./test/resources/tzbtc-parameter-entrypoints-ref.tz.
-- We parse it, extract the paramter type from the parsed contract. Then we use
-- the 'mkEntrypointsMap' function to extract the field and type annotations from
-- the relavant parts of the parameter type. This is compared against the
-- auto derivied entrypoints of the contract. They should match or else the
-- test fails. If the tzbtc parameter has to be changed, then this test should
-- be fixed by editing the `tzbtc-parameter-entrypoints-ref.tz` file.
entrypointsRef :: IO (Map EpName U.Ty)
entrypointsRef = mkEntrypointsMap <$> tzbtcParameterType
  where
    tzbtcParameterType :: IO U.ParameterType
    tzbtcParameterType = do
      code <- Utf8.readFile "./test/resources/tzbtc-parameter-entrypoints-ref.tz"
      case parseExpandContract MSUnspecified code of
        Right c -> pure $ U.contractParameter c
        Left e -> error ("Error in parsing reference contract paramter:" <> pretty e)

-- Tests

testContract
  :: String
  -> ( forall m caps ver. (MonadCleveland caps m, TestableTZBTCVersion ver)
       => (Address -> m (ContractHandle (Parameter ver) (Storage ver) ())) -> Proxy ver -> m ()
     )
  -> TestTree
testContract name testSuite =
  testGroup name $
    [ testScenario "V1 contract" $ scenario $ testSuite originateTzbtcV1Contract Proxy
    , testScenario "V2 contract" $ scenario $ testSuite originateTzbtcV2Contract Proxy
    ]

test_interface :: IO TestTree
test_interface = do
  reference <- entrypointsRef
  pure $ testGroup "TZBTC consistency test"
    [ testContractCoversEntrypointsT @AL.Parameter
        "Has an approvable ledger interface that satisfies FA1.2 specification"
        tzbtcContract

    , let untypedTzbtc = convertContract $ cMichelsonContract tzbtcContract
      in testContractCoversEntrypoints
        "Has the expected interface of TZBTC contract"
        untypedTzbtc reference
    ]

test_addOperator :: TestTree
test_addOperator = testGroup "TZBTC contract `addOperator` test"
  [ testContract
      "Call to `addOperator` from random address gets denied with `senderIsNotOwner` error." $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        bob <- refillable $ newAddress auto
        operator <- refillable $ newAddress auto
        withSender bob $
          expectCustomError_ #senderIsNotOwner $ transfer c $ calling def $
          fromFlatParameter $ AddOperator (#operator :! toAddress operator)
  , testContract
      "Call to `addOperator` from owner adds operator to the set." $
      \originateContract (_ :: Proxy ver) -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        operator <- refillable $ newAddress auto
        withSender admin $
          transfer c $ calling def $ fromFlatParameter $ AddOperator (#operator :! toAddress operator)
        st <- getStorage c
        checkField (getOperators @ver) (Set.member $ toAddress operator) st
          "New operator not found"
  ]

test_removeOperator :: TestTree
test_removeOperator = testGroup "TZBTC contract `addOperator` test"
  [ testContract
      "Call to `removeOperator` from random address gets denied with `senderIsNotOwner` error." $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        bob <- refillable $ newAddress auto
        operator <- refillable $ newAddress auto
        withSender bob $
          expectCustomError_ #senderIsNotOwner $
          transfer c $ calling def $ fromFlatParameter $ RemoveOperator (#operator :! toAddress operator)

  , testContract
      "Call to `removeOperator` from owner removes operator from the set." $
      \originateContract (_ :: Proxy ver) -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        operator <- refillable $ newAddress auto
        withSender admin $ transfer c $ calling def $ fromFlatParameter $ AddOperator (#operator :! toAddress operator)
        st1 <- getStorage c
        checkField (getOperators @ver) (Set.member $ toAddress operator) st1
          "New operator not found"
        withSender admin $ transfer c $ calling def $ fromFlatParameter $ RemoveOperator (#operator :! toAddress operator)
        st2 <- getStorage c
        checkField (getOperators @ver) (Prelude.not . Set.member (toAddress operator)) st2
          "Unexpectedly found operator"
  ]

test_transferOwnership :: TestTree
test_transferOwnership = testGroup "TZBTC contract `transferOwnership` test"
  [ testContract
      "Call to `transferOwnership` from random address gets denied with `senderIsNotOwner` error." $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        bob <- refillable $ newAddress auto
        replaceAddress <- refillable $ newAddress "replaceAddress"
        withSender bob $ expectCustomError_ #senderIsNotOwner $
          transfer c $ calling def $ fromFlatParameter $ TransferOwnership (#newOwner :! toAddress replaceAddress)
  , testContract
      "Call to `transferOwnership` from owner address gets denied with `senderIsNotOwner` error." $
      \originateContract (_ :: Proxy ver) -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        replaceAddress <- refillable $ newAddress auto
        withSender admin $
          transfer c $ calling def $ fromFlatParameter $ TransferOwnership (#newOwner :! toAddress replaceAddress)
        st <- getStorage c
        checkField (getNewOwner @ver) (== Just (toAddress replaceAddress)) st
          "Expected `newOwner` not found"
  ]

test_acceptOwnership :: TestTree
test_acceptOwnership = testGroup "TZBTC contract `acceptOwnership` test"
  [ testContract
      "Call to `acceptOwnership` to non-transfering contract gets denied with `notInTransferOwnershipMode` error " $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        bob <- refillable $ newAddress auto
        withSender bob $ expectCustomError_ #notInTransferOwnershipMode $
          transfer c $ calling def $ fromFlatParameter $ AcceptOwnership ()
  , testContract
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        bob <- refillable $ newAddress auto
        replaceAddress <- refillable $ newAddress auto
        withSender admin $
          transfer c $ calling def $ fromFlatParameter $ TransferOwnership (#newOwner :! toAddress replaceAddress)
        withSender bob $ expectCustomError_ #senderIsNotNewOwner $
          transfer c $ calling def $ fromFlatParameter $ AcceptOwnership ()
  , testContract
      "Call to `acceptOwnership` to transferring contract from random address gets denied with `senderIsNotNewOwner` error." $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        replaceAddress <- refillable $ newAddress auto
        withSender admin $
          transfer c $ calling def $ fromFlatParameter $ TransferOwnership (#newOwner :! toAddress replaceAddress)
        withSender replaceAddress $ do
          transfer c $ calling def $ fromFlatParameter $ AcceptOwnership ()
        consumer <- originate "consumer" [] contractConsumer
        transfer c $ calling def $ fromFlatParameter $ GetOwner $ mkView_ () consumer
        consumerStorage <- getStorage consumer
        assert (consumerStorage == [toAddress replaceAddress]) "Unexpected owner found"
  , testContract
      "Call to `acceptOwnership` to transferring contract from current address gets denied with `senderIsNotNewOwner` error." $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        replaceAddress <- refillable $ newAddress auto
        withSender admin $
          transfer c $ calling def $ fromFlatParameter $ TransferOwnership (#newOwner :! toAddress replaceAddress)
        expectCustomError_ #senderIsNotNewOwner $
          transfer c $ calling def $ fromFlatParameter $ AcceptOwnership ()
  ]

test_setRedeemAddress :: TestTree
test_setRedeemAddress = testGroup "TZBTC contract `setRedeemAddress` test"
  [ testContract
      "Call to `setRedeemAddress` from random address is denied with `SenderIsNotOwner` error" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        replaceAddress <- refillable $ newAddress auto
        bob <- refillable $ newAddress auto
        withSender bob $ expectCustomError_ #senderIsNotOwner $
          transfer c $ calling def $ fromFlatParameter $ SetRedeemAddress (#redeem :! toAddress replaceAddress)
  , testContract
      "Call to `setRedeemAddress` sets redeem address correctly" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        replaceAddress <- refillable $ newAddress auto
        withSender admin $
          transfer c $ calling def $ fromFlatParameter $ SetRedeemAddress (#redeem :! toAddress replaceAddress)
        consumer <- originate "consumer" [] contractConsumer
        transfer c $ calling def $ fromFlatParameter $ GetRedeemAddress $ mkView_ () consumer
        consumerStorage <- getStorage $ consumer
        assert (consumerStorage == [toAddress replaceAddress]) "Updated redeem address doesn't match"
  ]

test_burn :: TestTree
test_burn = testGroup "TZBTC contract `burn` test"
  [ testContract
      "Call to `burn` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        bob <- refillable $ newAddress auto
        withSender bob $ expectCustomError_ #senderIsNotOperator $
          transfer c $ calling def $ fromFlatParameter $ Burn (#value :! 100)
  , testContract
      "Call to `burn` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        withSender admin $ expectCustomError_ #senderIsNotOperator $
          transfer c $ calling def $ fromFlatParameter $ Burn (#value :! 100)
  , testContract
      "Call to `burn` from operator subtracts from redeemAddress and update bookkeeping fields" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        operator <- refillable $ newAddress "operator"
        consumer <- originate "consumer" [] contractConsumer

        -- Add an operator
        withSender admin $
          transfer c $ calling def $ fromFlatParameter $ AddOperator (#operator :! toAddress operator)

        transfer c $ calling def $ fromFlatParameter $ GetBalance (mkView_ (#owner :! toAddress redeemAddress_) consumer)

        withSender operator $
          transfer c $ calling def $ fromFlatParameter $ Burn (#value :! 130)

        transfer c $ calling def $ fromFlatParameter $ GetBalance (mkView_ (#owner :! toAddress redeemAddress_) consumer)
        transfer c $ calling def $ fromFlatParameter $ GetTotalBurned (mkView_ () consumer)
        transfer c $ calling def $ fromFlatParameter $ GetTotalSupply (mkView_ () consumer)
        transfer c $ calling def $ fromFlatParameter $ GetTotalMinted (mkView_ () consumer)
        consumerStorage <- getStorage consumer
        let expectedStorage =
              [ initialSupply
              , initialSupply - 130
              , 130
              , initialSupply - 130
              , initialSupply
              ]
        assert (consumerStorage == expectedStorage) "Consumer storage doesn't match"
  ]

test_mint :: TestTree
test_mint = testGroup "TZBTC contract `mint` test"
  [ testContract
      "Call to `mint` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        alice <- refillable $ newAddress auto
        bob <- refillable $ newAddress auto
        withSender bob $ expectCustomError_ #senderIsNotOperator $
          transfer c $ calling def $ fromFlatParameter $ Mint (#to :! toAddress alice, #value :! 100)
  , testContract
      "Call to `mint` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        alice <- refillable $ newAddress auto
        withSender admin $ expectCustomError_ #senderIsNotOperator $
          transfer c $ calling def $ fromFlatParameter $ Mint (#to :! toAddress alice, #value :! 100)
  , testContract
      "Call to `mint` from operator adds to account and update bookkeeping fields" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        operator <- refillable $ newAddress "operator"
        alice <- refillable $ newAddress auto
        consumer <- originate "consumer" [] contractConsumer

        -- Add an operator
        withSender admin $
          transfer c $ calling def $ fromFlatParameter $ AddOperator (#operator :! toAddress operator)

        withSender operator $
          transfer c $ calling def (fromFlatParameter $ Mint (#to :! toAddress alice, #value :! 130))

        transfer c $ calling def $ fromFlatParameter $ GetBalance (mkView_ (#owner :! toAddress alice) consumer)
        transfer c $ calling def $ fromFlatParameter $ GetTotalBurned (mkView_ () consumer)
        transfer c $ calling def $ fromFlatParameter $ GetTotalSupply (mkView_ () consumer)
        transfer c $ calling def $ fromFlatParameter $ GetTotalMinted (mkView_ () consumer)
        consumerStorage <- getStorage consumer
        let expectedStorage =
              [ initialSupply + 130
              , initialSupply + 130
              , 0
              , 130
              ]
        assert (consumerStorage == expectedStorage) "Consumer storage doesn't match"
  ]

test_approvableLedgerV1 :: TestTree
test_approvableLedgerV1 = testGroup "TZBTC contract approvable ledger tests"
  -- Our code does not comply with the recent approvable ledger spec in some
  -- minor points, so we are testing a set of pieces that cover the old behaviour
  -- of ManagedLedger .
  [ AL.alEmptyInitBalanceTest @(Parameter TZBTCv1) @(Storage TZBTCv1) alOriginate
  , AL.alTransferTest @(Parameter TZBTCv1) @(Storage TZBTCv1) alOriginate
  , AL.alTransferSelfAlwaysTest @(Parameter TZBTCv1) @(Storage TZBTCv1) alOriginate
  , AL.alAllowTest @(Parameter TZBTCv1) @(Storage TZBTCv1) alOriginate
  ]
  where
    alOriginate
      :: (MonadCleveland caps m)
      => ImplicitAddress -> AL.AlSettings -> m (ContractHandle (Parameter TZBTCv1) (Storage TZBTCv1) ())
    alOriginate = originateManagedLedger $ \op  -> do
      owner <- refillable $ pure $ case ML.opAdmin op of
            MkAddress x@ImplicitAddress{} -> x
            _ -> error "Unexpected address type in alOriginate"
      c <- originate "TZBTC Contract" (mkEmptyStorageV0 $ toL1Address owner) tzbtcContract
      let
        opTZBTC = dummyV1Parameters (toAddress redeemAddress_) (ML.opBalances op)
        upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
          { upMigrationScripts =
            Identity $
            manualConcatMigrationScripts (migrationScriptsV1 opTZBTC)
          , upNewCode = tzbtcContractRouterV1
          , upNewPermCode = emptyPermanentImplCompat
          }
      withSender owner $
        transfer c $ calling def $ fromFlatParameter $ Upgrade upgradeParams
      pure $ coerceContractHandler c

test_approvableLedgerV2 :: TestTree
test_approvableLedgerV2 = testGroup "TZBTC contract approvable ledger tests"
  [ AL.approvableLedgerGenericTest @(Parameter SomeTZBTCVersion) @(Storage SomeTZBTCVersion) $
    originateManagedLedger $ \op -> do
      owner <- refillable $ pure $ case ML.opAdmin op of
            MkAddress x@ImplicitAddress{} -> x
            _ -> error "Unexpected address type in originateManagedLedger"
      c <- originate "TZBTC Contract" (mkEmptyStorageV0 $ toL1Address owner) tzbtcContract
      let
        opTZBTC = dummyV2Parameters (toAddress redeemAddress_) (ML.opBalances op)
        upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
          { upMigrationScripts =
            Identity $
            manualConcatMigrationScripts (migrationScriptsV2 opTZBTC)
          , upNewCode = tzbtcContractRouterV2
          , upNewPermCode = emptyPermanentImplCompat
          }
      withSender owner $
        transfer c $ calling def $ fromFlatParameter $ Upgrade upgradeParams
      pure $ coerceContractHandler c
  ]

test_pause :: TestTree
test_pause = testGroup "TZBTC contract `pause` test"
  [ testContract
      "Call to `pause` from random address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        bob <- refillable $ newAddress auto
        withSender bob $ expectCustomError_ #senderIsNotOperator $
          transfer c $ calling def $ fromFlatParameter $ Pause ()
  , testContract
      "Call to `pause` from owner address is denied with `SenderIsNotOperator` error" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        withSender admin $ expectCustomError_ #senderIsNotOperator $
          transfer c $ calling def $ fromFlatParameter $ Pause ()
  , testContract
      "Call to `pause` from operator sets the paused status" $
      \originateContract (_ :: Proxy ver) -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        operator <- refillable $ newAddress "operator"
        -- Add an operator
        withSender admin $ transfer c $ calling def $ fromFlatParameter $ AddOperator (#operator :! toAddress operator)

        -- Call pause
        withSender operator $
          transfer c $ calling def $ fromFlatParameter $ Pause ()

        st <- getStorage c
        checkField (getPaused @ver) id st "Unexpected Paused status"
  ]

test_unpause :: TestTree
test_unpause = testGroup "TZBTC contract `unpause` test"
  [ testContract
      "Call to `unpause` from random address is denied with `SenderIsNotOwner` error" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        bob <- refillable $ newAddress "bob"
        withSender bob $ expectCustomError_ #senderIsNotOwner $
          transfer c $ calling def $ fromFlatParameter $ Unpause ()
  , testContract
      "Call to `unpause` from operator address is denied with `SenderIsNotOwner` error" $
      \originateContract _ -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        operator <- refillable $ newAddress "operator"
        -- Add an operator
        withSender admin $ transfer c $ calling def $ fromFlatParameter $ AddOperator (#operator :! toAddress operator)
        withSender operator $ expectCustomError_ #senderIsNotOwner $
          transfer c $ calling def $ fromFlatParameter $ Unpause ()
  , testContract
      "Call to `unpause` from owner unsets the paused status" $
      \originateContract (_ :: Proxy ver) -> do
        admin <- refillable $ newAddress "admin"
        c <- originateContract $ toAddress admin
        operator <- refillable $ newAddress "operator"
        -- Add an operator
        withSender admin $ transfer c $ calling def $ fromFlatParameter $ AddOperator (#operator :! toAddress operator)
        -- Pause the contract
        withSender operator $ transfer c $ calling def $ fromFlatParameter $ Pause ()
        st1 <- getStorage c

        checkField (getPaused @ver) id st1 "Unexpected Paused status"

        -- Call unpause
        withSender admin $ transfer c $ calling def $ fromFlatParameter $ Unpause ()
        st2 <- getStorage c

        checkField (getPaused @ver) Prelude.not st2 "Unexpected Paused status"
  ]

test_bookkeeping :: TestTree
test_bookkeeping = testGroup "TZBTC contract bookkeeping views test"
  [ testContract
      "calling book keeping views returns expected result" $
      \originateContract _ -> do
          admin <- refillable $ newAddress "admin"
          v1 <- originateContract $ toAddress admin
          operator <- refillable $ newAddress "operator"
          alice <- refillable $ newAddress "alice"
          -- Add an operator
          withSender admin $ transfer v1 $ calling def $ fromFlatParameter $ AddOperator (#operator :! toAddress operator)
          withSender operator $ do
            -- Mint and burn some tokens
            transfer v1 $ calling def (fromFlatParameter $ Mint (#to :! toAddress alice, #value :! 130))
            transfer v1 $ calling def (fromFlatParameter $ Burn (#value :! 20))

          consumer <- originate "consumer" [] contractConsumer

          transfer v1 $ calling def $ fromFlatParameter $ GetTotalSupply (mkView_ () consumer)
          transfer v1 $ calling def $ fromFlatParameter $ GetTotalMinted (mkView_ () consumer)
          transfer v1 $ calling def $ fromFlatParameter $ GetTotalBurned (mkView_ () consumer)
          -- Check expectations
          natConsumerStorage <- getStorage consumer
          assert (natConsumerStorage == [20, 630, 610 :: Natural]) "Unexpected nat consumer storage"

          -- Check redeem address getter (we need another consumer
          -- because the type is different).
          withSender admin $ transfer v1 $ calling def (fromFlatParameter $ SetRedeemAddress (#redeem :! toAddress operator))
          consumerAddr <- originate "consumerAddr" [] contractConsumer
          transfer v1 $ calling def $ fromFlatParameter $ GetRedeemAddress (mkView_ () consumerAddr)
          addrConsumerStorage <- getStorage consumerAddr
          assert (addrConsumerStorage == [toAddress operator]) "Unexpected address consumer storage"
  ]

test_get_meta :: TestTree
test_get_meta =
  testContract "Can get TZBTC metadata" $
    \originateContract _ -> do
      admin <- refillable $ newAddress "admin"
      v1 <- originateContract $ toAddress admin
      consumer <- originate "consumer" [] contractConsumer
      transfer v1 $ calling def $ fromFlatParameter $ GetTokenMetadata (mkView_ [singleTokenTokenId] consumer)
      consumerStorage <- getStorage consumer
      assert (consumerStorage == [[defaultTZBTCMetadata]]) "Unexpected metadata"

test_documentation :: [TestTree]
test_documentation =
  [ testGroup "V1" $
      runDocTests testLorentzDoc V1.tzbtcDoc
  , testGroup "V2" $
      runDocTests testLorentzDoc V2.tzbtcDoc
  ]
