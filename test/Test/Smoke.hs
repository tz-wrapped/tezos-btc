{- SPDX-FileCopyrightText: 2021 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Smoke
  ( test_smokeTests

  , TestUpgrade(..)
  , testUpgradeToV1
  , testUpgradeToV2
  ) where

import Debug qualified (show)

import Data.Set as Set
import Data.Tagged (Tagged(Tagged))
import System.Environment (setEnv)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Providers (IsTest(..), singleTest, testPassed)

import Lorentz (View_(..), mkView_)
import Lorentz.Contracts.Metadata
import Lorentz.Contracts.Upgradeable.Common (EpwUpgradeParameters(..), emptyPermanentImplCompat)
import Lorentz.UStore.Migration
import Morley.Client (OperationInfo(..), disableAlphanetWarning)
import Morley.Client.TezosClient qualified as TezosClient
import Morley.Michelson.Typed.Haskell.Value
import Morley.Tezos.Address
import Morley.Tezos.Core
import Morley.Util.Named
import Morley.Util.Sing (castSing)
import Test.Cleveland
import Test.Cleveland.Internal.Abstract
import Test.Cleveland.Internal.Client
import Test.Cleveland.Internal.Scenario
import Test.Cleveland.Lorentz (contractConsumer)
import Test.Cleveland.Tasty.Internal
import Test.Cleveland.Tasty.Internal.Options (clevelandOptions)

import Client.Parser (parseContractAddressFromOutput)
import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.V1.Types qualified as TZBTCTypes

test_smokeTests :: TestTree
test_smokeTests = testGroup "Smoke tests for tzbtc contract"
  [ testScenario "Smoke test V1" $ scenario $ simpleScenario testUpgradeToV1
  , testScenario "Smoke test V2" $ scenario $ simpleScenario testUpgradeToV2
  , nettestScenarioOnNetworkViaTzbtcClient "Smoke test V1 deployed by tzbtc-client" $
    scenario $ simpleScenario noTestUpgrade
  ]

newtype RunOnNetworkViaTzbtcClient = RunOnNetworkViaTzbtcClient (Scenario ClientM)

instance IsTest RunOnNetworkViaTzbtcClient where
  run opts (RunOnNetworkViaTzbtcClient scenario') _ = do
    let tastyEnv = tastyEnvFromOpts opts
    useNetworkEnv tastyEnv $ \nettestEnv -> case scenario' of
      ScenarioNetwork s -> runNettestTzbtcClient nettestEnv s $> testPassed ""
  testOptions = Tagged clevelandOptions

-- | Helper to run nettest scenario only on real network, because there is no
-- sense in running scenarios, that test @tzbtc-client@ on emulated environment
nettestScenarioOnNetworkViaTzbtcClient :: TestName -> Scenario ClientM -> TestTree
nettestScenarioOnNetworkViaTzbtcClient testName scenario' =
  testGroup testName [singleTest "On network" (RunOnNetworkViaTzbtcClient scenario')]

dummyV1Parameters :: ImplicitAddress -> TokenMetadata -> Map Address Natural -> V1Parameters
dummyV1Parameters redeem tokenMetadata balances = V1Parameters
  { v1RedeemAddress = toAddress redeem
  , v1TokenMetadata = tokenMetadata
  , v1Balances = balances
  }

dummyV2Parameters :: ImplicitAddress -> TokenMetadata -> Map Address Natural -> V2Parameters
dummyV2Parameters = dummyV1Parameters

newtype TestUpgrade m = TestUpgrade
  { unTestUpgrade
      :: ImplicitAddress  --- ^ Admin's address
      -> ImplicitAddress --- ^ Redeem address
      -> Map Address Natural --- ^ Initial balances
      -> ContractHandle (Parameter TZBTCv0) (Storage TZBTCv0) ()
      -> m ()
  }

instance Applicative m => Semigroup (TestUpgrade m) where
  TestUpgrade f1 <> TestUpgrade f2 =
    TestUpgrade $ \a b -> f1 a b *> f2 a b

instance Applicative m => Monoid (TestUpgrade m) where
  mempty = TestUpgrade $ \_ _ _ _ -> pass

noTestUpgrade :: MonadCleveland caps m => TestUpgrade m
noTestUpgrade = TestUpgrade $ \_ _ _ _ -> pass

testUpgradeToV1 :: MonadCleveland caps m => TestUpgrade m
testUpgradeToV1 = TestUpgrade $ \admin redeem balances tzbtc -> do
  let
    opTZBTC = dummyV1Parameters redeem defaultTZBTCMetadata balances
    upgradeParams :: OneShotUpgradeParameters TZBTCv0
    upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
      { upMigrationScripts =
        Identity $
        manualConcatMigrationScripts (migrationScriptsV1 opTZBTC)
      , upNewCode = tzbtcContractRouterV1
      , upNewPermCode = emptyPermanentImplCompat
      }
  transfer admin [tz|50|] -- 50 XTZ
  withSender admin $ transfer tzbtc $ calling def
    (fromFlatParameter $ Upgrade upgradeParams :: Parameter TZBTCv0)

testUpgradeToV2 :: MonadCleveland caps m => TestUpgrade m
testUpgradeToV2 = TestUpgrade $ \admin redeem balances tzbtc -> do
  let
    opTZBTC = dummyV2Parameters redeem defaultTZBTCMetadata balances
    upgradeParams :: OneShotUpgradeParameters TZBTCv0
    upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
      { upMigrationScripts =
        Identity $
        manualConcatMigrationScripts (migrationScriptsV2 opTZBTC)
      , upNewCode = tzbtcContractRouterV2
      , upNewPermCode = emptyPermanentImplCompat
      }
  transfer admin [tz|50|] -- 50 XTZ
  withSender admin $ transfer tzbtc $ unsafeCalling def
    (fromFlatParameter $ Upgrade upgradeParams :: Parameter TZBTCv0)

simpleScenario
  :: MonadCleveland caps m
  => TestUpgrade m -> m ()
simpleScenario upg = do
  -- admin <- resolveNettestAddress -- Fetch address for alias `nettest`.
  admin <- newAddress "admin"
  transfer admin [tz|15|]
  -- Originate and upgrade
  -- sender is made an admin in the tzbtc-client based implementation
  tzbtc <- withSender admin $
    originate "TZBTCContract" (mkEmptyStorageV0 $ toL1Address admin) tzbtcContract

  -- Originate Address view callback
  addressView <- originate "Address view" [] (contractConsumer @Address)

  -- Originate Natural view callback
  naturalView <- originate "Natural view" [] (contractConsumer @Natural)

  -- Originate [TokenMetadata] view callback
  tokenMetadatasView <- originate "[TokenMetadata] view" [] (contractConsumer @[TokenMetadata])

  -- Run upgrade
  unTestUpgrade upg admin admin mempty tzbtc

  let
    fromFlatParameterV1  :: FlatParameter SomeTZBTCVersion -> Parameter SomeTZBTCVersion
    fromFlatParameterV1 = fromFlatParameter

  -- Add an operator
  operator <- newAddress "operator"
  operatorToRemove <- newAddress "operator_to_remove"

  withSender admin $ do
    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ AddOperator (#operator :! toAddress operator))

  -- Add another operator
    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ AddOperator (#operator :! toAddress operatorToRemove))

  -- Mint some coins for alice
  alice <- newAddress "alice"

  -- use the new operator to make sure it has been added.
  withSender operatorToRemove $ transfer tzbtc $ unsafeCalling def
    (fromFlatParameterV1 $ Mint (#to :! toAddress alice, #value :! 100))

  -- Remove an operator
  withSender admin $
    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ RemoveOperator (#operator :! toAddress operatorToRemove))

  -- Set allowance
  -- Mint some coins for john
  john <- newAddress "john"

  withSender operator $ transfer tzbtc $ unsafeCalling def
    -- We use alias instead of address to let the nettest implementation
    -- to call the `tzbtc-client` program with --user override (which does not work with addresses)
    -- using the alias.
    (fromFlatParameterV1 $ Mint (#to :! toAddress john, #value :! 100))

  -- Set allowance for alice to transfer from john

  withSender john $ transfer tzbtc $ unsafeCalling def
    (fromFlatParameterV1 $ Approve (#spender :! toAddress alice, #value :! 100))

  -- Transfer coins from john to alice by alice
  withSender alice $ transfer tzbtc $ unsafeCalling def
    (fromFlatParameterV1 $ Transfer (#from :! toAddress john, #to :! toAddress alice, #value :! 15))

  -- Burn some coins from john to redeem address to burn
  withSender john $ transfer tzbtc $ unsafeCalling def
    (fromFlatParameterV1 $ Transfer (#from :! toAddress john, #to :! toAddress admin, #value :! 7))

  -- Burn it
  withSender operator $ do
    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ Burn (#value :! 7))

  -- Pause operations
    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ Pause ())

  -- Resume operations
  newOwner <- newAddress "newOwner"
  withSender admin $ do
    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ Unpause ())

    -- Transfer ownership
    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ TransferOwnership (#newOwner :! toAddress newOwner))

  -- Accept ownership
  withSender newOwner $ transfer tzbtc $ unsafeCalling def
    (fromFlatParameterV1 $ AcceptOwnership ())

  -- Make an anonymous address
  guest <- newAddress "guest"

  withSender guest $ do
    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $
        GetAllowance (mkView_ (#owner :! toAddress john, #spender :! toAddress alice) naturalView))

    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ GetBalance (mkView_ (#owner :! toAddress john) naturalView))

    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ GetTotalSupply (mkView_ () naturalView))

    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ GetTotalMinted (mkView_ () naturalView))

    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ GetTotalBurned (mkView_ () naturalView))

    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ GetTokenMetadata (mkView_ [singleTokenTokenId] tokenMetadatasView))

    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ GetOwner (mkView_ () addressView))

    transfer tzbtc $ unsafeCalling def
      (fromFlatParameterV1 $ GetRedeemAddress (mkView_ () addressView))

runNettestTzbtcClient :: NetworkEnv -> NetworkT ClientM a -> IO a
runNettestTzbtcClient env scenario' = do
  disableAlphanetWarning
  moneybagAddr <- setupMoneybagAddress env
  let caps = ClevelandCaps
        { ccSender = Sender $ unMoneybag moneybagAddr
        , ccMoneybag = moneybagAddr
        , ccMiscCap = networkMiscImpl env
        , ccOpsCap = nettestImplTzbtcClient (neMorleyClientEnv env)
        }
      netc = NetworkCaps { ncNetworkEnv = env, ncClevelandCaps = caps }
  ist <- newIORef ClientState
    { csDefaultAliasCounter = DefaultAliasCounter 0
    , csRefillableAddresses = Set.empty
    , csMoneybagAddress = moneybagAddr
    }
  let clientM = runReaderT scenario' netc
  runReaderT (unClientM clientM) ist

nettestImplTzbtcClient :: MorleyClientEnv -> Sender -> ClevelandOpsImpl ClientM
nettestImplTzbtcClient env sender =
  impl { coiRunOperationBatch = \case
      [] -> pure [] -- this happens when `newAddress` reuses addresses that have enough tez
      [op] -> case op of
        OpOriginate oud -> tzbtcClientOriginate oud
        OpTransfer td -> tzbtcClientTransfer td >> pure [OpTransfer []]
        OpReveal _ -> error "Reveal operation is not supported"
      _ -> error "Batch operations are not supported"
    }
  where
    impl = networkOpsImpl env sender

    tezosClientEnv = mceTezosClient env
    tzbtcClientEnvArgs =
      [ "-E", TezosClient.toCmdArg $ tceEndpointUrl tezosClientEnv
      ] <> (maybe [] (\dataDir -> ["-d", dataDir]) $ tceMbTezosClientDataDir tezosClientEnv)

    tzbtcClientOriginate :: UntypedOriginateData 'NotLarge -> ClientM [OperationInfo ClevelandResult]
    tzbtcClientOriginate od@(UntypedOriginateData {..}) =
      if uodName == "TZBTCContract" then do
        let senderAddr = unSender sender
        output <- liftIO $ callTzbtcClient $ tzbtcClientEnvArgs <>
          [ "deployTzbtcContract"
          , "--owner", TezosClient.toCmdArg senderAddr
          , "--redeem", TezosClient.toCmdArg senderAddr
          , "--user", TezosClient.toCmdArg senderAddr
          ]
        case parseContractAddressFromOutput output of
          Right a -> pure [OpOriginate a]
          Left err -> throwM $ TezosClient.UnexpectedClientFailure 1 "" (Debug.show err)
      else coiRunOperationBatch impl [OpOriginate od]

    tzbtcClientTransfer :: TransferData -> ClientM ()
    tzbtcClientTransfer td@(TransferData {..}) = do
      -- If we had a Typeable constraint for `v` in definition of
      -- `TransferData`, we could save this use of toVal/fromVal conversion and
      -- use `tdParameter` directly.
      case castSing (toVal tdParameter) of
        Just srcVal -> case (fromVal srcVal :: TZBTCTypes.Parameter SomeTZBTCVersion) of
          TZBTCTypes.GetTotalSupply (viewCallbackTo -> (crAddress -> MkAddress view_)) ->
            callTzbtc
              [ "getTotalSupply"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.GetTotalMinted (viewCallbackTo -> (crAddress -> MkAddress view_)) ->
            callTzbtc
              [ "getTotalMinted"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.GetTotalBurned (viewCallbackTo -> (crAddress -> MkAddress view_)) ->
            callTzbtc
              [ "getTotalBurned"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.GetAllowance
            (View_ (arg #owner -> MkAddress owner, arg #spender -> MkAddress spender)
              (crAddress -> MkAddress view_)) ->
              callTzbtc
                [ "getAllowance"
                , "--owner", toString (formatAddress owner)
                , "--spender", toString (formatAddress spender)
                , "--callback", toString (formatAddress view_)
                ]

          TZBTCTypes.GetOwner (viewCallbackTo -> (crAddress -> MkAddress view_)) ->
            callTzbtc
              [ "getOwner"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.GetRedeemAddress (viewCallbackTo -> (crAddress -> MkAddress view_)) ->
            callTzbtc
              [ "getRedeemAddress"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.GetTokenMetadata (viewCallbackTo -> (crAddress -> MkAddress view_)) ->
            callTzbtc
              [ "getTokenMetadata"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.SafeEntrypoints sp -> case sp of
            TZBTCTypes.Transfer
              (arg #from -> MkAddress from, arg #to -> MkAddress to, arg #value -> value) ->
              callTzbtc
                [ "transfer"
                , "--from", toString $ formatAddress from
                , "--to", toString $ formatAddress to
                , "--value", show value
                ]

            TZBTCTypes.Approve (arg #spender -> MkAddress spender, arg #value -> value) ->
              callTzbtc
                [ "approve"
                , "--spender", toString $ formatAddress spender
                , "--value", show value
                ]

            TZBTCTypes.Mint (arg #to -> MkAddress to, arg #value -> value) ->
              callTzbtc
                [ "mint"
                , "--to", toString $ formatAddress to
                , "--value", show value
                ]

            TZBTCTypes.Burn (arg #value -> value) -> callTzbtc [ "burn" , "--value", show value ]
            TZBTCTypes.AddOperator (arg #operator -> MkAddress operator) ->
              callTzbtc [ "addOperator" , "--operator", toString $ formatAddress operator ]
            TZBTCTypes.RemoveOperator (arg #operator -> MkAddress operator) ->
              callTzbtc [ "removeOperator" , "--operator", toString $ formatAddress operator ]
            TZBTCTypes.Pause _  -> callTzbtc $ [ "pause" ]
            TZBTCTypes.Unpause _  -> callTzbtc $ [ "unpause" ]
            TZBTCTypes.TransferOwnership (arg #newOwner -> MkAddress newOwnerAddress) ->
              callTzbtc [ "transferOwnership" , toString $ formatAddress newOwnerAddress ]
            TZBTCTypes.AcceptOwnership _ -> callTzbtc $ [ "acceptOwnership" ]
            _ -> callMorleyClient
          _ -> callMorleyClient
        Nothing -> callMorleyClient
      where
        callMorleyClient = coiRunOperationBatch impl [OpTransfer td] >> pass
        callTzbtc :: [String] -> ClientM ()
        callTzbtc args = void $ liftIO $ callTzbtcClient $
          tzbtcClientEnvArgs <> args <> ["--user", TezosClient.toCmdArg (unSender sender)
                                        , "--contract-addr", TezosClient.toCmdArg $ toAddress $ toL1Address tdTo
                                        ]

-- | Write something to stderr.
putErrLn :: Print a => a -> IO ()
putErrLn = hPutStrLn stderr

callTzbtcClient :: [String] -> IO Text
callTzbtcClient args = toText <$> do
  setEnv "TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER" "YES"
  readProcessWithExitCode "tzbtc-client" args "N" >>=
    \case
      (ExitSuccess, output, errOutput) ->
        output <$ putErrLn errOutput
      (ExitFailure code, toText -> output, toText -> errOutput) ->
        throwM $ TezosClient.UnexpectedClientFailure code output errOutput
