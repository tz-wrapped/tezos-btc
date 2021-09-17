{- SPDX-FileCopyrightText: 2021 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Test.Smoke
  ( test_smokeTests

  , TestUpgrade(..)
  , testUpgradeToV1
  , testUpgradeToV2
  ) where

import Data.Tagged (Tagged(Tagged))
import System.Environment (setEnv)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Providers (testPassed, IsTest(..), singleTest)

import Lorentz (TrustEpName(..), View(..), arg, mkView, toAddress, toMutez)
import Lorentz.Contracts.Metadata
import Lorentz.Contracts.Upgradeable.Common (EpwUpgradeParameters(..), emptyPermanentImpl)
import Lorentz.Test (contractConsumer)
import Lorentz.UStore.Migration
import Michelson.Typed.Haskell.Value
import Michelson.Untyped.Entrypoints
import Morley.Client (AddressOrAlias(..), disableAlphanetWarning, runMorleyClientM)
import qualified Morley.Client.TezosClient as TezosClient
import Morley.Nettest
import Morley.Nettest.Abstract
import Morley.Nettest.Client (ClientM(..))
import qualified Morley.Nettest.Client as TezosClient
import Morley.Nettest.Tasty.Options (nettestOptions)
import Morley.Nettest.Tasty
import Tezos.Address
import Tezos.Crypto (toPublic)
import Util.Named
import Util.Sing (castSing)

import Client.Parser (parseContractAddressFromOutput)
import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.V1.Types as TZBTCTypes

test_smokeTests :: TestTree
test_smokeTests = testGroup "Smoke tests for tzbtc contract"
  [ nettestScenarioCaps "Smoke test V1" $ simpleScenario testUpgradeToV1
  , nettestScenarioCaps "Smoke test V2" $ simpleScenario testUpgradeToV2
  , nettestScenarioOnNetworkViaTzbtcClient "Smoke test V1 deployed by tzbtc-client" $
    uncapsNettest $ simpleScenario noTestUpgrade
  ]

newtype RunOnNetworkViaTzbtcClient = RunOnNetworkViaTzbtcClient (NettestScenario ClientM)

instance IsTest RunOnNetworkViaTzbtcClient where
  run opts (RunOnNetworkViaTzbtcClient scenario) _ = do
    let tastyEnv = tastyEnvFromOpts opts
    useNettestEnv tastyEnv $ \nettestEnv ->
      (runNettestTzbtcClient nettestEnv scenario $> testPassed "")
  testOptions = Tagged nettestOptions

-- | Helper to run nettest scenario only on real network, because there is no
-- sense in running scenarios, that test @tzbtc-client@ on emulated environment
nettestScenarioOnNetworkViaTzbtcClient :: TestName -> NettestScenario ClientM -> TestTree
nettestScenarioOnNetworkViaTzbtcClient testName scenario =
  testGroup testName [singleTest "On network" (RunOnNetworkViaTzbtcClient scenario)]

dummyV1Parameters :: Address -> TokenMetadata -> Map Address Natural -> V1Parameters
dummyV1Parameters redeem tokenMetadata balances = V1Parameters
  { v1RedeemAddress = redeem
  , v1TokenMetadata = tokenMetadata
  , v1Balances = balances
  }

dummyV2Parameters :: Address -> TokenMetadata -> Map Address Natural -> V2Parameters
dummyV2Parameters = dummyV1Parameters

newtype TestUpgrade m = TestUpgrade
  { unTestUpgrade
      :: Address  --- ^ Admin's address
      -> Address --- ^ Redeem address
      -> Map Address Natural --- ^ Initial balances
      -> ContractHandler (Parameter TZBTCv0) (Storage TZBTCv0)
      -> m ()
  }

instance Applicative m => Semigroup (TestUpgrade m) where
  TestUpgrade f1 <> TestUpgrade f2 =
    TestUpgrade $ \a b -> f1 a b *> f2 a b

instance Applicative m => Monoid (TestUpgrade m) where
  mempty = TestUpgrade $ \_ _ _ _ -> pass

noTestUpgrade :: MonadNettest caps base m => TestUpgrade m
noTestUpgrade = TestUpgrade $ \_ _ _ _ -> pass

testUpgradeToV1 :: MonadNettest caps base m => TestUpgrade m
testUpgradeToV1 = TestUpgrade $ \admin redeem balances tzbtc -> do
  let
    opTZBTC = dummyV1Parameters redeem defaultTZBTCMetadata balances
    upgradeParams :: OneShotUpgradeParameters TZBTCv0
    upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
      { upMigrationScripts =
        Identity $
        manualConcatMigrationScripts (migrationScriptsV1 opTZBTC)
      , upNewCode = tzbtcContractRouterV1
      , upNewPermCode = emptyPermanentImpl
      }
  transferMoney admin $ toMutez 50 * 1000000 -- 50 XTZ
  withSender admin $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameter $ Upgrade upgradeParams :: Parameter TZBTCv0)

testUpgradeToV2 :: MonadNettest caps base m => TestUpgrade m
testUpgradeToV2 = TestUpgrade $ \admin redeem balances tzbtc -> do
  let
    opTZBTC = dummyV2Parameters redeem defaultTZBTCMetadata balances
    upgradeParams :: OneShotUpgradeParameters TZBTCv0
    upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
      { upMigrationScripts =
        Identity $
        manualConcatMigrationScripts (migrationScriptsV2 opTZBTC)
      , upNewCode = tzbtcContractRouterV2
      , upNewPermCode = emptyPermanentImpl
      }
  transferMoney admin $ toMutez 50 * 1000000 -- 50 XTZ
  withSender admin $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameter $ Upgrade upgradeParams :: Parameter TZBTCv0)

simpleScenario
  :: MonadNettest caps base m
  => TestUpgrade m -> m ()
simpleScenario upg = do
  -- admin <- resolveNettestAddress -- Fetch address for alias `nettest`.
  admin <- newAddress "admin"
  transferMoney admin $ toMutez 15 * 1000000
  -- Originate and upgrade
  -- sender is made an admin in the tzbtc-client based implementation
  tzbtc <- withSender admin $ originateSimple "TZBTCContract" (mkEmptyStorageV0 admin) tzbtcContract

  -- Originate Address view callback
  addressView <- originateSimple "Address view" [] (contractConsumer @Address)

  -- Originate Natural view callback
  naturalView <- originateSimple "Natural view" [] (contractConsumer @Natural)

  -- Originate [TokenMetadata] view callback
  tokenMetadatasView <- originateSimple "[TokenMetadata] view" [] (contractConsumer @[TokenMetadata])

  -- Run upgrade
  unTestUpgrade upg admin admin mempty tzbtc

  let
    fromFlatParameterV1  :: FlatParameter SomeTZBTCVersion -> Parameter SomeTZBTCVersion
    fromFlatParameterV1 = fromFlatParameter

  -- Add an operator
  operator <- newAddress "operator"
  operatorToRemove <- newAddress "operator_to_remove"

  withSender admin $ do
    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ AddOperator (#operator .! operator))

  -- Add another operator
    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ AddOperator (#operator .! operatorToRemove))

  -- Mint some coins for alice
  alice <- newAddress "alice"

  -- use the new operator to make sure it has been added.
  withSender operatorToRemove $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ Mint (#to .! alice, #value .! 100))

  -- Remove an operator
  withSender admin $
    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ RemoveOperator (#operator .! operatorToRemove))

  -- Set allowance
  -- Mint some coins for john
  john <- newAddress "john"

  withSender operator $ call
    -- We use alias instead of address to let the nettest implementation
    -- to call the `tzbtc-client` program with --user override (which does not work with addresses)
    -- using the alias.
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ Mint (#to .! john, #value .! 100))

  -- Set allowance for alice to transfer from john

  withSender john $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ Approve (#spender .! alice, #value .! 100))

  -- Transfer coins from john to alice by alice
  withSender alice $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ Transfer (#from .! john, #to .! alice, #value .! 15))

  -- Burn some coins from john to redeem address to burn
  withSender john $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ Transfer (#from .! john, #to .! admin, #value .! 7))

  -- Burn it
  withSender operator $ do
    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ Burn (#value .! 7))

  -- Pause operations
    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ Pause ())

  -- Resume operations
  newOwner <- newAddress "newOwner"
  withSender admin $ do
    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ Unpause ())

    -- Transfer ownership
    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ TransferOwnership (#newOwner .! newOwner))

  -- Accept ownership
  withSender newOwner $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ AcceptOwnership ())

  -- Make an anonymous address
  guest <- newAddress "guest"

  withSender guest $ do
    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ GetAllowance (mkView (#owner .! john, #spender .! alice) naturalView))

    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ GetBalance (mkView (#owner .! john) naturalView))

    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ GetTotalSupply (mkView () naturalView))

    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ GetTotalMinted (mkView () naturalView))

    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ GetTotalBurned (mkView () naturalView))

    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ GetTokenMetadata (mkView [singleTokenTokenId] tokenMetadatasView))

    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ GetOwner (mkView () addressView))

    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ GetRedeemAddress (mkView () addressView))

runNettestTzbtcClient :: NettestEnv -> NettestScenario ClientM -> IO ()
runNettestTzbtcClient (NettestEnv env envKey) scenario = do
  disableAlphanetWarning
  storageAddress <- runMorleyClientM env $
    TezosClient.resolveAddressMaybe (AddressAlias nettestAddressAlias)
  nettestAddr <- case (envKey, storageAddress) of
    (Nothing, Just addr) -> pure addr
    (Nothing, Nothing) -> throwM TezosClient.NoNettestAddress
    (Just ek, Just sa)
      | mkKeyAddress (toPublic ek) == sa -> pure sa
      | otherwise -> throwM $ TezosClient.TwoNettestKeys ek
    (Just ek, Nothing) -> do
      runMorleyClientM env (TezosClient.importKey False "nettest" ek)
      return $ mkKeyAddress (toPublic ek)
  defaultAliasCounter <- newIORef $ DefaultAliasCounter 0
  flip runReaderT defaultAliasCounter $ unClientM $
    scenario (nettestImplTzbtcClient env) nettestAddr

nettestImplTzbtcClient :: MorleyClientEnv -> NettestImpl ClientM
nettestImplTzbtcClient env = impl
  { niOpsImpl = \sender -> NettestOpsImpl
    { noiRunOperationBatch = \case
      [op] -> case op of
        OriginateOp oud -> tzbtcClientOriginate sender oud
        TransferOp td -> tzbtcClientTransfer sender td >> pure [TransferResult]
      _ -> error "Batch operations are not supported"
    }
  }
  where
    impl = (TezosClient.nettestImplClient env)
    tezosClientEnv = mceTezosClient env
    tzbtcClientEnvArgs =
      [ "-E", TezosClient.toCmdArg $ tceEndpointUrl tezosClientEnv
      ] <> (maybe [] (\dataDir -> ["-d", dataDir]) $ tceMbTezosClientDataDir tezosClientEnv)

    tzbtcClientOriginate :: Sender -> UntypedOriginateData -> ClientM [BaseOperationResult]
    tzbtcClientOriginate sender od@(UntypedOriginateData {..}) =
      if TezosClient.unAliasHint uodName == "TZBTCContract" then do
        let senderAddr = unSender sender
        output <- liftIO $ callTzbtcClient $ tzbtcClientEnvArgs <>
          [ "deployTzbtcContract"
          , "--owner", TezosClient.toCmdArg senderAddr
          , "--redeem", TezosClient.toCmdArg senderAddr
          , "--user", TezosClient.toCmdArg senderAddr
          ]
        case parseContractAddressFromOutput output of
          Right a -> pure [OriginateResult a]
          Left err -> throwM $ TezosClient.UnexpectedClientFailure 1 "" (show err)
      else (noiRunOperationBatch (niOpsImpl impl sender)) [OriginateOp od]

    tzbtcClientTransfer :: Sender -> TransferData -> ClientM ()
    tzbtcClientTransfer sender td@(TransferData {..}) = do
      -- If we had a Typeable constraint for `v` in definition of
      -- `TransferData`, we could save this use of toVal/fromVal conversion and
      -- use `tdParameter` directly.
      case castSing (toVal tdParameter) of
        Just srcVal -> case (fromVal srcVal :: TZBTCTypes.Parameter SomeTZBTCVersion) of
          TZBTCTypes.GetTotalSupply (viewCallbackTo -> (crAddress -> view_)) ->
            callTzbtc
              [ "getTotalSupply"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.GetTotalMinted (viewCallbackTo -> (crAddress -> view_)) ->
            callTzbtc
              [ "getTotalMinted"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.GetTotalBurned (viewCallbackTo -> (crAddress -> view_)) ->
            callTzbtc
              [ "getTotalBurned"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.GetAllowance
            (View (arg #owner -> owner, arg #spender -> spender) (crAddress -> view_)) ->
              callTzbtc
                [ "getAllowance"
                , "--owner", toString (formatAddress owner)
                , "--spender", toString (formatAddress spender)
                , "--callback", toString (formatAddress view_)
                ]

          TZBTCTypes.GetOwner (viewCallbackTo -> (crAddress -> view_)) ->
            callTzbtc
              [ "getOwner"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.GetRedeemAddress (viewCallbackTo -> (crAddress -> view_)) ->
            callTzbtc
              [ "getRedeemAddress"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.GetTokenMetadata (viewCallbackTo -> (crAddress -> view_)) ->
            callTzbtc
              [ "getTokenMetadata"
              , "--callback", toString (formatAddress view_)
              ]

          TZBTCTypes.SafeEntrypoints sp -> case sp of
            TZBTCTypes.Transfer (arg #from -> from, arg #to -> to, arg #value -> value) ->
              callTzbtc
                [ "transfer"
                , "--from", toString $ formatAddress from
                , "--to", toString $ formatAddress to
                , "--value", show value
                ]

            TZBTCTypes.Approve (arg #spender -> spender, arg #value -> value) ->
              callTzbtc
                [ "approve"
                , "--spender", toString $ formatAddress spender
                , "--value", show value
                ]

            TZBTCTypes.Mint (arg #to -> to, arg #value -> value) ->
              callTzbtc
                [ "mint"
                , "--to", toString $ formatAddress to
                , "--value", show value
                ]

            TZBTCTypes.Burn (arg #value -> value) -> callTzbtc [ "burn" , "--value", show value ]
            TZBTCTypes.AddOperator (arg #operator -> operator) ->
              callTzbtc [ "addOperator" , "--operator", toString $ formatAddress operator ]
            TZBTCTypes.RemoveOperator (arg #operator -> operator) ->
              callTzbtc [ "removeOperator" , "--operator", toString $ formatAddress operator ]
            TZBTCTypes.Pause _  -> callTzbtc $ [ "pause" ]
            TZBTCTypes.Unpause _  -> callTzbtc $ [ "unpause" ]
            TZBTCTypes.TransferOwnership (arg #newOwner -> newOwnerAddress) ->
              callTzbtc [ "transferOwnership" , toString $ formatAddress newOwnerAddress ]
            TZBTCTypes.AcceptOwnership _ -> callTzbtc $ [ "acceptOwnership" ]
            _ -> callMorleyClient
          _ -> callMorleyClient
        Nothing -> callMorleyClient
      where
        callMorleyClient = (noiRunOperationBatch (niOpsImpl impl sender)) [TransferOp td] >> pass
        callTzbtc :: [String] -> ClientM ()
        callTzbtc args = void $ liftIO $ callTzbtcClient $
          tzbtcClientEnvArgs <> args <> ["--user", TezosClient.toCmdArg (unSender sender)
                                        , "--contract-addr", TezosClient.toCmdArg $ toAddress tdTo
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
