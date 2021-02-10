{- SPDX-FileCopyrightText: 2021 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Test.Smoke
  ( test_smokeTests
  ) where

import Data.Tagged (Tagged(Tagged))
import Data.Typeable (cast)
import System.Environment (setEnv)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Providers (testPassed, IsTest(..), singleTest)

import Lorentz (TAddress, TrustEpName(..), View(..), arg, mkView, toMutez)
import Lorentz.Contracts.Metadata
import Lorentz.Contracts.Upgradeable.Common (EpwUpgradeParameters(..), emptyPermanentImpl)
import Lorentz.Test (contractConsumer)
import Lorentz.UStore.Migration
import Michelson.Typed.Haskell.Value
import Michelson.Untyped.Entrypoints
import qualified Morley.Client.TezosClient as TezosClient
import Morley.Nettest
import qualified Morley.Nettest.Client as TezosClient
import Morley.Nettest.Abstract
import Morley.Nettest.Tasty.Options (nettestOptions)
import Morley.Nettest.Tasty
import Tezos.Address
import Util.Named

import Client.Parser (parseContractAddressFromOutput)
import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.V1.Types as TZBTCTypes

test_smokeTests :: TestTree
test_smokeTests = testGroup "Smoke tests for tzbtc contract"
  [ nettestScenario "Smoke test V1" $ simpleScenario testUpgradeToV1
  , nettestScenario "Smoke test V2" $ simpleScenario testUpgradeToV2
  , nettestScenarioOnNetworkViaTzbtcClient "Smote test V1 deployed by tzbtc-client" $
    simpleScenario noTestUpgrade
  ]

newtype RunOnNetworkViaTzbtcClient = RunOnNetworkViaTzbtcClient (NettestScenario IO)

instance IsTest RunOnNetworkViaTzbtcClient where
  run opts (RunOnNetworkViaTzbtcClient scenario) _ =
    case tastyEnvFromOpts opts of
      Nothing -> pure testSkipped
      Just tastyEnv ->
        useNettestEnv tastyEnv $ \nettestEnv ->
          (runNettestTzbtcClient nettestEnv scenario $> testPassed "")
  testOptions = Tagged nettestOptions

-- | Helper to run nettest scenario only on real network, because there is no
-- sense in running scenarios, that test @tzbtc-client@ on emulated environment
nettestScenarioOnNetworkViaTzbtcClient :: TestName -> NettestScenario IO -> TestTree
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
      -> TAddress (Parameter TZBTCv0)
      -> NettestT m ()
  }

instance Applicative m => Semigroup (TestUpgrade m) where
  TestUpgrade f1 <> TestUpgrade f2 =
    TestUpgrade $ \a b -> f1 a b *> f2 a b

instance Applicative m => Monoid (TestUpgrade m) where
  mempty = TestUpgrade $ \_ _ -> pass

noTestUpgrade :: Monad m => TestUpgrade m
noTestUpgrade = TestUpgrade $ \_ _ -> pass

testUpgradeToV1 :: Monad m => TestUpgrade m
testUpgradeToV1 = TestUpgrade $ \admin tzbtc -> do
  let
    opTZBTC = dummyV1Parameters admin defaultTZBTCMetadata mempty
    upgradeParams :: OneShotUpgradeParameters TZBTCv0
    upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
      { upMigrationScripts =
        Identity $
        manualConcatMigrationScripts (migrationScriptsV1 opTZBTC)
      , upNewCode = tzbtcContractRouterV1
      , upNewPermCode = emptyPermanentImpl
      }
  withSender (AddressResolved admin) $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameter $ Upgrade upgradeParams :: Parameter TZBTCv0)

testUpgradeToV2 :: Monad m => TestUpgrade m
testUpgradeToV2 = TestUpgrade $ \admin tzbtc -> do
  let
    opTZBTC = dummyV2Parameters admin defaultTZBTCMetadata mempty
    upgradeParams :: OneShotUpgradeParameters TZBTCv0
    upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
      { upMigrationScripts =
        Identity $
        manualConcatMigrationScripts (migrationScriptsV2 opTZBTC)
      , upNewCode = tzbtcContractRouterV2
      , upNewPermCode = emptyPermanentImpl
      }
  withSender (AddressResolved admin) $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameter $ Upgrade upgradeParams :: Parameter TZBTCv0)

simpleScenario :: TestUpgrade m -> NettestScenario m
simpleScenario upg = uncapsNettest $ do
  admin <- resolveNettestAddress -- Fetch address for alias `nettest`.

  -- Originate and upgrade
  tzbtc <- originateSimple "TZBTCContract" (mkEmptyStorageV0 admin) tzbtcContract

  -- Originate Address view callback
  addressView <- originateSimple "Address view" [] (contractConsumer @Address)

  -- Originate Natural view callback
  naturalView <- originateSimple "Natural view" [] (contractConsumer @Natural)

  -- Originate [TokenMetadata] view callback
  tokenMetadatasView <- originateSimple "[TokenMetadata] view" [] (contractConsumer @[TokenMetadata])

  -- Run upgrade
  unTestUpgrade upg admin tzbtc

  let
    fromFlatParameterV1  :: FlatParameter SomeTZBTCVersion -> Parameter SomeTZBTCVersion
    fromFlatParameterV1 = fromFlatParameter

  -- Add an operator
  (operator, operatorAddr) <- newAddress' "operator"

  -- Transfer some credits to operator for further
  -- operations.
  withSender (AddressResolved admin) $ transfer $ TransferData
    { tdTo = operator
    , tdAmount = toMutez $ 5000 * 1000 -- 5 XTZ
    , tdEntrypoint = DefEpName
    , tdParameter = ()
    }

  call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ AddOperator (#operator .! operatorAddr))

  -- Add another operator
  (operatorToRemove, operatorToRemoveAddr) <- newAddress' "operator_to_remove"
  call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ AddOperator (#operator .! operatorToRemoveAddr))

  -- Mint some coins for alice
  (alice, aliceAddr) <- newAddress' "alice"

  -- use the new operator to make sure it has been added.
  withSender operatorToRemove $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ Mint (#to .! aliceAddr, #value .! 100))

  -- Remove an operator
  call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ RemoveOperator (#operator .! operatorToRemoveAddr))

  -- Set allowance
  -- Mint some coins for john
  (john, johnAddr) <- newAddress' "john"

  withSender operator $ call
    -- We use alias instead of address to let the nettest implementation
    -- to call the `tzbtc-client` program with --user override (which does not work with addresses)
    -- using the alias.
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ Mint (#to .! johnAddr , #value .! 100))

  -- Set allowance for alice to transfer from john

  withSender john $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ Approve (#spender .! aliceAddr, #value .! 100))

  -- Transfer coins from john to alice by alice
  withSender alice $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ Transfer (#from .! johnAddr, #to .! aliceAddr, #value .! 15))

  -- Burn some coins from john to redeem address to burn
  withSender john $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ Transfer (#from .! johnAddr, #to .! admin, #value .! 7))

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
  call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ Unpause ())

  -- Transfer ownership
  (newOwner, newOwnerAddr) <- newAddress' "newOwner"
  call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ TransferOwnership (#newOwner .! newOwnerAddr))

  -- Accept ownership
  withSender newOwner $ call
    tzbtc
    (TrustEpName DefEpName)
    (fromFlatParameterV1 $ AcceptOwnership ())

  -- Make an anonymous address
  (guest, _) <- newAddress' "guest"

  -- Transfer some credits to guest for further
  -- operations.
  withSender (AddressResolved admin) $ transfer $ TransferData
    { tdTo = guest
    , tdAmount = toMutez $ 5000 * 1000 -- 5 XTZ
    , tdEntrypoint = DefEpName
    , tdParameter = ()
    }

  withSender guest $ do
    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ GetAllowance (mkView (#owner .! johnAddr, #spender .! aliceAddr) naturalView))

    call
      tzbtc
      (TrustEpName DefEpName)
      (fromFlatParameterV1 $ GetBalance (mkView (#owner .! johnAddr) naturalView))

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

-- | This is a version of 'newAddress' that returns both an address and an alias.
--
-- This is necessary, because tezos-client key revelaing does not work on
-- addresses somehow.
newAddress' :: MonadNettest caps base m => TezosClient.AliasHint -> m (AddressOrAlias, Address)
newAddress' alias = do
  addr <- newAddress alias
  prefixedAlias <- getAlias (AddressResolved addr)
  return (AddressAlias prefixedAlias, addr)

runNettestTzbtcClient :: NettestEnv -> NettestScenario IO -> IO ()
runNettestTzbtcClient env scenario = do
  scenario $ nettestImplTzbtcClient env

nettestImplTzbtcClient :: NettestEnv -> NettestImpl IO
nettestImplTzbtcClient (NettestEnv env _) = NettestImpl
  { niOriginateUntyped = tzbtcClientOriginate
  , niTransferBatchFrom = \sender -> \case
      [td] -> tzbtcClientTransfer sender td
      _ -> error "Batch transfers are not supported"
  , ..
  }
  where
    impl@NettestImpl {..} = TezosClient.nettestImplClient env
    tezosClientEnv = mceTezosClient env
    tzbtcClientEnvArgs =
      [ "-E", TezosClient.toCmdArg $ tceEndpointUrl tezosClientEnv
      ] <> (maybe [] (\dataDir -> ["-d", dataDir]) $ tceMbTezosClientDataDir tezosClientEnv)

    tzbtcClientOriginate :: Sender -> UntypedOriginateData -> IO Address
    tzbtcClientOriginate sender od@(UntypedOriginateData {..}) =
      if TezosClient.unAliasHint uodName == "TZBTCContract" then do
        let senderAddr = unSender sender
        output <- callTzbtcClient $ tzbtcClientEnvArgs <>
          [ "deployTzbtcContract"
          , "--owner", TezosClient.toCmdArg senderAddr
          , "--redeem", TezosClient.toCmdArg senderAddr
          , "--user", TezosClient.toCmdArg senderAddr
          ]
        case parseContractAddressFromOutput output of
          Right a -> pure a
          Left err -> throwM $ TezosClient.UnexpectedClientFailure 1 "" (show err)
      else niOriginateUntyped sender od

    tzbtcClientTransfer :: Sender -> TransferData -> IO ()
    tzbtcClientTransfer sender td@(TransferData {..}) =
      -- If we had a Typeable constraint for `v` in definition of
      -- `TransferData`, we could save this use of toVal/fromVal conversion and
      -- use `tdParameter` directly.
      case cast (toVal tdParameter) of
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
            _ -> niTransfer impl sender td
          _ -> niTransfer impl sender td
        Nothing -> niTransfer impl sender td
      where
        callTzbtc :: [String] -> IO ()
        callTzbtc args = void $ callTzbtcClient $
          tzbtcClientEnvArgs <> args <> ["--user", TezosClient.toCmdArg (unSender sender)
                                        , "--contract-addr", TezosClient.toCmdArg tdTo
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
