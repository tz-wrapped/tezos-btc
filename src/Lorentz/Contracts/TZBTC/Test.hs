{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Lorentz.Contracts.TZBTC.Test
  ( smokeTests
  ) where

import Data.Typeable (cast)
import qualified Data.Text as T
import System.Environment (setEnv)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Lorentz (TAddress(..), View(..), arg, mkView)
import Lorentz.Contracts.Metadata
import Lorentz.Contracts.Upgradeable.Common (EpwUpgradeParameters(..), emptyPermanentImpl)
import Lorentz.Test (contractConsumer)
import Lorentz.UStore.Migration
import Michelson.Typed.Haskell.Value
import Michelson.Untyped.EntryPoints
import Morley.Nettest
import Morley.Nettest.Abstract
import qualified Morley.Nettest.Client as TezosClient
import Tezos.Address
import Util.Named

import Client.Parser (parseContractAddressFromOutput)
import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.Types as TZBTCTypes

-- Prerequisites:
-- 1. `tezos-client` program should be available or configured via env variable
--    just like required for `tzbtc-client` config.
-- 2. `tezos-client` alias `nettest` should exist with some balance.
smokeTests :: Maybe NettestClientConfig -> IO ()
smokeTests mconfig = do
  runNettestViaIntegrational $ simpleScenario True
  case mconfig of
    Nothing -> pass
    Just config -> do
      let sname n = (<> n) <$> nccScenarioName config
      runNettestClient (config { nccScenarioName = sname "_tezos_client"}) $ simpleScenario True
      runNettestTzbtcClient (config { nccScenarioName = sname "_tzbtc_client"}) $ simpleScenario False

dummyV1Parameters :: Address -> TokenMetadata -> Map Address Natural -> V1Parameters
dummyV1Parameters redeem tokenMetadata balances = V1Parameters
  { v1RedeemAddress = redeem
  , v1TokenMetadata = tokenMetadata
  , v1Balances = balances
  }

simpleScenario :: Bool -> NettestScenario
simpleScenario requireUpgrade = uncapsNettest $ do
  admin <- resolveNettestAddr -- Fetch address for alias `nettest`.

  -- Originate and upgrade
  tzbtc <- originateSimple "TZBTCContract" (mkEmptyStorageV0 admin) tzbtcContract

  -- Originate Address view callback
  addressView <- originateSimple "Address view" [] (contractConsumer @Address)

  -- Originate Natural view callback
  naturalView <- originateSimple "Natural view" [] (contractConsumer @Natural)

  -- Originate [TokenMetadata] view callback
  tokenMetadatasView <- originateSimple "[TokenMetadata] view" [] (contractConsumer @[TokenMetadata])

  let
    fromFlatParameterV1  :: FlatParameter TZBTCv1 -> Parameter TZBTCv1
    fromFlatParameterV1 = fromFlatParameter
    adminAddr = AddrResolved admin
    tzbtcAddr = AddrResolved tzbtc
    opTZBTC = dummyV1Parameters admin defaultTZBTCMetadata mempty
    upgradeParams :: OneShotUpgradeParameters TZBTCv0
    upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
      { upMigrationScripts =
        Identity $
        manualConcatMigrationScripts (migrationScripts opTZBTC)
      , upNewCode = tzbtcContractRouter
      , upNewPermCode = emptyPermanentImpl
      }
  when requireUpgrade $
    callFrom
      adminAddr
      tzbtcAddr
      DefEpName
      (fromFlatParameter $ Upgrade upgradeParams :: Parameter TZBTCv0)

  -- Add an operator
  operator <- newAddress "operator"

  -- Transfer some credits to operator for further
  -- operations.
  transfer $ TransferData
    { tdFrom = AddrResolved admin
    , tdTo = AddrResolved operator
    , tdAmount = 5000 * 1000 -- 5 XTZ
    , tdEntrypoint = DefEpName
    , tdParameter = ()
    }

  callFrom
    (AddrAlias "nettest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ AddOperator (#operator .! operator))

  -- Add another operator
  operatorToRemove <- newAddress "operator_to_remove"
  callFrom
    (AddrAlias "nettest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ AddOperator (#operator .! operatorToRemove))

  -- Mint some coins for alice
  alice <- newAddress "alice"

  callFrom
    (AddrAlias "operator_to_remove") -- use the new operator to make sure it has been added.
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ Mint (#to .! alice, #value .! 100))

  -- Remove an operator
  callFrom
    (AddrAlias "nettest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ RemoveOperator (#operator .! operatorToRemove))

  -- Set allowance
  -- Mint some coins for john
  john <- newAddress "john"

  callFrom
    (AddrAlias "operator")
    -- We use alias instead of address to let the nettest implementation
    -- to call the `tzbtc-client` program with --user override (which does not work with addresses)
    -- using the alias.
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ Mint (#to .! john, #value .! 100))

  -- Set allowance for alice to transfer from john

  callFrom
    (AddrAlias "john")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ Approve (#spender .! alice, #value .! 100))

  -- Transfer coins from john to alice by alice
  callFrom
    (AddrAlias "alice")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ Transfer (#from .! john, #to .! alice, #value .! 15))

  -- Burn some coins from john to redeem address to burn
  callFrom
    (AddrAlias "john")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ Transfer (#from .! john, #to .! admin, #value .! 7))

  -- Burn it
  callFrom
    (AddrAlias "operator")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ Burn (#value .! 7))

  -- Pause operations
  callFrom
    (AddrAlias "operator")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ Pause ())

  -- Resume operations
  callFrom
    (AddrAlias "nettest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ Unpause ())

  -- Transfer ownership
  newOwnerAddress <- newAddress "newOwner"
  callFrom
    (AddrAlias "nettest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ TransferOwnership (#newOwner .! newOwnerAddress))

  -- Accept ownership
  callFrom
    (AddrAlias "newOwner")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ AcceptOwnership ())

  -- Make an anonymous address
  guest <- newAddress "guest"

  -- Transfer some credits to guest for further
  -- operations.
  transfer $ TransferData
    { tdFrom = AddrResolved admin
    , tdTo = AddrResolved guest
    , tdAmount = 5000 * 1000 -- 5 XTZ
    , tdEntrypoint = DefEpName
    , tdParameter = ()
    }

  callFrom
    (AddrAlias "guest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ GetAllowance (mkView (#owner .! john, #spender .! alice ) (TAddress @Natural naturalView)))

  callFrom
    (AddrAlias "guest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ GetBalance (mkView (#owner .! john) (TAddress @Natural naturalView)))

  callFrom
    (AddrAlias "guest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ GetTotalSupply (mkView () (TAddress @Natural naturalView)))

  callFrom
    (AddrAlias "guest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ GetTotalMinted (mkView () (TAddress @Natural naturalView)))

  callFrom
    (AddrAlias "guest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ GetTotalBurned (mkView () (TAddress @Natural naturalView)))

  callFrom
    (AddrAlias "guest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ GetTokenMetadata (mkView [0] (TAddress @[TokenMetadata] tokenMetadatasView)))

  callFrom
    (AddrAlias "guest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ GetOwner (mkView () (TAddress @Address addressView)))

  callFrom
    (AddrAlias "guest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ GetRedeemAddress (mkView () (TAddress @Address addressView)))

runNettestTzbtcClient :: NettestClientConfig -> NettestScenario -> IO ()
runNettestTzbtcClient config scenario = do
  scenario $ nettestImplTzbtcClient config

nettestImplTzbtcClient :: NettestClientConfig -> NettestImpl IO
nettestImplTzbtcClient config@(NettestClientConfig {..}) = NettestImpl
  { niOriginateUntyped = tzbtcClientOriginate
  , niTransfer = tzbtcClientTransfer
  , ..
  }
  where
    NettestImpl {..} = TezosClient.nettestImplClient config

    tzbtcClientOriginate :: UntypedOriginateData -> IO Address
    tzbtcClientOriginate od@(UntypedOriginateData {..}) =
      if uodName == "TZBTCContract" then do
        output <- callTzbtcClient
          [ "deployTzbtcContract"
          , "--owner", toString (formatAddrOrAlias uodFrom)
          , "--redeem", toString (formatAddrOrAlias uodFrom)
          , "--user", toString (formatAddrOrAlias uodFrom)
          ]
        case parseContractAddressFromOutput output of
          Right a -> pure a
          Left err -> throwM $ TezosClient.UnexpectedClientFailure (show err)
      else niOriginateUntyped od

    tzbtcClientTransfer :: TransferData -> IO ()
    tzbtcClientTransfer td@(TransferData {..}) =
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
            _ -> niTransfer td
          _ -> niTransfer td
        Nothing -> niTransfer td
      where
        callTzbtc :: [String] -> IO ()
        callTzbtc args = void $ callTzbtcClient $
          args <> ["--user", toString (formatAddrOrAlias tdFrom)
                  , "--contract-addr", toString (formatAddrOrAlias tdTo)
                  ]

    -- Names visible to users of nettest will be prefixed with
    -- "<nettest>" and optional scenario name before being passed to
    -- 'tezos-client'.
    prefixName :: Text -> Text
    prefixName name
      | name == "nettest" = name
      | otherwise =
        T.intercalate "." $ "nettest" : catMaybes [nccScenarioName, Just name]

    formatAddrOrAlias :: AddrOrAlias -> Text
    formatAddrOrAlias = \case
      AddrResolved addr -> formatAddress addr
      AddrAlias name -> prefixName name


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
      (ExitFailure _, _, toText -> errOutput) ->
        throwM $ TezosClient.UnexpectedClientFailure errOutput
