{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Lorentz.Contracts.TZBTC.Test
  ( smokeTests
  ) where

import Tezos.Address (Address)
import Morley.Nettest
import Michelson.Text
import Michelson.Untyped.EntryPoints
import Util.Named ((.!))
import Lorentz.Address (TAddress(..))
import Lorentz.Contracts.Upgradeable.Common (EpwUpgradeParameters(..), emptyPermanentImpl)
import Lorentz.Macro (mkView)
import Lorentz.Test (contractConsumer)
import Lorentz.UStore.Migration

import Lorentz.Contracts.TZBTC

-- Prerequisites:
-- 1. `tezos-client` program should be available or configured via env variable
--    just like required for `tzbtc-client` config
-- 2. `tezos-client` alias `nettest` should exists with some balance
smokeTests :: NettestClientConfig -> IO ()
smokeTests config = do
  runNettestViaIntegrational simpleScenario
  runNettestClient config simpleScenario

dummyTokenName :: MText
dummyTokenName = [mt|Test token|]

dummyTokenCode :: MText
dummyTokenCode = [mt|TEST|]

dummyV1Parameters :: Address -> Map Address Natural -> V1Parameters
dummyV1Parameters redeem balances = V1Parameters
  { v1RedeemAddress = redeem
  , v1Balances = balances
  , v1TokenName = dummyTokenName
  , v1TokenCode = dummyTokenCode
  }

simpleScenario :: NettestScenario
simpleScenario = uncapsNettest $ do
  admin <- resolveNettestAddr -- Fetch address for alias `nettest`.

  -- Originate and upgrade
  tzbtc <- originateSimple "TZBTCContract" (mkEmptyStorageV0 admin) tzbtcContract

  -- Originate Address view callback
  addressView <- originateSimple "Address view" [] (contractConsumer @Address)

  -- Originate Natural view callback
  naturalView <- originateSimple "Natural view" [] (contractConsumer @Natural)

  -- Originate Text view callback
  textView <- originateSimple "Text view" [] (contractConsumer @MText)

  let
    fromFlatParameterV1  :: FlatParameter TZBTCv1 -> Parameter TZBTCv1
    fromFlatParameterV1 = fromFlatParameter
    adminAddr = AddrResolved admin
    tzbtcAddr = AddrResolved tzbtc
    opTZBTC = dummyV1Parameters admin mempty
    upgradeParams :: OneShotUpgradeParameters TZBTCv0
    upgradeParams = makeOneShotUpgradeParameters @TZBTCv0 EpwUpgradeParameters
      { upMigrationScripts =
        Identity $
        manualConcatMigrationScripts (migrationScripts opTZBTC)
      , upNewCode = tzbtcContractRouter
      , upNewPermCode = emptyPermanentImpl
      }
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
    (fromFlatParameterV1 $ GetTokenName (mkView () (TAddress @MText textView)))

  callFrom
    (AddrAlias "guest")
    tzbtcAddr
    DefEpName
    (fromFlatParameterV1 $ GetTokenCode (mkView () (TAddress @MText textView)))

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
