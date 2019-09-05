{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.Proxy
  ( test_proxy
  ) where

import Fmt (pretty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertBool, assertFailure, testCase)
import qualified Data.Map as Map
import Data.Set
import Data.Singletons (SingI(..))
import qualified Data.Set as Set
import Named (arg)

import Lorentz
import Lorentz.Contracts.Consumer
import Lorentz.Contracts.TZBTC
import qualified Lorentz.Contracts.TZBTC.Agent as Agent
import Lorentz.Contracts.TZBTC.Types
import Lorentz.Test.Integrational
import Michelson.Interpret (ContractEnv(..), MichelsonFailed(..))
import Michelson.Test (ContractPropValidator, contractProp, dummyContractEnv, IntegrationalScenarioM)
import Michelson.Text (mt)
import Michelson.Typed (Instr, InstrWrapC, AppendCtorField, GetCtorField, ToTs, Value, Value'(..))
import Michelson.Typed.Scope (checkOpPresence, OpPresence(..))
import Util.Named

import qualified Lorentz.Contracts.TZBTC.Proxy as Proxy
import qualified Lorentz.Contracts.TZBTC as TZBTC

originateContract :: IntegrationalScenarioM (ContractAddr Parameter)
originateContract =
  lOriginate tzbtcContract "UserUpgradeable V1" storage (toMutez 1000)

adminAddress :: Address
adminAddress = genesisAddress3

redeemAddress_ :: Address
redeemAddress_ = adminAddress

initialSupply :: Natural
initialSupply = 500

storage :: Storage
storage =
  mkStorage adminAddress redeemAddress_
    (Map.fromList [(redeemAddress_, initialSupply)]) mempty

originateProxy :: ContractAddr TZBTC.Parameter -> IntegrationalScenarioM (ContractAddr Proxy.Parameter)
originateProxy targetAddress =
  lOriginate Proxy.tzbtcProxyContract "TZBTC Proxy" targetAddress (toMutez 1000)

test_proxy :: TestTree
test_proxy = testGroup "TZBTC contract proxy origination test"
  [ testCase
      "Proxy saves the address of target contract" $
        integrationalTestExpectation $ do
          c <- originateContract
          proxy <- originateProxy c
          validate . Right $
            lExpectStorageConst proxy (unContractAddress c)
  ]

