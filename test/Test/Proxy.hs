{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Test.Proxy
  ( test_proxy
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lorentz
import Lorentz.Contracts.Consumer
import Lorentz.Contracts.TZBTC
import Lorentz.Test.Integrational
import Michelson.Test (IntegrationalScenarioM)
import Util.Named

import qualified Lorentz.Contracts.TZBTC.Proxy as Proxy
import Lorentz.Contracts.TZBTC.Proxy (fromSaneParameter, SaneParameter(..))
import qualified Lorentz.Contracts.TZBTC as TZBTC
import Lorentz.Contracts.TZBTC.Types

originateContract :: IntegrationalScenarioM (ContractAddr Parameter)
originateContract =
  lOriginate tzbtcContract "TZBTC Contract" storage (toMutez 1000)

adminAddress :: Address
adminAddress = genesisAddress3

operatorAddress :: Address
operatorAddress = genesisAddress1

redeemAddress_ :: Address
redeemAddress_ = adminAddress

initialSupply :: Natural
initialSupply = 500

alice :: Address
alice = genesisAddress6

bob :: Address
bob = genesisAddress5

storage :: Storage
storage =
  mkStorage adminAddress redeemAddress_
    (Map.fromList [(redeemAddress_, initialSupply)])
    (Set.fromList [operatorAddress])

originateProxy
  :: ContractAddr TZBTC.Parameter
  -> IntegrationalScenarioM (ContractAddr Proxy.Parameter)
originateProxy targetAddress =
  lOriginate Proxy.tzbtcProxyContract "TZBTC Proxy" targetAddress (toMutez 1000)

originateAndSetProxy
  :: IntegrationalScenarioM (ContractAddr Parameter, ContractAddr Proxy.Parameter)
originateAndSetProxy = do
  c <- originateContract
  p <- originateProxy c
  withSender adminAddress $ lCall c (SetProxy $ unContractAddress p)
  pure (c, p)

test_proxy :: TestTree
test_proxy = testGroup "TZBTC contract proxy origination test"
  [ testCase
      "Proxy forwards calls to transfer entry points properly" $
        integrationalTestExpectation $ do
          (c, proxy) <- originateAndSetProxy
          -- Use proxy to transfer 100 tokens from redeemAddress to
          -- alice
          withSender redeemAddress_ $ lCall proxy
            (fromSaneParameter $
              STransfer (#from .! redeemAddress_, #to .! alice, #value 100))
          consumer <- lOriginateEmpty contractConsumer "consumer"
          -- Use proxy to get balance in redeemAddress_ and alice
          -- accounts.
          lCall proxy
            (fromSaneParameter $
              SGetBalance (View redeemAddress_ consumer))
          lCall proxy
            (fromSaneParameter $
              SGetBalance (View alice consumer))
          -- Use proxy to get total supply
          lCall proxy
            (fromSaneParameter $
              SGetTotalSupply (View () consumer))
          -- Call contract directly to get balance in redeemAddress_ and alice
          -- accounts.
          lCall c
            (GetBalance (View redeemAddress_ consumer))
          lCall c
            (GetBalance (View alice consumer))
          -- Call contract directly to get total supply
          lCall c
            (GetTotalSupply (View () consumer))
          validate . Right $
            lExpectViewConsumerStorage consumer [400, 100, 500, 400, 100, 500]
  , testCase
      "Proxy forwards calls to getAllowance entry points properly" $
        integrationalTestExpectation $ do
          (c, proxy) <- originateAndSetProxy
          -- Use proxy to transfer of 100 tokens from redeemAddress to
          -- alice
          withSender redeemAddress_ $ lCall proxy
            (fromSaneParameter $
              STransfer (#from .! redeemAddress_, #to .! alice, #value 100))
          -- Use proxy to approve transfer of 20 tokens from alice to
          -- bob
          withSender alice $ lCall proxy
            (fromSaneParameter $
              SApprove (#spender .! bob, #value 20))

          consumer <- lOriginateEmpty contractConsumer "consumer"
          -- Use proxy to get allowance for alice to bob transfer
          withSender alice $ lCall proxy
            (fromSaneParameter $
              SGetAllowance (View (#owner .! alice, #spender .! bob) consumer))

          -- Use proxy to transfer 10 tokens from alice to
          -- bob, as bob.
          withSender bob $ lCall proxy
            (fromSaneParameter $
              STransfer (#from .! alice, #to .! bob, #value 15))

          -- Use proxy to get balance in alice and bob
          -- accounts.
          lCall proxy
            (fromSaneParameter $
              SGetBalance (View alice consumer))
          lCall proxy
            (fromSaneParameter $
              SGetBalance (View bob consumer))

          -- Again, use proxy to get allowance for alice to bob transfer
          lCall proxy
            (fromSaneParameter $
              SGetAllowance (View (#owner .! alice, #spender .! bob) consumer))

          -- Call contract to get balance in alice and bob
          -- accounts.
          lCall c
            (GetBalance (View alice consumer))
          lCall c
            (GetBalance (View bob consumer))

          -- Again, call contract to get allowance for alice to bob transfer
          lCall c
            (GetAllowance (View (#owner .! alice, #spender .! bob) consumer))
          validate . Right $
            lExpectViewConsumerStorage consumer [20, 85, 15, 5, 85, 15, 5]
  , testCase
      "Transfer via Proxy entrypoints respect paused status" $
        integrationalTestExpectation $ do
          (c, proxy) <- originateAndSetProxy
          withSender operatorAddress $ lCall c (TZBTC.Pause ())
          -- Use proxy to transfer of 100 tokens from redeemAddress to
          -- alice
          withSender redeemAddress_ $ lCall proxy
            (fromSaneParameter $
              STransfer (#from .! redeemAddress_, #to .! alice, #value 100))
          validate . Left $
            lExpectError (== OperationsArePaused)
  , testCase
      "Approve via Proxy entrypoints respect paused status" $
        integrationalTestExpectation $ do
          (c, proxy) <- originateAndSetProxy
          withSender operatorAddress $ lCall c (TZBTC.Pause ())
          -- Use proxy to approve transfer of 100 tokens from alice to
          -- bob
          withSender alice $ lCall proxy
            (fromSaneParameter $
              SApprove (#spender .! bob, #value 20))
          validate . Left $
            lExpectError (== OperationsArePaused)
  ]

