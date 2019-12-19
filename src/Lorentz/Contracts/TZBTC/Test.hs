{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}

-- Use of Lorentz.TestScenario has been deprecated.
-- TODO [TBTC-86]: remove
{-# OPTIONS_GHC -Wno-deprecations #-}

module Lorentz.Contracts.TZBTC.Test
  ( mkTestScenario
  ) where

import Lorentz.TestScenario (TestScenario)
import Tezos.Address (Address)
import Util.Named ((.!))

import Lorentz.Contracts.TZBTC

mkTestScenario :: Address -> [Address] -> Maybe (TestScenario (Parameter SomeTZBTCVersion))
mkTestScenario owner addresses = do
  case addresses of
    addr0 : addr1 : _ -> Just
      [ (owner, fromFlatParameter $ AddOperator (#operator .! owner))
      , (owner, fromFlatParameter $ Pause ())
      , (owner, fromFlatParameter $ Unpause ())
      , (owner, fromFlatParameter $ Mint (#to .! addr0, #value .! 100500))
      , (owner, fromFlatParameter $ Mint (#to .! addr1, #value .! 100500))
      ]
    _ -> Nothing
