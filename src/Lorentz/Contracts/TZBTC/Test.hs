{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Lorentz.Contracts.TZBTC.Test
  ( mkTestScenario
  ) where

import Lorentz.Common (TestScenario)
import Tezos.Address (Address)
import Util.Named ((.!))

import Lorentz.Contracts.TZBTC (Parameter(..))
import Lorentz.Contracts.TZBTC.Types (ParameterWithoutView(..))

mkTestScenario :: Address -> [Address] -> Maybe (TestScenario Parameter)
mkTestScenario owner addresses = do
  case addresses of
    addr0 : addr1 : _ -> Just
      [ (owner, EntrypointsWithoutView $ AddOperator (#operator .! owner))
      , (owner, EntrypointsWithoutView $ Pause ())
      , (owner, EntrypointsWithoutView $ Unpause ())
      , (owner, EntrypointsWithoutView $ Mint (#to .! addr0, #value .! 100500))
      , (owner, EntrypointsWithoutView $ Mint (#to .! addr1, #value .! 100500))
      ]
    _ -> Nothing
