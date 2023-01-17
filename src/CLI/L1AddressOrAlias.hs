-- SPDX-FileCopyrightText: 2022 Bitcoin Suisse
-- SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse

{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO [morley#923]: Remove this module when the instance is in morley.
-- | Provides 'HasCLReader' for 'L1Address'
module CLI.L1AddressOrAlias
  () where

import Fmt
import Options.Applicative qualified as Opt

import Morley.Tezos.Address
import Morley.Util.CLI

instance HasCLReader L1Address where
  getMetavar = "CONTRACT OR IMPLICIT ADDRESS"
  getReader = Opt.str >>= either (fail . pretty) pure . parseConstrainedAddress
