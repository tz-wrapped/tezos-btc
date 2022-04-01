{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Client.Error
  ( TzbtcClientError (..)
  ) where

import Fmt (Buildable(..), pretty, (+|), (|+))
import Text.Show qualified (show)

data TzbtcClientError
  = TzbtcUnknownAliasError Text
  | TzbtcMultisigZeroThreshold
  | TzbtcMultisigThresholdLargerThanKeys

instance Buildable TzbtcClientError where
  build (TzbtcUnknownAliasError alias) =
    "Unknown alias: " +| alias |+ ""

  build TzbtcMultisigZeroThreshold =
    "Trying to originate multisig contract with zero threshold"

  build TzbtcMultisigThresholdLargerThanKeys =
    "Trying to originate multisig contract with threshold larger \
    \than signers public keys list size"

instance Show TzbtcClientError where
  show = pretty

instance Exception TzbtcClientError where
  displayException = pretty
