{- SPDX-FileCopyrightText: 2021 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

{-# LANGUAGE DeriveAnyClass #-}

module Test.AsRPC (StorageRPC(..)) where

import Lorentz.Contracts.Metadata (TokenMetadata)
import Lorentz.Contracts.TZBTC.V0 (Storage)
import Lorentz.Contracts.TZBTC.V1.Types (StorageFields)
import Michelson.Typed.Haskell.Value (BigMapId, IsoValue)
import Morley.Nettest (AsRPC)

data StorageRPC ver = StorageRPC
  { dataMap :: BigMapId ByteString ByteString
  , fields :: StorageFields ver
  } deriving stock (Generic, Show)
    deriving anyclass (IsoValue)

type instance AsRPC (Storage ver) = StorageRPC ver
type instance AsRPC TokenMetadata = TokenMetadata
