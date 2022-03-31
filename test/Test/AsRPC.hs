{- SPDX-FileCopyrightText: 2021 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}

module Test.AsRPC (StorageRPC(..)) where

import Lorentz.Contracts.Metadata (TokenMetadata)
import Lorentz.Contracts.TZBTC.V0 (Storage)
import Lorentz.Contracts.TZBTC.V1.Types (StorageFields)
import Morley.Michelson.Typed.Haskell.Value (BigMapId, IsoValue)
import Test.Cleveland (HasRPCRepr(..))

data StorageRPC ver = StorageRPC
  { dataMap :: BigMapId ByteString ByteString
  , fields :: StorageFields ver
  } deriving stock (Generic, Show)
    deriving anyclass (IsoValue)

instance HasRPCRepr (Storage ver) where
  type AsRPC (Storage ver) = StorageRPC ver
instance HasRPCRepr TokenMetadata where
  type AsRPC TokenMetadata = TokenMetadata
