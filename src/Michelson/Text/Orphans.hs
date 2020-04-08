{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# OPTIONS_GHC -Wno-orphans #-}

module Michelson.Text.Orphans () where

import Text.Read

import Michelson.Text

instance Read MText where
  readPrec = do
    rawString <- readPrec
    case mkMText $ fromString rawString of
      Left err -> fail $ toString err
      Right xs -> return xs

