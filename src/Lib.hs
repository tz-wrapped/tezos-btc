{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}

{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( someFunc
    ) where

import Universum

someFunc :: IO ()
someFunc = putTextLn "someFunc"
