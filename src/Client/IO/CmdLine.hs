{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Client.IO.CmdLine (confirmAction) where

import Client.Types

confirmAction :: Text -> IO ConfirmationResult
confirmAction msg = do
  putStrLn $ msg <> " [Y/N]"
  res <- getLine
  case res of
    x | x `elem` ["Y", "y", "yes"] -> pure Confirmed
    x | x `elem` ["N", "n", "no"] -> pure Canceled
    _ -> confirmAction msg
