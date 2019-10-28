{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE CPP #-}

module Util.Editor
  ( editConfigViaEditor
  ) where

#ifdef mingw32_HOST_OS

editConfigViaEditor :: FilePath -> IO ()
editConfigViaEditor _ =
  putTextLn "This feature is not available in Windows environment."

#else

import Data.ByteString (writeFile)
import Text.Editor (jsonTemplate, runUserEditorDWIMFile)

editConfigViaEditor :: FilePath -> (ByteString -> IO ()) -> IO ()
editConfigViaEditor path fn = do
  newContents <- runUserEditorDWIMFile jsonTemplate path
  fn newContents

#endif

