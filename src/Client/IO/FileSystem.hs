{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.IO.FileSystem
  ( getConfigPaths
  ) where

import System.Environment.XDG.BaseDir (getUserConfigDir, getUserConfigFile)

import Client.Types

appName, configFile :: FilePath
appName = "tzbtc"
configFile = "config.json"

getConfigPaths :: IO (DirPath, FilePath)
getConfigPaths = do
  configDir <- getUserConfigDir appName
  configPath <- getUserConfigFile appName configFile
  pure (DirPath configDir, configPath)
