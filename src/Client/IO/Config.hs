{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.IO.Config
  ( getConfigPaths
  , readConfig
  , writeConfig
  ) where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson as Aeson (ToJSON, FromJSON, decodeFileStrict)
import qualified Data.ByteString.Lazy as BSL hiding (putStrLn)
import qualified System.Directory as Directory (doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserConfigFile)

import Client.Error
import Client.Types

appName, configFile :: FilePath
appName = "tzbtc"
configFile = "config.json"
--
readConfig :: (Aeson.FromJSON a) => FilePath -> IO (Either TzbtcClientError a)
readConfig configPath = do
  fileExists <- Directory.doesFileExist configPath
  if fileExists then do
    mbConfig <- Aeson.decodeFileStrict configPath
    case mbConfig of
      Just config -> return $ Right config
      Nothing -> return $ Left TzbtcClientConfigError
  else return $ Left $ TzbtcClientConfigFileNotFound configPath

writeConfig :: (Aeson.ToJSON a) => FilePath -> a -> IO ()
writeConfig configFilePath c = do
  putStrLn configFilePath
  BSL.writeFile configFilePath $ encodePretty c

getConfigPaths :: IO (DirPath, FilePath)
getConfigPaths = do
  configDir <- getUserConfigDir appName
  configPath <- getUserConfigFile appName configFile
  pure (DirPath configDir, configPath)
