{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.IO.Config
  ( readConfig
  , writeConfig
  ) where

import Data.Aeson.Encode.Pretty as EP (Config(..), compare, defConfig, encodePretty')
import qualified Data.Aeson as Aeson (ToJSON, FromJSON, decodeStrict)
import qualified Data.ByteString.Lazy as BSL hiding (putStrLn)

import Client.Error
import Util.AbstractIO hiding (readConfig)
--
readConfig :: (Aeson.FromJSON a, HasFilesystem m) => FilePath -> m (Either TzbtcClientError a)
readConfig configPath = do
  fileExists <- doesFileExist configPath
  if fileExists then do
    mbConfig <- Aeson.decodeStrict <$> readFile configPath
    case mbConfig of
      Just config -> return $ Right config
      Nothing -> return $ Left TzbtcClientConfigError
  else return $ Left $ TzbtcClientConfigFileNotFound configPath

writeConfig :: (Aeson.ToJSON a, HasCmdLine m, HasFilesystem m) => FilePath -> a -> m ()
writeConfig configFilePath c = do
  printStringLn configFilePath
  writeFile configFilePath $ BSL.toStrict $ encodePretty' prettyConf c
  where
    prettyConf = defConfig { confCompare =  compare } -- Sort keys
