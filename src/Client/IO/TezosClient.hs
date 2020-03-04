{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.IO.TezosClient
  ( getAddressAndPKForAlias
  , getAddressForContract
  , getOperationHex
  , getTezosClientConfig
  , revealKeyForAlias
  , signWithTezosClient
  , rememberContractAs
  , waitForOperationInclusion
  ) where

import Data.Aeson (eitherDecodeStrict)
import Data.Text as T (strip, length)
import Servant.Client (ClientEnv, runClientM)
import Servant.Client.Core as Servant (ClientError(..))
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Lorentz hiding (address, balance, chainId, cons, map)
import Michelson.Untyped (InternalByteString(..))
import Tezos.Address (Address, formatAddress, parseAddress)

import qualified Client.API as API
import Client.Crypto
import Client.Error
import Client.Parser
import Client.Types
import Client.Util

getTezosClientConfig
  :: FilePath -> IO (Either Text TezosClientConfig)
getTezosClientConfig exePath  = do
  (exitCode, stdout', _stderr') <- readProcessWithExitCode
    exePath ["config", "show"] ""
  pure $ case exitCode of
    ExitSuccess -> let
      out = strip $ toText stdout'
      -- If the output was empty there was no config file
      -- so return the default tezos-client configuration
      in if T.length out == 0
        then pure defTezosConf
        else case eitherDecodeStrict $ encodeUtf8 $ toText stdout' of
          Right x -> Right x
          Left err -> Left $ toText err
    ExitFailure _ -> Left "Fetching config from `tezos-client` failed"
  where
    defTezosConf = TezosClientConfig
      { tcNodeAddr = "localhost"
      , tcNodePort = 8732
      , tcTls = False
      }

revealKeyForAlias :: FilePath -> Text -> IO ()
revealKeyForAlias tcPath alias = do
  void $ readProcessWithExitCode tcPath
    ["reveal", "key", "for", toString alias] ""

getAddressAndPKForAlias
  :: FilePath -> Text -> IO (Either TzbtcClientError (Address, PublicKey))
getAddressAndPKForAlias tcPath alias  = do
  (exitCode, stdout', _) <- readProcessWithExitCode tcPath
    ["show", "address", toString alias] ""
  case exitCode of
    ExitSuccess -> do
      addr <- throwLeft $ pure $ (parseAddressFromOutput $ toText stdout')
      pure $ Right addr
    ExitFailure _ -> pure $ Left $ TzbtcUnknownAliasError alias

getAddressForContract
  :: FilePath -> Text -> IO (Either TzbtcClientError Address)
getAddressForContract tcPath alias  = do
  (exitCode, stdout', _) <- readProcessWithExitCode tcPath
    ["show", "known", "contract", toString alias] ""
  case exitCode of
    ExitSuccess -> do
      case parseAddress $ strip $ toText stdout' of
        Right a -> pure $ Right a
        Left err -> pure $ Left $ TzbtcParseError (show err)
    ExitFailure _ -> pure $ Left $ TzbtcUnknownAliasError alias

waitForOperationInclusion :: FilePath -> Text -> IO ()
waitForOperationInclusion tcPath op = do
  (exitCode, stdout', stderr') <- readProcessWithExitCode tcPath
    ["wait", "for", toString op, "to", "be", "included"] ""
  case exitCode of
    ExitSuccess -> putStr stdout'
    ExitFailure _ -> putStr stderr'

signWithTezosClient
  :: FilePath ->  Either InternalByteString Text -> Text -> IO (Either Text Signature)
signWithTezosClient tcPath bs alias = do
  let toSign = case bs of
        Left rawBS -> addTezosBytesPrefix $ formatByteString rawBS
        Right formatedBS -> formatedBS
  (exitCode, stdout', stderr') <- readProcessWithExitCode tcPath
    [ "sign", "bytes", toString $ toSign
    , "for", toString alias
    ] ""
  case exitCode of
    ExitSuccess -> do
      Right <$> (throwLeft $ pure $ parseSignatureFromOutput (fromString stdout'))
    ExitFailure _ -> return . Left . fromString $
      "Operation signing failed: " <> stderr'

rememberContractAs
  :: FilePath -> Address -> Text -> IO ()
rememberContractAs tcPath addr alias = do
  (exitCode, _, stderr') <- readProcessWithExitCode tcPath
    [ "remember", "contract", toString $ alias , toString $ formatAddress addr, "--force"] ""
  case exitCode of
    ExitSuccess -> pass
    ExitFailure _ -> throwM (TzbtcTezosClientError
      $ "There was an error in saving the contract alias" <> toText stderr')

getOperationHex
  :: ClientEnv -> ForgeOperation
  -> IO (Either ClientError InternalByteString)
getOperationHex env forgeOp = runClientM (API.forgeOperation forgeOp) env
