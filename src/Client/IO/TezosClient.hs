{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.IO.TezosClient
  ( getAddressAndPKForAlias
  , getOperationHex
  , signWithTezosClient
  , waitForOperationInclusion
  ) where

import Servant.Client (ClientEnv, runClientM)
import Servant.Client.Core as Servant (ClientError(..))
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Lorentz hiding (address, balance, chainId, cons, map)
import Michelson.Untyped (InternalByteString(..))

import qualified Client.API as API
import Client.Crypto
import Client.Error
import Client.Parser
import Client.Types
import Client.Util

tezosNodeArgs :: ClientConfig -> [String]
tezosNodeArgs ClientConfig{..} = ["-A", toString ccNodeAddress, "-P", show ccNodePort]
  ++ bool [] ["-S"] ccNodeUseHttps

getAddressAndPKForAlias
  :: Text -> ClientConfig -> IO (Either TzbtcClientError (Address, PublicKey))
getAddressAndPKForAlias alias config@ClientConfig{..} = do
  (exitCode, stdout', _) <- readProcessWithExitCode ccTezosClientExecutable
    (tezosNodeArgs config <> ["show", "address", toString alias]) ""
  case exitCode of
    ExitSuccess -> do
      addr <- throwLeft $ pure $ (parseAddressFromOutput $ toText stdout')
      pure $ Right addr
    ExitFailure _ -> pure $ Left $ TzbtcUnknownAliasError alias

waitForOperationInclusion :: Text -> ClientConfig -> IO ()
waitForOperationInclusion op config@ClientConfig{..} = do
  (exitCode, stdout', stderr') <- readProcessWithExitCode ccTezosClientExecutable
    (tezosNodeArgs config <> ["wait", "for", toString op, "to", "be", "included"]) ""
  case exitCode of
    ExitSuccess -> putStr stdout'
    ExitFailure _ -> putStr stderr'

signWithTezosClient
  :: Either InternalByteString Text -> ClientConfig -> IO (Either Text Signature)
signWithTezosClient bs config@ClientConfig{..} = do
  let toSign = case bs of
        Left rawBS -> addTezosBytesPrefix $ formatByteString rawBS
        Right formatedBS -> formatedBS
  (exitCode, stdout', stderr') <- readProcessWithExitCode ccTezosClientExecutable
    (tezosNodeArgs config <>
     [ "sign", "bytes", toString $ toSign
     , "for", toString ccUserAlias
     ]) ""
  case exitCode of
    ExitSuccess -> do
      Right <$> (throwLeft $ pure $ parseSignatureFromOutput (fromString stdout'))
    ExitFailure _ -> return . Left . fromString $
      "Operation signing failed: " <> stderr'

getOperationHex
  :: ClientEnv -> ForgeOperation
  -> IO (Either ClientError InternalByteString)
getOperationHex env forgeOp = runClientM (API.forgeOperation forgeOp) env

