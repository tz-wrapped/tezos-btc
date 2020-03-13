{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.IO.TezosClient
  ( getAddressAndPKForAlias
  , getAddressForContract
  , getOperationHex
  , getTezosClientConfig
  , rememberContractAs
  , revealKeyForAlias
  , signWithTezosClient
  , simulateOrigination
  , simulateTransaction
  , toTezString
  , waitForOperationInclusion
  ) where

import Data.Aeson (eitherDecodeStrict)
import Data.Text as T (strip, length)
import Numeric (showFFloat)
import Servant.Client (ClientEnv, runClientM)
import Servant.Client.Core as Servant (ClientError(..))
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Tezos.Common.Json (TezosInt64)

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

-- | Dry run a transaction using 'tezos-client' and extract the expected baker fees
-- from the output. If the command fails due to a low 'burn-cap' detect it and parse
-- the required 'burn-cap' value from the stderr, and call again with that value using
-- the optional 'burn-cap' arugment.
simulateTransaction
  :: (NicePrintedValue param)
  => ClientConfig
  -> Maybe TezosInt64 -- burn cap
  -> Address
  -> EntrypointParam param
  -> IO (Either TzbtcClientError SimulationResult)
simulateTransaction config mbbc_ addr arg_ = simulateTransaction_ mbbc_
  where
    simulateTransaction_ mbbc = do
      let epArgs = case arg_ of
            DefaultEntrypoint p -> ["--arg", toString (printLorentzValue True p)]
            Entrypoint n p ->
              ["--arg", toString (printLorentzValue True p), "--entrypoint", toString n]
          bcArgs = case mbbc of
            Just bc -> ["--burn-cap", toTezString bc]
            Nothing -> []
      let args = ([ "transfer", "0.0", "from", (toString $ ccUserAlias config)
                  , "to",  toString $ formatAddress addr ] <> epArgs <> bcArgs <> ["-D"])
      (exitCode, stdout', stderr') <- readProcessWithExitCode (ccTezosClientExecutable config)
        args ""
      case exitCode of
        ExitSuccess -> do
          Right <$> (throwLeft $ pure $ parseSimulationResultFromOutput (fromString stdout'))
        ExitFailure _ -> case parseBurncapErrorFromOutput (fromString stderr') of
          Right p -> simulateTransaction_ (Just p)
          Left _ -> throwM (TzbtcTezosClientError $
            "There was an error in simulating the transaction:" <> (show args) <> "\nError:" <> toText stderr')

-- Same as above, but for origination
simulateOrigination
  :: (NiceParameterFull param, NiceStorage st)
  => ClientConfig
  -> Maybe TezosInt64 -- burn cap
  -> ContractCode param st
  -> st
  -> IO (Either TzbtcClientError SimulationResult)
simulateOrigination config mbbc_ contract_ initst = simulateOrigination_ mbbc_
  where
    simulateOrigination_ mbbc = do
      let bcArgs = case mbbc of
            Just bc -> ["--burn-cap", toTezString bc]
            Nothing -> []
      let args = ([ "originate", "contract", "-", "transferring", "0.0", "from", (toString $ ccUserAlias config)
                  , "running"
                  , toString $ printLorentzContract True contract_
                  , "--init", toString $ printLorentzValue True initst
                  ] <> bcArgs <> ["-D", "--force"])
      (exitCode, stdout', stderr') <- readProcessWithExitCode (ccTezosClientExecutable config) args ""
      case exitCode of
        ExitSuccess -> do
          Right <$> (throwLeft $ pure $ parseSimulationResultFromOutput (fromString stdout'))
        ExitFailure _ -> case parseBurncapErrorFromOutput (fromString stderr') of
          Right p -> do
            simulateOrigination_ (Just p)
          Left _ -> throwM (TzbtcTezosClientError $
            "There was an error in simulating the transaction:" <> (show args) <> "\nError:" <> toText stderr')

toTezString :: TezosInt64 -> String
toTezString x = showFFloat Nothing ((realToFrac x :: Double)/10e6) ""

getOperationHex
  :: ClientEnv -> ForgeOperation
  -> IO (Either ClientError InternalByteString)
getOperationHex env forgeOp = runClientM (API.forgeOperation forgeOp) env
