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
import Data.Text as T (intercalate, length, strip)
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
  :: Bool -> FilePath -> IO (Either Text TezosClientConfig)
getTezosClientConfig v exePath  = do
  callTezosClient v exePath ["config", "show"] Nothing $ \stdout' ->
    let
      out = strip $ toText stdout'
      -- If the output was empty there was no config file
      -- so return the default tezos-client configuration
    in if T.length out == 0
      then pure $ Right defTezosConf
      else case eitherDecodeStrict $ encodeUtf8 $ toText stdout' of
        Right x -> pure $ Right x
        Left err -> pure $ Left $ toText err
  where
    defTezosConf = TezosClientConfig
      { tcNodeAddr = "localhost"
      , tcNodePort = 8732
      , tcTls = False
      }

-- | Call a program at a given path with the provided arguments. If the
-- program ran successfully, process the stdout using the first callback, and
-- return the output as a Right value.  If the program failed, process the
-- stderr using the second callback, and return the output as a Left value.
callExe
  :: forall sPrcs ePrcs. Bool
  -> FilePath
  -> [String]
  -> (Text -> IO sPrcs)
  -> (Text -> IO ePrcs)
  -> IO (Either ePrcs sPrcs)
callExe v exePath args sPrcs ePrcs = do
  when v $ do
    putStrLn $ "Executing call:" <> (toText exePath) <> " " <> T.intercalate  " " (toText <$> args)
  (ec, so, se) <- readProcessWithExitCode exePath args ""
  when v $ do
    putStrLn $ "Status:" <> (show ec :: Text)
    putStrLn $ "Output:" <> so
    putStrLn $ "Error:" <> se
  case ec of
    ExitSuccess -> Right <$> sPrcs (toText so)
    ExitFailure _ -> Left <$> ePrcs (toText se)

-- | Call `tezos-client` at the provided path with provided args. If optional
-- burn-cap is provided, include it in the arguments. If the call failed see if
-- the failure was due to a low burn cap error. If it was, extract the required
-- burn-cap from the error, and call itself with explicitly specified burn-cap.
callTezosClient'
  :: forall sPrcs ePrcs. Bool
  -> FilePath
  -> [String]
  -> Maybe TezosInt64
  -> (Text -> IO sPrcs)
  -> (Text -> IO ePrcs)
  -> IO (Either ePrcs sPrcs)
callTezosClient' v tcPath args mburnCap sPrcs ePrcs = do
  case mburnCap of
    Just bc -> callExe v tcPath (args <> ["--burn-cap", toTezString bc]) sPrcs ePrcs
    Nothing -> do
      r <- callExe v tcPath args sPrcs burnCapParser
      case r of
        Right rs -> pure (Right rs)
        Left err -> case err of
          Right bc ->
            callTezosClient' v tcPath args (Just $ toMuTez bc) sPrcs ePrcs
          Left err_ -> Left <$> ePrcs err_
  where
    burnCapParser :: Text -> IO (Either Text Double)
    burnCapParser stderr' = case parseBurncapErrorFromOutput stderr' of
      Right bc -> pure $ Right bc
      Left _ -> pure $ Left stderr'

-- | Call tezos-client but throw an error on failure
callTezosClient
  :: forall sPrcs. Bool
  -> FilePath
  -> [String]
  -> Maybe TezosInt64
  -> (Text -> IO sPrcs) -> IO sPrcs
callTezosClient v tcPath args mburnCap sPrcs = do
  r <- callTezosClient' v tcPath args mburnCap sPrcs pure
  case r of
    Right a -> pure a
    Left err -> throwM $ TzbtcTezosClientError err

-- | Call tezos-client but return a Nothing on falure
callTezosClientSafe
  :: forall sPrcs. Bool
  -> FilePath
  -> [String]
  -> Maybe TezosInt64
  -> (Text -> IO sPrcs) -> IO (Maybe sPrcs)
callTezosClientSafe v tcPath args mburnCap sPrcs = do
  r <- callTezosClient' v tcPath args mburnCap sPrcs pure
  case r of
    Right a -> pure $ Just a
    Left _ -> pure Nothing

parserToCallback :: forall a e. (Show e) => (Text -> Either e a) -> (Text -> IO a)
parserToCallback fn = \t -> case fn t of
  Right a -> pure a
  Left e -> throwM $ TzbtcTezosClientError (show e)

revealKeyForAlias :: Bool -> FilePath -> Text -> IO ()
revealKeyForAlias v tcPath alias =
  void $ callTezosClientSafe v tcPath ["reveal", "key", "for", toString alias] Nothing (void . pure)

getAddressAndPKForAlias
  :: Bool -> FilePath -> Text -> IO (Either TzbtcClientError (Address, PublicKey))
getAddressAndPKForAlias v tcPath alias  = do
  callTezosClient' v
    tcPath ["show", "address", toString alias]
    Nothing (parserToCallback parseAddressFromOutput) (\_ -> pure $ TzbtcUnknownAliasError alias)

getAddressForContract
  :: Bool -> FilePath -> Text -> IO (Either TzbtcClientError Address)
getAddressForContract v tcPath alias  = do
  callTezosClient' v tcPath
    ["show", "known", "contract", toString alias] Nothing (\stdout' ->
      case parseAddress $ strip $ toText stdout' of
        Right a -> pure $ a
        Left err -> throwM $ TzbtcParseError (show err))
      (\stderr' -> pure $ TzbtcTezosClientError stderr')

waitForOperationInclusion :: Bool -> FilePath -> Text -> IO ()
waitForOperationInclusion v tcPath op =
  void $ callTezosClient' v tcPath
    ["wait", "for", toString op, "to", "be", "included"] Nothing putStr putStr

signWithTezosClient
  :: Bool -> FilePath ->  Either InternalByteString Text -> Text -> IO (Either Text Signature)
signWithTezosClient v tcPath bs alias = do
  let toSign = case bs of
        Left rawBS -> addTezosBytesPrefix $ formatByteString rawBS
        Right formatedBS -> formatedBS
  callTezosClient' v tcPath
    [ "sign", "bytes", toString $ toSign
    , "for", toString alias
    ] Nothing (parserToCallback parseSignatureFromOutput) (\stderr' -> pure $ "Operation signing failed: " <> stderr')

rememberContractAs
  :: Bool -> FilePath -> Address -> Text -> IO ()
rememberContractAs v tcPath addr alias =
  callTezosClient v tcPath
    [ "remember", "contract", toString $ alias , toString $ formatAddress addr, "--force"]
    Nothing (void . pure)

-- | Dry run a transaction using 'tezos-client' and extract the expected baker fees
-- from the output. If the command fails due to a low 'burn-cap' detect it and parse
-- the required 'burn-cap' value from the stderr, and call again with that value using
-- the optional 'burn-cap' arugment.
simulateTransaction
  :: (NicePrintedValue param)
  => Bool
  -> ClientConfig
  -> Maybe TezosInt64 -- burn cap
  -> Address
  -> EntrypointParam param
  -> IO SimulationResult
simulateTransaction v config mbbc_ addr arg_ = do
  let epArgs = case arg_ of
        DefaultEntrypoint p -> ["--arg", toString (printLorentzValue True p)]
        Entrypoint n p ->
          ["--arg", toString (printLorentzValue True p), "--entrypoint", toString n]
  let args = ([ "transfer", "0.0", "from", (toString $ ccUserAlias config)
              , "to",  toString $ formatAddress addr ] <> epArgs <> ["-D"])
  callTezosClient v (ccTezosClientExecutable config) args mbbc_ (parserToCallback parseSimulationResultFromOutput)

-- Same as above, but for origination
simulateOrigination
  :: (NiceParameterFull param, NiceStorage st)
  => Bool
  -> ClientConfig
  -> Maybe TezosInt64 -- burn cap
  -> ContractCode param st
  -> st
  -> IO SimulationResult
simulateOrigination v config mbbc_ contract_ initst = do
  let args = ([ "originate", "contract", "-", "transferring", "0.0", "from", (toString $ ccUserAlias config)
              , "running"
              , toString $ printLorentzContract True contract_
              , "--init", toString $ printLorentzValue True initst
              ] <> ["-D", "--force"])
  callTezosClient v (ccTezosClientExecutable config) args mbbc_ (parserToCallback parseSimulationResultFromOutput)

toTezString :: TezosInt64 -> String
toTezString x = showFFloat Nothing ((realToFrac x :: Double)/10e6) ""

getOperationHex
  :: ClientEnv -> ForgeOperation
  -> IO (Either ClientError InternalByteString)
getOperationHex env forgeOp = runClientM (API.forgeOperation forgeOp) env
