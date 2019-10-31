{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Error
  ( TzbtcClientError (..)
  ) where

import Fmt (Buildable(..), Builder, pretty, (+|), (|+), (+||), (||+))
import Servant.Client.Core (ClientError(..), Response, ResponseF(..))
import Servant.Client.Core.Request (RequestF(..))
import qualified Text.Show (show)

import Tezos.Crypto (CryptoParseError (..))

import Client.Types

data TzbtcClientError
  = TzbtcServantError ClientError
  | TzbtcClientConfigError
  | TzbtcClientConfigFileNotFound FilePath
  | TzbtcRunFailed [RunError]
  | TzbtcUnexpectedRunResult Text
  | TzbtcPublicKeyParseError Text CryptoParseError
  | TzbtcUnknownAliasError Text
  | TzbtcMutlisigConfigUnavailable
  | TzbtcOriginationError Text
  | TzbtcContractConfigUnavailable

instance Buildable TzbtcClientError where
  build (TzbtcServantError err) = case err of
    ConnectionError e ->
      "Connection error, no responce recieved with error: " +|| e ||+ ""
    DecodeFailure text response ->
      "The body could not be decoded at the expected type: " +|| text |+ "\n" +|
      "Response:\n" +| buildResponse response
    FailureResponse Request{..} response ->
      "Request to " +|| requestPath ||+
      " returned an error response:\n" +|
      buildResponse response
    UnsupportedContentType mediaType response ->
      "The content-type '" +|| mediaType ||+ "' of the response is not supported\n" +|
      "Response:\n" +| buildResponse response |+ ""
    InvalidContentTypeHeader response ->
      "The content-type header is invalid" +|
      "Response:\n" +| buildResponse response |+ ""

  build TzbtcClientConfigError =
    "Invalid client configuration. Use 'tzbtc-client setupClient'"

  build (TzbtcClientConfigFileNotFound p) =
    "Config file was not found at : " +| (build p)

  build TzbtcMutlisigConfigUnavailable =
    "Multi-sig configuration was not available"

  build TzbtcContractConfigUnavailable =
    "TZBTC contract address is not specified in the config.\n\
    \Use 'tzbtc-client deployContract' command to deploy the contract"

  build (TzbtcRunFailed errs) =
    "Transaction run have failed with " +| length errs |+ " errors:\n" +|
    mconcat (map ((<> "\n\n") . build) errs) |+ ""

  build (TzbtcUnexpectedRunResult msg) =
    "Unexpected result of transaction preliminary run: " +| msg |+ ""

  build (TzbtcPublicKeyParseError pk err) =
    "Failed to parse public key " +| pk |+ " with: " +| err |+ ""

  build (TzbtcUnknownAliasError alias) =
    "Unknown alias: " +| alias |+ ""

  build (TzbtcOriginationError msg) =
    "Error during contract origination:\n" +| msg |+ ""

instance Show TzbtcClientError where
  show = pretty

instance Exception TzbtcClientError where
  displayException = pretty

buildResponse :: Response -> Builder
buildResponse Response{..} =
  "Response status code: " +|| responseStatusCode ||+ "\n" +|
  "Response headers: " +|| intercalate ", " (map show $ toList responseHeaders) ||+ "\n" +|
  "Response HTTP version: " +|| responseHttpVersion ||+ "\n" +|
  "Response body:" +|| responseBody ||+ ""
