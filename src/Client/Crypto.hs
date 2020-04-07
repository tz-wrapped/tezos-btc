{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Client.Crypto
  ( formatByteString
  , prepareForInjection
  , unsafeParseSignature
  ) where

import Fmt (pretty)
import Text.Hex (encodeHex)

import Michelson.Untyped (InternalByteString(..))
import Tezos.Crypto (Signature, parseSignature, signatureToBytes)

formatByteString :: InternalByteString -> Text
formatByteString (InternalByteString bs) = encodeHex bs

prepareForInjection :: Text -> Signature -> Text
prepareForInjection operationHex signature =
  operationHex <> (encodeHex . signatureToBytes) signature

unsafeParseSignature :: HasCallStack => Text -> Signature
unsafeParseSignature = either (error . pretty) id . parseSignature
