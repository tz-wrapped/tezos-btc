{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Crypto
  ( formatByteString
  , prepareForInjection
  , unsafeParseSignature
  ) where

import qualified Data.ByteArray as BA (convert)
import Fmt (pretty)
import Text.Hex (encodeHex)

import Michelson.Untyped (InternalByteString(..))
import Tezos.Crypto (Signature(..), parseSignature)

formatByteString :: InternalByteString -> Text
formatByteString (InternalByteString bs) = encodeHex bs

prepareForInjection :: Text -> Signature -> Text
prepareForInjection operationHex (Signature signature') =
  operationHex <> (encodeHex . BA.convert) signature'

unsafeParseSignature :: HasCallStack => Text -> Signature
unsafeParseSignature = either (error . pretty) id . parseSignature
