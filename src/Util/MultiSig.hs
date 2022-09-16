{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.MultiSig
  ( Sign
  , Package(..)
  , MSigParameter
  , MSigParamMain
  , MSigPayload
  , addSignature
  , decodePackage
  , encodePackage
  , mkPackage
  , mkMultiSigParam
  , getOpDescription
  , mergePackages
  , getBytesToSign
  , getToSign
  , fetchSrcParam
  )
where

import Prelude hiding (drop, toStrict, (>>))

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Lazy as LBS (toStrict)
import Data.List (lookup)
import Fmt (Buildable(..), Builder, blockListF, pretty, (|+))
import Morley.Michelson.Interpret.Unpack
import Morley.Tezos.Crypto
import Morley.Util.ByteString
import Text.Hex (encodeHex)
import Text.Show qualified (show)

import Lorentz
import Lorentz.Contracts.Multisig
import Lorentz.Contracts.Multisig.Specialized.Types qualified as Spec
import Lorentz.Contracts.TZBTC as TZBTC
import Lorentz.Contracts.TZBTC.Common.Types as TZBTC


-- The workflow consists of first calling the `mkPackage` function with the
-- required parameters to get a `Package` object which will hold the bytestring
-- to be signed. It also has a field for storing the signatures.
--
-- This can be json encoded and written to a file using the `encodePackage`
-- function.
--
-- Signatures can be added to the package file using the `addSignature` function.
--
-- After collecting enough signatures, the package file (or multiple ones) can
-- be passed to `mkMultiSigParam` function, which will create a multi-sig
-- contract param.  This param, when used in a call to the multisig contract
-- make it validate the signatures and forward the action to the main contract.

-- | Our signature.
type Sign =
  TSignature $ Packed $
    Spec.ToSign (SafeParameter SomeTZBTCVersion) (Parameter SomeTZBTCVersion)

-- | The type that will represent the actual data that will be provide to the
-- signers. Note that we are also collecting the public key associated with a
-- signature. This is because the multisig expects the signatures to be
-- ordered in the exact same way as the keys in its storage. If a signature is
-- not available then it expects a Nothing value in its place. So while calling
-- the multisig we have to correlate the public keys from the multisig
-- contract, and arrange the available signatures, in the same order, filling
-- the unavailable ones with Nothings.
data Package = Package
  { pkToSign :: !HexJSONByteString  -- ^ Hex encoded byte sequence that should be signed
  , pkSignatures :: ![(PublicKey, Sign)] -- ^ Field to hold signatures and public keys as they are collected.
  }

instance {-# OVERLAPS #-} Buildable (PublicKey, Sign) where
  build (pk, sig) =  ("Public key: " :: String)
                  |+ (formatPublicKey pk)
                  |+ ("\nSignature: " :: String)
                  |+ (build $ formatSignature $ unTSignature sig)

instance Buildable Package where
  build p =
    ("Operation:" :: String)
    |+ ("\n------------------\n" :: String) |+ (getOpDescription p) |+ newLine
    |+ ("\nIncluded Signatures:" :: String)
    |+ ("\n--------------------\n" :: String) |+ (blockListF $ build <$> pkSignatures p)
    where
      newLine = "\n" :: String

-- | Extract and return the source parameter from the packed byte sequence
-- that is to be signed.
fetchSrcParam
  :: Package
  -> Either UnpackageError (TZBTC.Parameter SomeTZBTCVersion)
fetchSrcParam package =
  case fromVal @ToSign <$> unpackValue' (unHexJSONByteString $ pkToSign package) of
    Right (_, (_, action)) ->
      case action of
        Operation (safeParameter, _) ->
          Right (TZBTC.SafeEntrypoints safeParameter)
        _ -> error "Unsupported multisig operation"
    Left err -> Left $ UnpackFailure err

checkIntegrity
  :: Package
  -> Bool
checkIntegrity = isRight . fetchSrcParam

-- | Get Operation description from serialized value
getOpDescription
  :: Package -> Builder
getOpDescription p = case fetchSrcParam p of
  Right param -> build param
  Left err -> build err

-- | Make the `Package` value from input parameters.
mkPackage
  :: forall msigAddr. (ToTAddress MSigParameter () msigAddr)
  => msigAddr
  -> ChainId
  -> Counter
  -> TAddress (TZBTC.Parameter SomeTZBTCVersion) ()
  -> TZBTC.SafeParameter SomeTZBTCVersion -> Package
mkPackage msigAddress chainId_ counter tzbtc param
  = let msigParam = Operation (param, tzbtc)
        msigTAddr = (toTAddress @MSigParameter @()) msigAddress
    -- Create the package for required action
    in Package
      {
        -- Wrap the parameter with multisig address and replay attack counter,
        -- forming a structure that the multi-sig contract will ultimately
        -- verify the included signatures against
        pkToSign = HexJSONByteString $ toBytes $ lPackValue $
                   ((chainId_, toAddress msigTAddr), (counter, msigParam))
      , pkSignatures = [] -- No signatures when creating package
      }

mergeSignatures
  :: Package
  -> Package
  -> Maybe Package
mergeSignatures p1 p2 =
  -- If the payloads are same, then merge the signatures from both
  -- packages and form a new package with both the signatures.
  if pkToSign p1 == pkToSign p2
    then Just $ p1 { pkSignatures = pkSignatures p1 ++ pkSignatures p2 }
    else Nothing

mergePackages
  :: NonEmpty Package
  -> Either UnpackageError Package
mergePackages (p :| ps) = maybeToRight PackageMergeFailure $
  foldM mergeSignatures p ps

getBytesToSign :: Package -> Text
getBytesToSign Package{..} =
  addTezosBytesPrefix $ encodeHex $ unHexJSONByteString pkToSign
  where
    addTezosBytesPrefix :: Text -> Text
    addTezosBytesPrefix = ("0x" <>)

-- | Extract the signable component from package. This is a structure
-- with the packed parameter that represent the action, the multi-sig contract
-- address, and the replay attack prevention counter.
getToSign :: Package -> Either UnpackageError ToSign
getToSign Package{..} = first UnpackFailure $
  fromVal @ToSign <$> unpackValue' (unHexJSONByteString pkToSign)

-- | Errors that can happen when package is de-serialized back to the multi-sig
-- contract param
data UnpackageError
  = HexDecodingFailure
  | UnpackFailure UnpackError
  | PackageMergeFailure
  | UnexpectedParameterWithView

instance Buildable UnpackageError where
  build = \case
    HexDecodingFailure -> "Error decoding hex encoded string"
    PackageMergeFailure -> "Provied packages had different action/enviroments"
    UnpackFailure err -> build err
    UnexpectedParameterWithView -> "Unexpected parameter with View_ in package"

instance Show UnpackageError where
  show = pretty

instance Exception UnpackageError where
  displayException = pretty

deriveJSON (aesonPrefix camelCase) ''Package

-- | Encode package
encodePackage
  :: Package
  -> ByteString
encodePackage = toStrict . encode

-- | Decode package
decodePackage
  :: ByteString
  -> Either String Package
decodePackage = eitherDecodeStrict

-- | Add signature to package.
addSignature
  :: Package
  -> (PublicKey, Sign)
  -> Either String Package
addSignature package sig =
  if checkIntegrity package then let
    -- ^ Checks if the included source parameter matches with the
    -- signable payload.
    existing = pkSignatures package
    in Right $ package { pkSignatures = sig:existing }
  else Left "WARNING!! Cannot add signature as the integrity of the multi-sig package could not be verified"

-- | Given a value of type `Package`, and a list of public keys,
-- make the actual parameter that the multi-sig contract can be called with.
-- The list of public keys should be in the same order that the contract has
-- them in it's storage.
mkMultiSigParam
  :: [PublicKey]
  -> NonEmpty Package
  -> Either UnpackageError ((TAddress MSigParameter ()), MSigParamMain)
mkMultiSigParam pks packages = do
  package <- mergePackages packages
  toSign <- getToSign package
  return $ mkParameter toSign (pkSignatures package)
  where
    mkParameter
      :: ToSign
      -> [(PublicKey, Sign)]
      -> (TAddress MSigParameter (), MSigParamMain)
    mkParameter ((_, address_), payload) sigs =
      -- There should be as may signatures in the submitted request
      -- as there are keys in the contract's storage. Not all keys should
      -- be present, but they should be marked as absent using Nothing values [1].
      -- So we pad the list with Nothings to make up for missing signatures.
      (toTAddress address_, (payload, sortSigs sigs))
    sortSigs :: [(PublicKey, Sign)] -> [Maybe Sign]
    sortSigs sigs = flip lookup sigs <$> pks

deriving newtype instance ToJSON (TSignature a)
deriving newtype instance FromJSON (TSignature a)
