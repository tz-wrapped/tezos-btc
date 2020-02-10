{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.MultiSig
  ( Package(..)
  , Signable
  , MSigParameter
  , ParamMain
  , Payload
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

import Data.Aeson (eitherDecodeStrict, encode)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Lazy as LBS (toStrict)
import Data.List (lookup)
import Fmt (Buildable(..), Builder, blockListF, pretty, (|+))
import Text.Hex (decodeHex, encodeHex)
import qualified Text.Show (show)

import Client.Util (addTezosBytesPrefix)
import Lorentz
import Lorentz.Contracts.TZBTC as TZBTC
--import Lorentz.Contracts.TZBTC.MultiSig as MSig
import Lorentz.Contracts.TZBTC.Types as TZBTC
import Lorentz.Contracts.Upgradeable.Common (VersionKind, VerPermanent)
import qualified Lorentz.Contracts.Multisig.Specialized as SpMSig
import Michelson.Interpret.Unpack
import Tezos.Crypto

type MSigParameter v = SpMSig.Parameter (SafeParameter v) (TZBTC.Parameter v)
type ParamMain v = SpMSig.MainParams (SafeParameter v) (TZBTC.Parameter v)
type Payload v = SpMSig.Payload (SafeParameter v) (TZBTC.Parameter v)

-- The work flow consist of first calling the `mkPackage` function with the
-- required parameters to get a `Package` object which will hold the byte string
-- to be signed along with the environment to recreate the original action, along
-- with a place holder for storing the signatures. This can be json encoded and
-- written to a file using the `encodePackage` function.
--
-- Signatures can be added to the file using the `addSignature` function.
--
-- After collecting enough signatures, the file can be passed to
-- `mkMultiSigParam` function, which will create a multi-sig contract param.
-- This to param, when used in a call to the multisig contract make it validate
-- the signatures and call the action in the main contract.

-- | This is the structure that will be serialized and signed on. We are using
-- this particular type because the multisig contract extracts the payload,
-- `ParamPayload` from its parameter, pair it with the contracts address
-- (self), then serialize and check the signatures provided on that serialized
-- data.
type ToSign (v :: VersionKind) = (Address, Payload v)

-- | The type that will represent the actual data that will be provide to
-- signers. We are using this, instead of just providing the byte sequence,
-- because we need the `TcOriginatedContracts` map to recreate the original
-- payload from the serialized bytesequence. So this information along with
-- the signatures should be enough to actually execute the transaction at the
-- multisig contract.
--
-- Note that we are also collecting the public key associated with a signature.
-- This is because the multi-sig expects the signatures to be orders in the
-- exact same way as the keys in its storage. If a signature is not available
-- then it expects a Nothing value in its place.
data Package = Package
  { pkToSign :: !Text  -- ^ Hex encoded byte sequence that should be signed
  , pkSignatures :: ![(PublicKey, Signature)] -- ^ Field to hold signatures and public keys as they are collected.
  , pkSrcParam :: !Text -- ^ Packed main contract param and counter value
  }

instance Buildable (PublicKey, Signature) where
  build (pk, sig) =  ("Public key: " :: String)
                  |+ (formatPublicKey pk)
                  |+ ("\nSignature: " :: String)
                  |+ (build $ formatSignature sig)

instance Buildable Package where
  build p =
    ("Operation:" :: String)
    |+ ("\n------------------\n" :: String) |+ (getOpDescription p) |+ newLine
    |+ ("\nIncluded Signatures:" :: String)
    |+ ("\n--------------------\n" :: String) |+ (blockListF $ build <$> pkSignatures p)
    where
      newLine = "\n" :: String

-- | Match packed parameter with the signed bytesequence and if it matches,
-- return it, or else return an error.  The idea is that the source parameter
-- is meaningless (and probably dangerous) if it does not match with the
-- packed bytesequence that is being signed on.
fetchSrcParam
  :: forall (v :: VersionKind). (TZBTCVersionC v)
  => Package
  -> Either UnpackageError (TZBTC.Parameter v)
fetchSrcParam package =
  case decodeHex $ pkSrcParam package of
    Just hexDecoded ->
      case fromVal @((TZBTC.SafeParameter v), Natural, TAddress _)
          <$> unpackValue' hexDecoded of
        Right (safeParameter, counter, caddress) ->
          case getToSign @v package of
            Right (msigAddr, _) ->
              let newPackage = mkPackage
                    msigAddr counter caddress safeParameter
              in if pkToSign newPackage == pkToSign package
              then Right (TZBTC.SafeEntrypoints safeParameter)
              else Left BadSrcParameterFailure
            Left err -> Left err
        Left err -> Left $ UnpackFailure err
    Nothing -> Left HexDecodingFailure

checkIntegrity
  :: Package
  -> Bool
checkIntegrity = isRight . fetchSrcParam @SomeTZBTCVersion

-- | Get Operation description from serialized value
getOpDescription
  :: Package -> Builder
getOpDescription p = case fetchSrcParam @SomeTZBTCVersion p of
  Right param -> build param
  Left err -> build err

-- | Make the `Package` value from input parameters.
mkPackage
  :: forall (v :: VersionKind) msigAddr. (TZBTCVersionC v, ToTAddress (MSigParameter v) msigAddr)
  => msigAddr
  -> Natural
  -> TAddress (TZBTC.Parameter v)
  -> TZBTC.SafeParameter v -> Package
mkPackage msigAddress counter tzbtc param
  = let msigLambda = SpMSig.Operation (param, tzbtc)
        msigTAddr = (toTAddress @(MSigParameter v)) msigAddress
    -- Create the Lambda for required action
    in Package
      { pkToSign = encodeToSign $ (toAddress msigTAddr, (SpMSig.Counter counter, msigLambda))
      -- ^ Wrap the the lambda with multisig address and replay attack counter,
      -- forming a structure that the multi-sig contract will ultimately
      -- verify the included signatures against
      , pkSignatures = [] -- No signatures when creating package
      , pkSrcParam = encodeHex $ lPackValue (param, counter, tzbtc)
      -- ^ Include the input parameter for integrity check and showing human
      -- readable description of the action parameter.
      }

mergeSignatures
  :: Package
  -> Package
  -> Maybe Package
mergeSignatures p1 p2 =
  if pkToSign p1 == pkToSign p2
    -- ^ If the payloads are same, then merge the signatures from both
    -- packages and form a new package with both the signatures.
    then Just $ p1 { pkSignatures = pkSignatures p1 ++ pkSignatures p2 }
    else Nothing

mergePackages
  :: NonEmpty Package
  -> Either UnpackageError Package
mergePackages (p :| ps) = maybeToRight PackageMergeFailure $
  foldM mergeSignatures p ps

getBytesToSign :: Package -> Text
getBytesToSign Package{..} = addTezosBytesPrefix pkToSign

type Signable v = (VerPermanent v ~ Empty, Typeable v)

-- | Extract the signable component from package. This is a structure
-- with the packed lambda that represent the action, the multi-sig contract
-- address, and the replay attack prevention counter.
getToSign :: forall (v :: VersionKind). (Signable v) => Package -> Either UnpackageError (ToSign v)
getToSign Package{..} =
  case decodeHex pkToSign of
    Just hexDecoded ->
      case fromVal @(ToSign v) <$> unpackValue' hexDecoded of
        Right toSign -> Right toSign
        Left err -> Left $ UnpackFailure err
    Nothing -> Left HexDecodingFailure

-- | Errors that can happen when package is de-serialized back to the multi-sig
-- contract param
data UnpackageError
  = HexDecodingFailure
  | UnpackFailure UnpackError
  | PackageMergeFailure
  | BadSrcParameterFailure -- If the bundled operation differes from the one that is being signed for
  | UnexpectedParameterWithView

instance Buildable UnpackageError where
  build = \case
    HexDecodingFailure -> "Error decoding hex encoded string"
    PackageMergeFailure -> "Provied packages had different action/enviroments"
    BadSrcParameterFailure -> "ERROR!! The bundled operation does not match the byte sequence that is being signed."
    UnpackFailure err -> build err
    UnexpectedParameterWithView -> "Unexpected parameter with View in package"

instance Show UnpackageError where
  show = pretty

instance Exception UnpackageError where
  displayException = pretty

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
  -> (PublicKey, Signature)
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
  :: forall v. (Signable v)
  => [PublicKey]
  -> NonEmpty Package
  -> Either UnpackageError ((TAddress (MSigParameter v)), ParamMain v)
mkMultiSigParam pks packages = do
  package <- mergePackages packages
  toSign <- getToSign package
  return $ mkParameter toSign (pkSignatures package)
  where
    mkParameter
      :: ToSign v
      -> [(PublicKey, Signature)]
      -> (TAddress (MSigParameter v), ParamMain v)
    mkParameter (address_, payload) sigs =
      -- There should be as may signatures in the submitted request
      -- as there are keys in the contract's storage. Not all keys should
      -- be present, but they should be marked as absent using Nothing values [1].
      -- So we pad the list with Nothings to make up for missing signatures.
      -- [1] https://github.com/murbard/smart-contracts/blob/master/multisig/michelson/generic.tz#L63
      (toTAddress address_, (payload, sortSigs sigs))
    sortSigs :: [(PublicKey, Signature)] -> [Maybe Signature]
    sortSigs sigs = flip lookup sigs <$> pks

encodeToSign :: (Signable v) => ToSign v -> Text
encodeToSign ts = (encodeHex $ lPackValue ts)

deriveJSON (aesonPrefix camelCase) ''Package
