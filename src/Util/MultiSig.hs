{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.MultiSig
  ( Package(..)
  , addSignature
  , addSignature'
  , decodePackage
  , encodePackage
  , mkPackage
  , mkMultiSigParam
  , getOpDescription
  , mergePackages
  , getBytesToSign
  )
where

import Prelude hiding (drop, toStrict, (>>))

import Data.Aeson (encode, eitherDecodeStrict)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Lazy as LBS (toStrict)
import Data.List (lookup)
import Fmt (Buildable(..), (+|), (|+))
import Text.Hex (encodeHex, decodeHex)

import Lorentz
import Lorentz.Contracts.TZBTC.MultiSig as MSig
import Lorentz.Contracts.TZBTC.Types
import Michelson.Interpret.Pack
import Michelson.Interpret.Unpack
import Michelson.TypeCheck.TypeCheck
import Tezos.Crypto

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

type ParamConstraints parameter =
  ( KnownValue parameter
  , NoOperation parameter
  , IsoValue parameter
  , NoBigMap parameter)

-- | This is the structure that will be serialized and signed on. We are using
-- this particular type because the multisig contract extracts the payload,
-- `ParamPayload` from its parameter, pair it with the contracts address
-- (self), then serialize and check the signatures provided on that serialized
-- data.
type ToSign = (Address, ParamPayload)

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
  , pkUnpackEnv :: !TcOriginatedContracts  -- ^ Maps address to the type of originated contracts.
  , pkSignatures :: ![(PublicKey, Signature)] -- ^ Field to hold signatures and public keys as they are collected.
  }

instance Buildable (PublicKey, Signature) where
  build (pk, sig) = (formatPublicKey pk) |+ (build $ formatSignature sig)

instance Buildable Package where
  build p =
    ("Serialized TZBTC contract operation: " :: String) |+ (pkToSign p) |+ "\n"
    +| ("Decoded operation:" :: String) |+ (getOpDescription p) |+ "\n"
    +| ("Originated contracts:" :: String) |+ (show $ pkUnpackEnv p :: String) |+ "\n"
    +| ("Signatures present" :: String) |+ (pkSignatures p) |+ "\n"

-- | Get Operation description from serialized value
getOpDescription :: Package -> Builder
getOpDescription p = case mkMultiSigParam [] (p :| []) of
  Right param -> build (printLorentzValue True param)
  Left err -> build err

-- | Make the `Package` value from input parameters.
mkPackage
  :: forall parameter. (ParamConstraints parameter, ToUnpackEnv parameter)
  => Address
  -> Natural
  -> ContractAddr parameter
  -> parameter -> Package
mkPackage msigAddress counter tzbtc param
  = let msigLambda = contractToLambda tzbtc param
    in Package
      { pkToSign = encodeToSign $ (msigAddress, (counter, ParamLambda msigLambda))
      , pkUnpackEnv = toUnpackEnv tzbtc param
      , pkSignatures = []
      }

mergeSignatures
  :: Package
  -> Package
  -> Maybe Package
mergeSignatures p1 p2 =
  if (pkToSign p1, pkUnpackEnv p1) == (pkToSign p2, pkUnpackEnv p2)
    then Just $ p1 { pkSignatures = pkSignatures p1 ++ pkSignatures p2 }
    else Nothing

mergePackages
  :: NonEmpty Package
  -> Maybe Package
mergePackages (p :| ps) = foldM mergeSignatures p ps

getBytesToSign :: Package -> Text
getBytesToSign = pkToSign

-- | Errors that can happen when package is de-serialized back to the multi-sig
-- contract param
data UnpackageError
  = HexDecodingFailure
  | UnpackFailure UnpackError
  | PackageMergeFailure

instance Buildable UnpackageError where
  build = \case
    HexDecodingFailure -> "Error decoding hex encoded string"
    PackageMergeFailure -> "Provied packages had different action/enviroments"
    UnpackFailure err -> build err

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

-- | Add signature to the encoded package.
addSignature
  :: ByteString
  -> (Text, Text)
  -> Either String ByteString
addSignature p (pk, sg) = do
  package <- decodePackage p
  case parsePublicKey pk of
    Right pubk -> case parseSignature sg of
      Right sig -> Right $ encodePackage $ addSignature' package (pubk, sig)
      Left err -> Left $ show err
    Left err -> Left $ show err

-- | Add signature to package.
addSignature'
  :: Package
  -> (PublicKey, Signature)
  -> Package
addSignature' package sig = let
  existing = pkSignatures package
  in package { pkSignatures = sig:existing }

-- | Given a value of type `Package`, and a list of public keys,
-- make the actual parameter that the multi-sig contract can be called with.
-- The list of public keys should be in the same order that the contract has
-- them in it's storage.
mkMultiSigParam
  :: [PublicKey]
  -> NonEmpty Package
  -> Either UnpackageError (ContractAddr MSig.Parameter, MSig.Parameter)
mkMultiSigParam pks packages =
  case (mergePackages packages) of
    Just package -> case decodeHex $ pkToSign package of
      Just hexDecoded -> case unpackValue' (UnpackEnv (pkUnpackEnv package)) hexDecoded of
        Right toSignVal ->
          Right $ mkParameter (fromVal toSignVal) (pkSignatures package)
        Left uerr -> Left $ UnpackFailure uerr
      Nothing -> Left HexDecodingFailure
    Nothing -> Left PackageMergeFailure
  where
    mkParameter
      :: ToSign
      -> [(PublicKey, Signature)]
      -> (ContractAddr MSig.Parameter, MSig.Parameter)
    mkParameter (address_, payload) sigs =
      -- There should be as may signatures in the submitted request
      -- as there are keys in the contract's storage. Not all keys should
      -- be present, but they should be marked as absent using Nothing values [1].
      -- So we pad the list with Nothings to make up for missing signatures.
      -- [1] https://github.com/murbard/smart-contracts/blob/master/multisig/michelson/generic.tz#L63
      (ContractAddr address_, ParameterMain (payload, sortSigs sigs))
    sortSigs :: [(PublicKey, Signature)] -> [Maybe Signature]
    sortSigs sigs = flip lookup sigs <$> pks

encodeToSign :: ToSign -> Text
encodeToSign ts = (encodeHex $ packValue' $ toVal ts)

deriveJSON (aesonPrefix camelCase) ''Package
