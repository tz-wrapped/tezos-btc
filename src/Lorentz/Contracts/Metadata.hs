{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -Wno-orphans #-}

module Lorentz.Contracts.Metadata
  ( TokenId
  , TokenMetadata(..)
  , mkTokenMetadata
  , parseSingleTokenMetadata
  , singleTokenTokenId
  , singleTokenResolveMetadata
  ) where

import Fmt (Buildable(..), genericF)
import Named
import qualified Options.Applicative as Opt

import Prelude hiding (drop, map, (>>))

import Lorentz
import Util.CLI
import Util.Named

import Michelson.Text.Orphans ()


type TokenId = Natural

-- | See TZIP-12 for more info:
-- https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-12/tzip-12.md
data TokenMetadata =
  TokenMetadata
    { metadata_tokenId :: TokenId
    , metadata_rest1 :: TokenMetadataRest1
    }
  deriving stock (Eq, Generic, Read)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc TokenMetadata where
  typeDocName _ = "TokenMetadata"
  typeDocMdDescription = "Type which defines metadata of the token."
  typeDocMdReference tp =
    customTypeDocMdReference ("TokenMetadata", DType tp) []
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

instance Buildable TokenMetadata where build = genericF


data TokenMetadataRest1 =
  TokenMetadataRest1
    { metadata_symbol :: MText
    , metadata_rest2 :: TokenMetadataRest2
    }
  deriving stock (Eq, Generic, Read)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc TokenMetadataRest1 where
  typeDocName _ = "TokenMetadataRest1"
  typeDocMdDescription =
    "Type which defines 1st continuation of metadata of the token."
  typeDocMdReference tp =
    customTypeDocMdReference ("TokenMetadataRest1", DType tp) []
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

instance Buildable TokenMetadataRest1 where build = genericF

data TokenMetadataRest2 =
  TokenMetadataRest2
    { metadata_name :: MText
    , metadata_rest3 :: TokenMetadataRest3
    }
  deriving stock (Eq, Generic, Read)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc TokenMetadataRest2 where
  typeDocName _ = "TokenMetadataRest2"
  typeDocMdDescription =
    "Type which defines 2nd continuation of metadata of the token."
  typeDocMdReference tp =
    customTypeDocMdReference ("TokenMetadataRest2", DType tp) []
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

instance Buildable TokenMetadataRest2 where build = genericF

data TokenMetadataRest3 =
  TokenMetadataRest3
    { metadata_decimals :: Natural
    , metadata_extras :: Map MText MText
    }
  deriving stock (Eq, Generic, Read)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc TokenMetadataRest3 where
  typeDocName _ = "TokenMetadataRest3"
  typeDocMdDescription =
    "Type which defines 3rd continuation of metadata of the token."
  typeDocMdReference tp =
    customTypeDocMdReference ("TokenMetadataRest3", DType tp) []
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

instance Buildable TokenMetadataRest3 where build = genericF


-- | Construct `TokenMetadata` from @tokenid symbol name decimals extras@
mkTokenMetadata ::
     ("tokenId" :! TokenId)
  -> ("symbol" :! MText)
  -> ("name" :! MText)
  -> ("decimals" :! Natural)
  -> ("extras" :! Map MText MText)
  -> TokenMetadata
mkTokenMetadata tokenid symbol name decimals extras =
  TokenMetadata
    { metadata_tokenId = arg #tokenId tokenid
    , metadata_rest1 =
        TokenMetadataRest1
          { metadata_symbol = arg #symbol symbol
          , metadata_rest2 =
              TokenMetadataRest2
                { metadata_name = arg #name name
                , metadata_rest3 =
                    TokenMetadataRest3
                      { metadata_decimals = arg #decimals decimals
                      , metadata_extras = arg #extras extras
                      }
                }
          }
    }

-- | The `TokenId` of a token in a single-token contract is @0@
singleTokenTokenId :: TokenId
singleTokenTokenId = 0

-- | Parse `TokenMetadata` for a single token, with no extras
parseSingleTokenMetadata :: Opt.Parser TokenMetadata
parseSingleTokenMetadata =
  mkTokenMetadata <$>
  pure (#tokenId .! singleTokenTokenId) <*>
  ((#symbol .!) . arg (Name @"token-symbol") <$> namedParser (Just [mt|TZBTC|]) "token-symbol") <*>
  ((#name .!) . arg (Name @"token-name") <$> namedParser (Just [mt|Tezos BTC|]) "token-name") <*>
  namedParser (Just 0) "token-decimals" <*>
  pure (#extras .! mempty)

-- | Assert that each `TokenId` is @0@ (`singleTokenTokenId`) and replace each one in the input
-- list with the given `TokenMetadata`
singleTokenResolveMetadata ::
     [TokenId] & TokenMetadata & s :-> [TokenMetadata] & s
singleTokenResolveMetadata = do
  map $ do
    push singleTokenTokenId
    assertEq $ CustomError #invalidSingleTokenId
    dup
  dip drop

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------
-- | For the get `TokenMetadata` entry point,
-- if the `TokenId` is not @0@ (the only valid `TokenId` for a single token).
type instance ErrorArg "invalidSingleTokenId" = ()

instance CustomErrorHasDoc "invalidSingleTokenId" where
  customErrClass = ErrClassBadArgument
  customErrDocMdCause = "The only valid token id is 0"
