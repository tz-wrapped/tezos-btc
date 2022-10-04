-- SPDX-FileCopyrightText: 2022 Bitcoin Suisse
-- SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse

{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO: Remove this module when upgrading to the next morley release.
-- | Provides 'L1AddressOrAlias'
module CLI.L1AddressOrAlias
  ( L1AddressOrAlias(..)
  , resolveL1AddressMaybe
  , l1AddressOrAliasOption
  ) where

import Data.Constraint ((\\))
import Data.Singletons (SingI(..), demote)
import Data.Text
import Fmt
import Options.Applicative qualified as Opt

import Morley.Client
import Morley.Tezos.Address
import Morley.Tezos.Address.Alias
import Morley.Tezos.Address.Kinds
import Morley.Util.CLI
import Morley.Util.Interpolate (itu)
import Morley.Util.Named

-- | A version of 'AddressOrAlias' that can be either a contract or an implicit
-- address/alias.
--
-- The existence of this type is necessitated by the fact that while address
-- kind is always known, alias kind might not be until it's resolved.
data L1AddressOrAlias where
  L1AOAKindSpecified :: L1AddressKind kind => AddressOrAlias kind -> L1AddressOrAlias
  -- ^ Address or alias with a known kind.
  L1AOAKindUnspecified :: Text -> L1AddressOrAlias
  -- ^ Alias with unknown kind.

deriving stock instance Show L1AddressOrAlias

-- | Try parsing an alias with an explicit kind prefix.
parseKindedAlias :: forall kind. (L1AddressKind kind, SingI kind) => Text -> Maybe (Alias kind)
parseKindedAlias = fmap mkAlias . stripPrefix (pretty (demote @kind) <> ":")

instance HasCLReader L1AddressOrAlias where
  getReader = do
    addrOrAlias <- Opt.str
    case parseAddress addrOrAlias of
      Right (MkAddress (addr :: KindedAddress kind')) -> case addr of
        ImplicitAddress{} -> pure $ L1AOAKindSpecified $ AddressResolved addr
        ContractAddress{} -> pure $ L1AOAKindSpecified $ AddressResolved addr
        TxRollupAddress{} ->
          Opt.readerError $ pretty $ nameF "Unexpected address kind" $
            "expected contract or implicit address, but got tansaction rollup"
      Left _
        | Just alias <- parseKindedAlias @'AddressKindContract addrOrAlias
        -> pure $ L1AOAKindSpecified $ AddressAlias alias
        | Just alias <- parseKindedAlias @'AddressKindImplicit addrOrAlias
        -> pure $ L1AOAKindSpecified $ AddressAlias alias
        | otherwise
        -> pure $ L1AOAKindUnspecified addrOrAlias
  getMetavar = "CONTRACT OR IMPLICIT ADDRESS OR ALIAS"

instance Buildable L1AddressOrAlias where
  build = \case
    L1AOAKindSpecified (AddressAlias (alias :: Alias kind))
      -> (build (demote @kind) \\ aliasKindSanity alias) <> ":" +| alias |+ ""
    L1AOAKindSpecified (AddressResolved address) -> build address
    L1AOAKindUnspecified alias -> build alias

-- | Exception thrown on ambiguous address alias.
data AmbiguousAddressAlias = AmbiguousAddressAlias Text ContractAddress ImplicitAddress
  deriving stock Show

instance Exception AmbiguousAddressAlias where
  displayException = pretty

instance Buildable AmbiguousAddressAlias where
  build (AmbiguousAddressAlias aliasText contractAddr implicitAddr) = [itu|
    The alias '#{aliasText}' is assigned to both:
      * a contract address: #{contractAddr}
      * and an implicit address: #{implicitAddr}
    Use '#{contractPrefix}:#{aliasText}' or '#{implicitPrefix}:#{aliasText}' to disambiguate.
    |]

-- | Try to resolve 'L1AddressOrAlias' to an 'L1Address'.
resolveL1AddressMaybe
  :: forall m. (MonadThrow m, HasTezosClient m)
  => L1AddressOrAlias -> m (Maybe L1Address)
resolveL1AddressMaybe = \case
  L1AOAKindSpecified x -> fmap MkConstrainedAddress <$> resolveAddressMaybe x
  L1AOAKindUnspecified x -> do
    implicit <- resolveAddressMaybe $ AddressAlias $ mkAlias @'AddressKindImplicit x
    contract <- resolveAddressMaybe $ AddressAlias $ mkAlias @'AddressKindContract x
    whenJust implicit $ \implicit' -> whenJust contract $ \contract' ->
      throwM $ AmbiguousAddressAlias x contract' implicit'
    pure $ (MkConstrainedAddress <$> implicit) <|> (MkConstrainedAddress <$> contract)

-- | Parser for 'L1AddressOrAlias'
l1AddressOrAliasOption
  :: Maybe L1AddressOrAlias
  -> "name" :! String
  -> "help" :! String
  -> Opt.Parser L1AddressOrAlias
l1AddressOrAliasOption dflt name (arg #help -> help) =
  mkCLOptionParser dflt name $ #help :!
    [itu|#{help}. When using an alias that is assigned to both a contract and an implicit account,
      use the prefix '#{contractPrefix}:' or '#{implicitPrefix}:' to disambiguate.|]

contractPrefix, implicitPrefix :: Builder
contractPrefix = build AddressKindContract
implicitPrefix = build AddressKindImplicit

instance HasCLReader L1Address where
  getMetavar = "CONTRACT OR IMPLICIT ADDRESS"
  getReader = do
    addrStr <- Opt.str
    case parseAddress addrStr of
      Right (MkAddress (addr :: KindedAddress kind')) -> case addr of
        ImplicitAddress{} -> pure $ MkConstrainedAddress addr
        ContractAddress{} -> pure $ MkConstrainedAddress addr
        TxRollupAddress{} ->
          Opt.readerError $ pretty $ nameF "Unexpected address kind" $
            "expected contract or implicit address, but got tansaction rollup"
      Left err -> Opt.readerError $ pretty err
