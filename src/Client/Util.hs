{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Util
  ( calcFees
  , paramToExpression
  , throwClientError
  , throwLeft
  ) where

import qualified Data.ByteString as BS (drop)
import Servant.Client.Core (ClientError)
import Tezos.Binary (decode)
import Tezos.Json (TezosWord64)
import Tezos.Micheline (Expression)

import Lorentz.Constraints (KnownValue, NoBigMap, NoOperation)
import Michelson.Interpret.Pack (packValue')
import Michelson.Typed.Haskell.Value (IsoValue(..))
import Michelson.Typed.Scope (forbiddenBigMap, forbiddenOp)

import Client.Error (TzbtcClientError(..))

paramToExpression
  :: forall param.
     (IsoValue param, KnownValue param, NoBigMap param, NoOperation param)
  => param -> Expression
paramToExpression param = decode . BS.drop 1 $
  forbiddenOp @(ToT param) $ forbiddenBigMap @(ToT param) $
  packValue' $ toVal param

calcFees :: TezosWord64 -> TezosWord64 -> TezosWord64
calcFees consumedGas storageSize =
  minimalFees +
  minimalTezPerGas * consumedGas +
  minimalTezPerStorage * storageSize
  where
    minimalFees = 100
    minimalTezPerGas = 1
    minimalTezPerStorage = 10

throwClientError :: MonadThrow m => m (Either ClientError a) -> m a
throwClientError =
  (>>= \case
      Left e -> throwM $ TzbtcServantError e
      Right x -> return x)
throwLeft :: (MonadThrow m, Exception e) => m (Either e a) -> m a
throwLeft =
  (>>= \case
      Left e -> throwM e
      Right x -> return x)
