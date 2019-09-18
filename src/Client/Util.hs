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

import Michelson.Interpret.Pack (packValue')
import Michelson.Typed.Haskell.Value (IsoValue(..))

import Client.Error (TzbtcClientError(..))
import Lorentz.Contracts.TZBTC (Parameter(..))

paramToExpression :: Parameter -> Expression
paramToExpression = decode . BS.drop 1 . packValue' . toVal

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
