{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Util
  ( addTezosBytesPrefix
  , calcFees
  , exprToValue
  , paramToExpression
  , throwClientError
  , throwLeft
  , valueToScriptExpr
  ) where

import qualified Data.ByteString as BS (cons, drop, pack)
import Servant.Client.Core (ClientError)
import Tezos.Binary (encode, decode)
import Tezos.Json (TezosWord64)
import Tezos.Micheline (Expression)

import Lorentz.Constraints
  (NicePackedValue, NiceUnpackedValue)
import Lorentz.Pack (lPackValue, lUnpackValue)
import Michelson.Interpret.Unpack (UnpackError)
import Tezos.Crypto (blake2b)

import Client.Error (TzbtcClientError(..))

paramToExpression
  :: forall param.
     (NicePackedValue param)
  => param -> Expression
paramToExpression param = decode . BS.drop 1 $
  lPackValue param

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

addTezosBytesPrefix :: Text -> Text
addTezosBytesPrefix = ("0x" <>)

exprToValue
  :: forall t. (NiceUnpackedValue t)
  => Expression -> Either UnpackError t
exprToValue =
  lUnpackValue . BS.cons 0x05 . encode

addExprPrefix :: ByteString -> ByteString
addExprPrefix = (BS.pack [0x0D, 0x2C, 0x40, 0x1B] <>)


-- | This function transforms Lorentz value into `script_expr`.
--
-- `script_expr` is used in RPC as an argument in entrypoint
-- designed for getting value by key from the big_map in Babylon.
-- In order to convert value to the `script_expr` we have to pack it,
-- take blake2b hash and add specific `expr` prefix. Take a look at
-- https://gitlab.com/tezos/tezos/blob/6e25ae8eb385d9975a30388c7a7aa2a9a65bf184/src/proto_005_PsBabyM1/lib_protocol/script_expr_hash.ml
-- and https://gitlab.com/tezos/tezos/blob/6e25ae8eb385d9975a30388c7a7aa2a9a65bf184/src/proto_005_PsBabyM1/lib_protocol/contract_services.ml#L136
-- for more information.
valueToScriptExpr
  :: forall t. (NicePackedValue t)
  => t -> ByteString
valueToScriptExpr = addExprPrefix . blake2b . lPackValue
