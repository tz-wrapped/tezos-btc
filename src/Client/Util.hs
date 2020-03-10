{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Util
  ( addTezosBytesPrefix
  , calcFees
  , exprToValue
  , mkOriginationScript
  , nicePackedValueToExpression
  , throwClientError
  , throwClientErrorAfterRetry
  , throwLeft
  , typeToExpression
  , valueToScriptExpr
  ) where

import qualified Data.ByteString as BS (cons, drop, pack)
import Data.Constraint ((\\))
import Data.Sequence (fromList)
import Data.Singletons (SingI)
import Servant.Client.Core (ClientError)
import Tezos.Common.Binary (decode, encode)
import Tezos.Common.Json (TezosInt64)
import Tezos.V005.Micheline (Expression(..), MichelinePrimAp(..), MichelinePrimitive(..))

import Lorentz (ContractCode, compileLorentzContract)
import Lorentz.Constraints
import Lorentz.Pack (lPackValue, lUnpackValue)
import Michelson.Interpret.Pack (encodeValue', packCode', packNotedT')
import Michelson.Interpret.Unpack (UnpackError)
import Michelson.Typed (FullContract(..), Instr, Notes, unParamNotes)
import Michelson.Typed.Haskell.Value (IsoValue(..))
import Tezos.Crypto (blake2b)

import Client.Error (TzbtcClientError(..))
import Client.Types

nicePackedValueToExpression
  :: forall param.
     (NicePackedValue param)
  => param -> Expression
nicePackedValueToExpression param = decode . BS.drop 1 $ lPackValue param

typeToExpression :: forall t. (SingI t) => Notes t -> Expression
typeToExpression = decode . packNotedT'

codeToExpression :: Instr inp out -> Expression
codeToExpression = decode . packCode'

lEncodeValue' :: forall a. NicePrintedValue a => a -> ByteString
lEncodeValue' = encodeValue' . toVal \\ nicePrintedValueEvi @a

mkOriginationScript
  :: forall cp st. (NiceParameterFull cp, NiceStorage st)
  => ContractCode cp st -> st -> OriginationScript
mkOriginationScript contract storage = OriginationScript
  { osCode = Expression_Seq $ fromList
    [ Expression_Prim $
      MichelinePrimAp (MichelinePrimitive "parameter")
      (fromList [typeToExpression $ unParamNotes $ fcParamNotesSafe compiledContract])
      (fromList [])
    , Expression_Prim $
      MichelinePrimAp (MichelinePrimitive "storage")
      (fromList [typeToExpression $ fcStoreNotes compiledContract])
      (fromList [])
    , Expression_Prim $
      MichelinePrimAp (MichelinePrimitive "code")
      (fromList [codeToExpression $ fcCode compiledContract])
      (fromList [])
    ]
  -- Here we assume, that the storage has the following structure:
  -- (big_map, fields)
  , osStorage = decode . lEncodeValue' $ storage
  }
  where
    compiledContract = compileLorentzContract contract

calcFees :: TezosInt64 -> TezosInt64 -> TezosInt64
calcFees consumedGas storageSize =
  minimalFees +
  (computeFeesForGas consumedGas) +
  minimalTezPerStorage * storageSize
  where
    minimalFees = 100
    computeFeesForGas gas = div (gas + 9) 10
      -- Assuming tez per unit of gas to be 0.1 MuTez
      -- this is essentially doing (ceiling $ gas * 0.1)
    minimalTezPerStorage = 10

throwClientError :: MonadThrow m => m (Either ClientError a) -> m a
throwClientError =
  (>>= \case
      Left e -> throwM $ TzbtcServantError e
      Right x -> return x)

type RetrySpec m = (Int, m ())

throwClientErrorAfterRetry :: MonadThrow m => RetrySpec m -> m (Either ClientError a) -> m a
throwClientErrorAfterRetry (retryfor, delayFn) action = do
  r <- action
  case r of
    Left e -> if retryfor > 0 then do
      delayFn
      throwClientErrorAfterRetry ((retryfor - 1), delayFn) action else throwM $ TzbtcServantError e
    Right x -> return x

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
