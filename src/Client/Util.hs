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
  , throwLeft
  , valueToScriptExpr
  ) where

import qualified Data.ByteString as BS (cons, drop, pack)
import Data.Sequence (fromList)
import Data.Singletons (SingI)
import Servant.Client.Core (ClientError)
import Tezos.Binary (encode, decode)
import Tezos.Json (TezosWord64)
import Tezos.Micheline (Expression(..), MichelinePrimAp(..), MichelinePrimitive(..))

import Lorentz (Contract, compileLorentz)
import Lorentz.Constraints (NicePackedValue, NiceUnpackedValue)
import Lorentz.UStore.Types (UStore(..))
import Lorentz.Pack (lPackValue, lUnpackValue)
import Michelson.Interpret.Pack (packT', packCode')
import Michelson.Interpret.Unpack (UnpackError)
import Michelson.Typed (Instr)
import Michelson.Typed.Haskell.Value (BigMap(..), IsoValue(..))
import Tezos.Crypto (blake2b)

import Client.Error (TzbtcClientError(..))
import Client.Types
import Lorentz.Contracts.TZBTC.Types (Storage(..))
import Lorentz.Contracts.TZBTC.V0 (Interface, Parameter(..), UStoreV0)

nicePackedValueToExpression
  :: forall param.
     (NicePackedValue param)
  => param -> Expression
nicePackedValueToExpression param = decode . BS.drop 1 $ lPackValue param

typeToExpression :: forall t. (SingI (ToT t)) => Expression
typeToExpression = decode $ packT' @(ToT t)

codeToExpression :: Instr inp out -> Expression
codeToExpression = decode . packCode'

mkOriginationScript
  :: Contract (Parameter Interface) UStoreV0 -> UStoreV0 -> OriginationScript
mkOriginationScript contract Storage{..} = OriginationScript
  { osCode = Expression_Seq $ fromList
    [ Expression_Prim $
      MichelinePrimAp (MichelinePrimitive "parameter")
      (fromList [typeToExpression @(Parameter Interface)])
      (fromList [])
    , Expression_Prim $
      MichelinePrimAp (MichelinePrimitive "storage")
      (fromList [typeToExpression @UStoreV0])
      (fromList [])
    , Expression_Prim $
      MichelinePrimAp (MichelinePrimitive "code")
      (fromList [codeToExpression $ compileLorentz contract])
      (fromList [])
    ]
  -- Here we assume, that the storage has the following structure:
  -- (big_map, fields)
  , osStorage = Expression_Prim $
    MichelinePrimAp (MichelinePrimitive "Pair") (fromList
    -- Here is another hack. We cannot pack big_map, but JSON representation
    -- (and thus Micheline representation) of big_map and basic map don't differ,
    -- so we convert basic map (made from our big_map) to Expression here.
    [ nicePackedValueToExpression $ (unBigMap . unUStore) dataMap
    , nicePackedValueToExpression fields
    ]) (fromList [])
  }

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
