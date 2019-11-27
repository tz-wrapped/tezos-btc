{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Util
  ( addTezosBytesPrefix
  , calcFees
  , convertTypeToExpression
  , exprToValue
  , mkOriginationScript
  , nicePackedValueToExpression
  , throwClientError
  , throwLeft
  , typeToExpression
  , valueToScriptExpr
  ) where

import qualified Data.ByteString as BS (cons, drop, pack)
import Data.Sequence (fromList)
import Data.Singletons (SingI)
import Servant.Client.Core (ClientError)
import Tezos.Common.Binary (encode, decode)
import Tezos.Common.Json (TezosInt64)
import Tezos.V005.Micheline
  (Annotation(..), Expression(..), MichelinePrimAp(..), MichelinePrimitive(..))

import Lorentz (Contract, compileLorentz, compileLorentzContract)
import Lorentz.Constraints (NicePackedValue, NiceUnpackedValue)
import Lorentz.UStore.Types (UStore(..))
import Lorentz.Pack (lPackValue, lUnpackValue)
import Michelson.Interpret.Pack (packT', packCode')
import Michelson.Interpret.Unpack (UnpackError)
import Michelson.Typed (Instr, convertFullContract)
import Michelson.Typed.Haskell.Value (BigMap(..), IsoValue(..))
import Michelson.Untyped.Annotation (Annotation (..), FieldAnn, TypeAnn, noAnn)
import Michelson.Untyped.Contract (Contract'(..))
import qualified Michelson.Untyped.Type as U (Comparable(..), CT(..), T(..), Type(..))
import Tezos.Crypto (blake2b)

import Client.Error (TzbtcClientError(..))
import Client.Types
import Lorentz.Contracts.TZBTC.Types (Storage(..))
import Lorentz.Contracts.TZBTC.V0 (Interface, Parameter(..), StoreTemplateV0, UStoreV0)

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
  :: Contract (Parameter Interface StoreTemplateV0) UStoreV0 -> UStoreV0 -> OriginationScript
mkOriginationScript contract Storage{..} = OriginationScript
  { osCode = Expression_Seq $ fromList
    [ Expression_Prim $
      MichelinePrimAp (MichelinePrimitive "parameter")
      (fromList [convertTypeToExpression $ para untypedContract])
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
  where
    untypedContract = convertFullContract $ compileLorentzContract contract

calcFees :: TezosInt64 -> TezosInt64 -> TezosInt64
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


-- | Function to convert `Untyped.Type` to Micheline `Expression`.
--
-- We cannot simply use `typeToExpression` for parameter type of our contract
-- because Morley typed representation doesn't preserve annotations. However,
-- after compiling and untyping Lorentz contract, parameter and storage represented
-- as `Untyped.Type` has required annotations.
--
-- We use this function after compiling and converting the contract to
-- untyped version, that's why `TypeParameter` and `TypeStorage` won't
-- appear as an argument and we use `error` when we find them.
convertTypeToExpression :: HasCallStack => U.Type -> Expression
convertTypeToExpression type_ = case type_ of
  (U.Type t_ typeAnn_) -> convertTToExpression t_ typeAnn_ noAnn
  _ -> failWithError
  where
    failWithError = error "Cannot convert implicit type"
    convertTToExpression :: HasCallStack => U.T -> TypeAnn -> FieldAnn -> Expression
    convertTToExpression t typeAnn fieldAnn = case t of
      U.Tc ct -> convertCTToExpression ct typeAnn fieldAnn
      U.TKey -> mkLeafPrim "key"
      U.TUnit -> mkLeafPrim "unit"
      U.TSignature -> mkLeafPrim "signature"
      U.TChainId -> mkLeafPrim "chain_id"
      U.TOption (U.Type t' typeAnn') -> mkExpressionPrim "option"
                   [convertTToExpression t' typeAnn' noAnn] typeAnn fieldAnn
      U.TList (U.Type t' typeAnn') -> mkExpressionPrim "list"
                   [convertTToExpression t' typeAnn' noAnn] typeAnn fieldAnn
      U.TSet (U.Comparable ct' typeAnn') -> mkExpressionPrim "set"
                   [convertCTToExpression ct' typeAnn' noAnn] typeAnn fieldAnn
      U.TOperation -> mkLeafPrim "operation"
      U.TContract (U.Type t' typeAnn') ->
        mkExpressionPrim "contract" [convertTToExpression t' typeAnn' noAnn]
        typeAnn fieldAnn
      U.TPair fieldAnn1 fieldAnn2 (U.Type t1 typeAnn1) (U.Type t2 typeAnn2) ->
        mkExpressionPrim "pair"
        [ convertTToExpression t1 typeAnn1 fieldAnn1
        , convertTToExpression t2 typeAnn2 fieldAnn2
        ] typeAnn fieldAnn
      U.TOr fieldAnn1 fieldAnn2 (U.Type t1 typeAnn1) (U.Type t2 typeAnn2) ->
        mkExpressionPrim "or"
        [ convertTToExpression t1 typeAnn1 fieldAnn1
        , convertTToExpression t2 typeAnn2 fieldAnn2
        ] typeAnn fieldAnn
      U.TLambda (U.Type t1 typeAnn1) (U.Type t2 typeAnn2) ->
        mkExpressionPrim "lambda"
        [ convertTToExpression t1 typeAnn1 noAnn
        , convertTToExpression t2 typeAnn2 noAnn
        ] typeAnn fieldAnn
      U.TMap (U.Comparable ct1 typeAnn1) (U.Type t2 typeAnn2) ->
        mkExpressionPrim "map"
        [ convertCTToExpression ct1 typeAnn1 noAnn
        , convertTToExpression t2 typeAnn2 noAnn
        ] typeAnn fieldAnn
      U.TBigMap (U.Comparable ct1 typeAnn1) (U.Type t2 typeAnn2) ->
        mkExpressionPrim "big_map"
        [ convertCTToExpression ct1 typeAnn1 noAnn
        , convertTToExpression t2 typeAnn2 noAnn
        ] typeAnn fieldAnn
      U.TOption _ -> failWithError
      U.TList _ -> failWithError
      U.TContract _ -> failWithError
      U.TPair {} -> failWithError
      U.TOr {} -> failWithError
      U.TLambda {} -> failWithError
      U.TMap {} -> failWithError
      U.TBigMap {} -> failWithError
      where
        mkLeafPrim :: Text -> Expression
        mkLeafPrim prim = mkExpressionPrim prim [] typeAnn fieldAnn
    convertCTToExpression :: U.CT -> TypeAnn -> FieldAnn -> Expression
    convertCTToExpression ct typeAnn fieldAnn = case ct of
      U.CInt -> mkLeafPrim "int"
      U.CNat -> mkLeafPrim "nat"
      U.CString -> mkLeafPrim "string"
      U.CMutez -> mkLeafPrim "mutez"
      U.CBool -> mkLeafPrim "bool"
      U.CKeyHash -> mkLeafPrim "key_hash"
      U.CTimestamp -> mkLeafPrim "timestamp"
      U.CBytes -> mkLeafPrim "bytes"
      U.CAddress -> mkLeafPrim "address"
      where
        mkLeafPrim :: Text -> Expression
        mkLeafPrim prim = mkExpressionPrim prim [] typeAnn fieldAnn
    mkExpressionPrim :: Text -> [Expression] -> TypeAnn -> FieldAnn -> Expression
    mkExpressionPrim prim childs typeAnn@(Annotation typeAnnText) fieldAnn@(Annotation fieldAnnText) =
      Expression_Prim $ MichelinePrimAp (MichelinePrimitive prim) (fromList childs)
      (fromList $
       (bool [Annotation_Type typeAnnText] [] (typeAnn == noAnn)) <>
       (bool [Annotation_Field fieldAnnText] [] (fieldAnn == noAnn))
      )
