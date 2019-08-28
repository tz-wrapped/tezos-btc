{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Types
  ( ClientConfig (..)
  , ForgeOperation (..)
  , InternalOperation (..)
  , OperationContent (..)
  , RunError (..)
  , RunMetadata (..)
  , RunOperation (..)
  , RunOperationResult (..)
  , RunRes (..)
  , TransactionOperation (..)
  , combineResults
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.=), (.:), (.:?), (.!=))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Fmt (Buildable(..), (+|), (|+), (+||))
import Tezos.Base16ByteString (Base16ByteString(..))
import Tezos.Micheline
  (Expression(..), MichelinePrimAp(..), MichelinePrimitive(..))
import Tezos.Json (TezosWord64(..))

import Tezos.Address (Address, formatAddress)
import Tezos.Crypto (SecretKey, Signature, encodeBase58Check, formatSignature)

newtype MichelsonExpression = MichelsonExpression Expression
  deriving newtype FromJSON

instance Buildable MichelsonExpression where
  build (MichelsonExpression expr) = case expr of
    Expression_Int i -> build $ unTezosWord64 i
    Expression_String s -> build s
    Expression_Bytes b ->
      build $ encodeBase58Check $ unbase16ByteString b
    Expression_Seq s -> "(" +| buildSeq s |+ ")"
    Expression_Prim (MichelinePrimAp (MichelinePrimitive text) s) ->
      text <> " " |+ "(" +|
      buildSeq s +| ")"
    where
      buildSeq =
        mconcat . intersperse ", " . map
        (build . MichelsonExpression) . toList

data ForgeOperation = ForgeOperation
  { foBranch :: Text
  , foContents :: [TransactionOperation]
  }

instance ToJSON ForgeOperation where
  toJSON ForgeOperation{..} = object
    [ "branch" .= toString foBranch
    , "contents" .= toJSON foContents
    ]

data RunOperation = RunOperation
  { roBranch :: Text
  , roContents :: [TransactionOperation]
  , roSignature :: Signature
  }

instance ToJSON RunOperation where
  toJSON RunOperation{..} = object
    [ "branch" .= toString roBranch
    , "contents" .= toJSON roContents
    , "signature" .= formatSignature roSignature
    ]

data RunRes = RunRes
  { rrOperationContents :: [OperationContent]
  }

instance FromJSON RunRes where
  parseJSON = withObject "preApplyRes" $ \o ->
    RunRes <$> o .: "contents"

data OperationContent = OperationContent RunMetadata

instance FromJSON OperationContent where
  parseJSON = withObject "operationCostContent" $ \o ->
    OperationContent <$> o .: "metadata"

data RunMetadata = RunMetadata
  { rmOperationResult :: RunOperationResult
  , rmInternalOperationResults :: [InternalOperation]
  }

instance FromJSON RunMetadata where
  parseJSON = withObject "metadata" $ \o ->
    RunMetadata <$> o .: "operation_result" <*>
    o .:? "internal_operation_results" .!= []

newtype InternalOperation = InternalOperation
  { unInternalOperation :: RunOperationResult }

instance FromJSON InternalOperation where
  parseJSON = withObject "internal_operation" $ \o ->
    InternalOperation <$> o .: "result"

data RunError
  = RunErrorId Text Text
  | RunErrorValue Expression

instance FromJSON RunError where
  parseJSON = withObject "preapply error" $ \o -> asum
    [ RunErrorId <$> o .: "contract_handle" <*> o .: "id"
    , RunErrorValue <$> o .: "with"
    ]

instance Buildable RunError where
  build (RunErrorId contractHandle id') =
    "Pre apply for contract '" +| contractHandle |+
    "' failed with error: " +| id' |+ ""
  build (RunErrorValue expr) =
    "Pre apply failed with value '" +|| expr ||+ "'"

data RunOperationResult
  = RunOperationApplied TezosWord64 TezosWord64
  | RunOperationFailed [RunError]

combineResults :: RunOperationResult -> RunOperationResult -> RunOperationResult
combineResults (RunOperationApplied c1 c2) (RunOperationApplied c3 c4) =
  RunOperationApplied (c1 + c3) (c2 + c4)
combineResults (RunOperationApplied _ _) (RunOperationFailed e) =
  RunOperationFailed e
combineResults (RunOperationFailed e) (RunOperationApplied _ _) =
  RunOperationFailed e
combineResults (RunOperationFailed e1) (RunOperationFailed e2) =
  RunOperationFailed $ e1 <> e2

instance FromJSON RunOperationResult where
  parseJSON = withObject "operation_costs" $ \o -> do
    status <- o .: "status"
    case status of
      "applied" -> RunOperationApplied <$> o .: "consumed_gas" <*> o .: "storage_size"
      "failed" -> RunOperationFailed <$> o .: "errors"
      _ -> fail ("unexpected status " ++ status)

data TransactionOperation = TransactionOperation
  { source :: Address
  , fee :: TezosWord64
  , counter :: TezosWord64
  , gasLimit :: TezosWord64
  , storageLimit :: TezosWord64
  , amount :: TezosWord64
  , destination :: Address
  , parameters :: Expression
  }

instance ToJSON TransactionOperation where
  toJSON TransactionOperation {..} = object
    [ "kind" .= ("transaction" :: String)
    , "source" .= formatAddress source
    , "fee" .= toJSON fee
    , "counter" .= toJSON counter
    , "gas_limit" .= toJSON gasLimit
    , "storage_limit" .= toJSON storageLimit
    , "amount" .= toJSON amount
    , "destination" .= formatAddress destination
    , "parameters" .= toJSON parameters
    ]

data ClientConfig = ClientConfig
  { ccNodeAddress :: Text
  , ccNodePort :: Int
  , ccContractAddress :: Address
  , ccUserAddress :: Address
  , ccUserSecretKey :: SecretKey
  } deriving (Generic)

deriveJSON defaultOptions ''ClientConfig
