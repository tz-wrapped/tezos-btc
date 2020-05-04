{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Client.API
  ( forgeOperation
  , getBlockConstants
  , getFromBigMap
  , getCounter
  , getStorage
  , getLastBlock
  , getChainId
  , injectOperation
  , preApplyOperations
  , runOperation
  ) where

import Servant.API
  (Capture, Get, JSON, Post, ReqBody, QueryParam, (:>), (:<|>)(..))
import Servant.Client (ClientM, client)
import Tezos.Common.Json (TezosInt64)
import Tezos.V005.Micheline (Expression)

import Michelson.Untyped (InternalByteString(..))

import Client.Types

type NodeAPI =
  "chains" :> "main" :> "blocks" :> "head" :> "helpers" :> "forge" :> "operations"
  :> ReqBody '[JSON] ForgeOperation :> Post '[JSON] InternalByteString :<|>
  "chains" :> "main" :> "blocks" :> "head" :> "hash"
  :> Get '[JSON] Text :<|>
  "injection" :> "operation" :> QueryParam "chain" Text :> ReqBody '[JSON] Text
  :> Post '[JSON] Text :<|>
  "chains" :> "main" :> "blocks" :> "head" :> "context" :> "contracts"
  :> Capture "contract" Text :> "counter" :> Get '[JSON] TezosInt64 :<|>
  "chains" :> "main" :> "blocks" :> "head" :> "helpers" :> "scripts" :> "run_operation"
  :> ReqBody '[JSON] RunOperation :> Post '[JSON] RunRes :<|>
  "chains" :> "main" :> "blocks" :> "head" :> "context" :> "contracts"
  :> Capture "contract" Text :> "storage" :> Get '[JSON] Expression :<|>
  "chains" :> "main" :> "blocks" :> Capture "block_id" Text :> Get '[JSON] BlockConstants :<|>
  "chains" :> "main" :> "blocks" :> "head" :> "context" :> "big_maps" :> Capture "big_map_id" Natural
  :> Capture "script_expr" Text :> Get '[JSON] Expression :<|>
  "chains" :> "main" :> "blocks" :> "head" :> "helpers" :> "preapply" :> "operations"
  :> ReqBody '[JSON] [PreApplyOperation] :> Post '[JSON] [RunRes] :<|>
  "chains" :> Capture "chain_id" Text :> "chain_id" :> Get '[JSON] Text


nodeAPI :: Proxy NodeAPI
nodeAPI = Proxy

getLastBlock :: ClientM Text
forgeOperation :: ForgeOperation -> ClientM InternalByteString
injectOperation :: Maybe Text -> Text -> ClientM Text
getCounter :: Text -> ClientM TezosInt64
runOperation :: RunOperation -> ClientM RunRes
getStorage :: Text -> ClientM Expression
getBlockConstants :: Text -> ClientM BlockConstants
getFromBigMap :: Natural -> Text -> ClientM Expression
preApplyOperations :: [PreApplyOperation] -> ClientM [RunRes]
getChainId :: Text -> ClientM Text
forgeOperation :<|>
  getLastBlock :<|>
  injectOperation :<|>
  getCounter :<|>
  runOperation :<|>
  getStorage :<|>
  getBlockConstants :<|>
  getFromBigMap :<|>
  preApplyOperations :<|>
  getChainId = client nodeAPI
