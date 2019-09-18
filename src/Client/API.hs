{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.API
  ( forgeOperation
  , getCounter
  , getLastBlock
  , injectOperation
  , runOperation
  ) where

import Servant.API
  (Capture, Get, JSON, Post, ReqBody, QueryParam, (:>), (:<|>)(..))
import Servant.Client (ClientM, client)
import Tezos.Json (TezosWord64)

import Michelson.Untyped (InternalByteString(..))

import Client.Types

type NodeAPI =
  "chains/main/blocks/head/helpers/forge/operations"
  :> ReqBody '[JSON] ForgeOperation :> Post '[JSON] InternalByteString :<|>
  "chains/main/blocks/head/hash"
  :> Get '[JSON] Text :<|>
  "injection/operation" :> QueryParam "chain" Text :> ReqBody '[JSON] Text
  :> Post '[JSON] Text :<|>
  "chains/main/blocks/head/context/contracts"
  :> Capture "contract" Text :> "counter" :> Get '[JSON] TezosWord64 :<|>
  "chains/main/blocks/head/helpers/scripts/run_operation"
  :> ReqBody '[JSON] RunOperation :> Post '[JSON] RunRes


nodeAPI :: Proxy NodeAPI
nodeAPI = Proxy

getLastBlock :: ClientM Text
forgeOperation :: ForgeOperation -> ClientM InternalByteString
injectOperation :: Maybe Text -> Text -> ClientM Text
getCounter :: Text -> ClientM TezosWord64
runOperation :: RunOperation -> ClientM RunRes
forgeOperation :<|>
  getLastBlock :<|>
  injectOperation :<|>
  getCounter :<|>
  runOperation = client nodeAPI
