{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Client.IO.HttpClient
  ( getClientEnv
  ) where

import Network.HTTP.Client (ManagerSettings(..), Request(..), defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (BaseUrl(..), ClientEnv, Scheme(..), mkClientEnv)

import Client.Types

getClientEnv :: ClientConfig -> IO ClientEnv
getClientEnv ClientConfig{..} = do
  manager' <- newManager $ bool
    (defaultManagerSettings { managerModifyRequest = fixRequest })
    (tlsManagerSettings { managerModifyRequest = fixRequest })
    ccNodeUseHttps
  let nodeUrl = BaseUrl (bool Http Https ccNodeUseHttps)
                (toString ccNodeAddress) ccNodePort ""
  return $ mkClientEnv manager' nodeUrl

-- | Add header, required by the Tezos RPC interface
fixRequest :: Request -> IO Request
fixRequest req = return $
  req { requestHeaders = ("Content-Type", "application/json") :
        filter (("Content-Type" /=) . fst) (requestHeaders req)
      }

