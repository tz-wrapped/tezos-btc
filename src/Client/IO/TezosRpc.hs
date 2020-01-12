{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}

module Client.IO.TezosRpc
  ( deployTzbtcContract
  , getStorage
  , runTransactions
  ) where

import Data.ByteString (cons)
import Servant.Client (ClientEnv, runClientM)
import Servant.Client.Core as Servant (ClientError(..))
import Tezos.Common.Json (TezosInt64)
import Tezos.V005.Micheline (Expression)
import Time (Second, Time(..), threadDelay)

import Lorentz hiding (address, balance, chainId, cons, map)
import Lorentz.UStore.Migration (manualConcatMigrationScripts)
import Michelson.Runtime.GState (genesisAddress1, genesisAddress2)
import Michelson.Untyped (InternalByteString(..))
import Tezos.Address
import Util.Named ((.!))

import qualified Client.API as API
import Client.Crypto
import Client.Error
import Client.IO.HttpClient as IO
import Client.IO.TezosClient as IO
import Client.Types
import Client.Util
import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.Preprocess (migrationScripts, tzbtcContract, tzbtcContractRouter)
import Lorentz.Contracts.TZBTC.V0 (mkEmptyStorageV0)

-- | Datatype that contains various values required for
-- chain operations.
data OperationConstants = OperationConstants
  { ocClientEnv :: ClientEnv
  -- ^ Environment required for network operations
  , ocLastBlockHash :: Text
  -- ^ Block in which operations is going to be injected
  , ocSourceAddr :: Address
  -- ^ The address of the operations sender
  , ocBlockConstants :: BlockConstants
  -- ^ Information about block: chain_id and protocol
  , ocCounter :: TezosInt64
  -- ^ Sender counter
  }

stubSignature :: Signature
stubSignature = unsafeParseSignature
  "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"

injectOp :: Text -> Signature -> ClientConfig -> IO Text
injectOp opToInject sign config = do
  clientEnv <- IO.getClientEnv config
  throwClientError $ runClientM
    (API.injectOperation (Just "main") $ prepareForInjection opToInject sign) clientEnv

getLastBlockHash :: ClientEnv -> IO (Either ClientError Text)
getLastBlockHash env = runClientM API.getLastBlock env

getCurrentBlockConstants :: ClientEnv -> Text -> IO (Either ClientError BlockConstants)
getCurrentBlockConstants env block = runClientM (API.getBlockConstants block) env

getAddressCounter :: ClientEnv -> Address -> IO (Either ClientError TezosInt64)
getAddressCounter env address = runClientM (API.getCounter $ formatAddress address) env

dumbOp :: TransactionOperation
dumbOp = TransactionOperation
  { toKind = "transaction"
  , toSource = genesisAddress1
  , toFee = 50000
  , toCounter = 0
  , toGasLimit = 800000
  , toStorageLimit = 60000
  , toAmount = 1
  , toDestination = genesisAddress2
  , toParameters = ParametersInternal
    { piEntrypoint = "default"
    , piValue = nicePackedValueToExpression $ fromFlatParameter $ Pause ()
    }
  -- Forge operation with given limits and get its hexadecimal representation
  }

-- | Function which obtains `OperationConstant` by given
-- `ClientConfig`.
preProcessOperation :: ClientConfig -> IO OperationConstants
preProcessOperation config@ClientConfig{..} = do
  ocClientEnv <- IO.getClientEnv config
  ocLastBlockHash <- throwClientError $ getLastBlockHash ocClientEnv
  (ocSourceAddr, _) <- throwLeft $ getAddressAndPKForAlias ccTezosClientExecutable ccUserAlias
  ocBlockConstants <-
    throwClientError $ getCurrentBlockConstants ocClientEnv ocLastBlockHash
  ocCounter <- throwClientError $ getAddressCounter ocClientEnv ocSourceAddr
  pure OperationConstants{..}

-- | Function which waits for operation to be included into
-- the chain.
postProcessOperation :: ClientConfig -> Text -> IO ()
postProcessOperation config opHash = do
  putStrLn $ "Operation hash: " <> opHash
  waitForOperationInclusion (ccTezosClientExecutable config) opHash

toParametersInternals :: (NicePackedValue param) => EntrypointParam param -> ParametersInternal
toParametersInternals = \case
  DefaultEntrypoint param -> ParametersInternal
    { piEntrypoint = "default"
    , piValue = nicePackedValueToExpression param
    }
  Entrypoint ep param -> ParametersInternal
    { piEntrypoint = ep
    , piValue = nicePackedValueToExpression param
    }

addOperationPrefix :: InternalByteString -> InternalByteString
addOperationPrefix (InternalByteString bs) =
  InternalByteString $ cons 0x03 bs

-- It is possible to include multiple transaction within single operations batch
-- so we are accepting list of param's here.
runTransactions
  :: (NicePackedValue param)
  => Address -> [EntrypointParam param] -> ClientConfig -> IO ()
runTransactions to params config@ClientConfig{..} = do
  OperationConstants{..} <- preProcessOperation config
  let opsToRun = map (\(param, i) -> dumbOp
        { toDestination = to
        , toSource = ocSourceAddr
        , toCounter = ocCounter + (fromInteger i)
        , toParameters = toParametersInternals param
        }) $ zip params [1..]
  let runOp = RunOperation
        { roOperation =
          RunOperationInternal
          { roiBranch = ocLastBlockHash
          , roiContents = map (\opToRun -> Left opToRun) opsToRun
          , roiSignature = stubSignature
          }
        , roChainId = bcChainId ocBlockConstants
        }
  -- Perform run_operation with dumb signature in order
  -- to estimate gas cost, storage size and paid storage diff
  results <- getAppliedResults ocClientEnv (Left runOp)
  -- Forge operation with given limits and get its hexadecimal representation
  hex <- throwClientError $ getOperationHex ocClientEnv ForgeOperation
    { foBranch = ocLastBlockHash
    , foContents = map (\(opToRun, AppliedResult{..}) ->
                          Left $ opToRun
                          { toGasLimit = arConsumedGas + 200
                          , toStorageLimit = arStorageSize + 20
                          , toFee = calcFees arConsumedGas arPaidStorageDiff
                          }
                       ) $ zip opsToRun results
    }
  -- Sign operation with sender secret key
  signRes <- IO.signWithTezosClient ccTezosClientExecutable (Left $ addOperationPrefix hex) ccUserAlias
  case signRes of
    Left err -> putStrLn err
    Right signature' -> do
      -- Sign and inject the operation
      injectOp (formatByteString hex) signature' config >>=
        (postProcessOperation config)

getAppliedResults
  :: ClientEnv -> Either RunOperation PreApplyOperation
  -> IO [AppliedResult]
getAppliedResults env op = do
  results <- case op of
    Left runOp ->
      (sequence . (: [])) <$> throwLeft $ runClientM (API.runOperation runOp) env
    Right preApplyOp ->
      throwLeft $ runClientM (API.preApplyOperations [preApplyOp]) env
  concatMapM handleRunRes results
  where
    handleRunRes :: MonadThrow m => RunRes -> m [AppliedResult]
    handleRunRes RunRes{..} =
      case rrOperationContents of
        [] -> throwM $ TzbtcUnexpectedRunResult "empty result"
        opContents ->
          mapM (\(OperationContent (RunMetadata res internalOps)) ->
                  let internalResults = map unInternalOperation internalOps in
                    case foldr combineResults res internalResults of
                      RunOperationApplied appliedRes -> pure appliedRes
                      RunOperationFailed errors -> throwM (TzbtcRunFailed errors)
               ) opContents

originateTzbtcContract
  :: Address -> ClientConfig -> IO (Either TzbtcClientError Address)
originateTzbtcContract owner config@ClientConfig{..} = do
  OperationConstants{..} <- preProcessOperation config
  let origOp = OriginationOperation
        { ooKind = "origination"
        , ooSource = ocSourceAddr
        , ooFee = 50000
        , ooCounter = ocCounter + 1
        , ooGasLimit = 800000
        , ooStorageLimit = 60000
        , ooBalance = 0
        , ooScript =
          mkOriginationScript tzbtcContract (mkEmptyStorageV0 owner)
        }
  let runOp = RunOperation
        { roOperation =
          RunOperationInternal
          { roiBranch = ocLastBlockHash
          , roiContents = [Right origOp]
          , roiSignature = stubSignature
          }
        , roChainId = bcChainId ocBlockConstants
        }
  -- Estimate limits and paid storage diff
  -- We run only one op and expect to have only one result
  [ar1] <- getAppliedResults ocClientEnv (Left runOp)
  -- Update limits and fee
  let updOrigOp = origOp { ooGasLimit = arConsumedGas ar1 + 200
                         , ooStorageLimit = arStorageSize ar1 + 1000
                         , ooFee = calcFees (arConsumedGas ar1) (arPaidStorageDiff ar1)
                         }
  hex <- throwClientError $ getOperationHex ocClientEnv ForgeOperation
    { foBranch = ocLastBlockHash
    , foContents = [Right updOrigOp]
    }
  signRes <- IO.signWithTezosClient ccTezosClientExecutable (Left $ addOperationPrefix hex) ccUserAlias
  case signRes of
    Left err -> pure $ Left $ TzbtcOriginationError err
    Right signature' -> do
      let preApplyOp = PreApplyOperation
            { paoProtocol = bcProtocol ocBlockConstants
            , paoBranch = ocLastBlockHash
            , paoContents = [Right updOrigOp]
            , paoSignature = signature'
            }
      -- Perform operations preapplying in order to obtain contract address
      -- We preapply only one op and expect to have only one result
      [ar2] <- getAppliedResults ocClientEnv (Right preApplyOp)
      injectOp (formatByteString hex) signature' config >>=
        (postProcessOperation config)
      case arOriginatedContracts ar2 of
        [contractAddr] -> pure $ Right contractAddr
        _ -> pure $ Left $ TzbtcOriginationError
          "Error during contract origination, expecting to \
          \originate exactly one contract."

deployTzbtcContract :: ClientConfig -> OriginationParameters -> IO Address
deployTzbtcContract config@ClientConfig{..} op = do
  putTextLn "Originate contract"
  contractAddr <- throwLeft $ originateTzbtcContract (opOwner op) config
  let migration = migrationScripts op
      transactionsToTzbtc params = runTransactions contractAddr params config
  putTextLn "Upgrade contract to V1"
  throwClientErrorAfterRetry (10, delayFn) $ try $
    -- We retry here because it was found that in some cases, the storage
    -- is unavailable for a short time since contract origination.
    transactionsToTzbtc $ (DefaultEntrypoint . fromFlatParameter) <$>
      [ Upgrade ( #newVersion .! 1
                , #migrationScript .! manualConcatMigrationScripts migration
                , #newCode .! tzbtcContractRouter
                )
      ]
  pure contractAddr
  where
    delayFn = do
      hPutStrLn stderr ("Upgrading failed, retrying after 10 seconds..." :: String)
      threadDelay (Time @Second 10)

getStorage :: ClientConfig -> Text -> IO Expression
getStorage config addr = do
  clientEnv <- IO.getClientEnv config
  throwClientError $ runClientM (API.getStorage addr) clientEnv
