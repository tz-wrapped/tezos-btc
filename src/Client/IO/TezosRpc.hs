{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

module Client.IO.TezosRpc
  ( deployTzbtcContract
  , getStorage
  , originateContract
  , runTransactions
  ) where

import Data.ByteString (cons)
import Servant.Client (ClientEnv, runClientM)
import Servant.Client.Core as Servant (ClientError(..))
import Time (Second, Time(..), threadDelay)

import Lorentz hiding (address, balance, chainId, cons, contract, map)
import Michelson.Runtime.GState (genesisAddress1, genesisAddress2)
import Michelson.Untyped (InternalByteString(..))
import Morley.Micheline (Expression, TezosInt64)
import Tezos.Address

import qualified Client.API as API
import Client.Crypto
import Client.Error
import Client.IO.HttpClient as IO
import Client.IO.TezosClient as IO
import Client.Types
import Client.Util
import Lorentz.Contracts.TZBTC
import Lorentz.Contracts.TZBTC.Preprocess (upgradeParameters)

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
  , toAmount = 0
  , toDestination = genesisAddress2
  , toParameters = ParametersInternal
    { piEntrypoint = "default"
    , piValue = nicePackedValueToExpression $ fromFlatParameter $ Pause @SomeTZBTCVersion ()
    }
  -- Forge operation with given limits and get its hexadecimal representation
  }

-- | Function which obtains `OperationConstant` by given
-- `ClientConfig`.
preProcessOperation :: Bool -> ClientConfig -> IO OperationConstants
preProcessOperation v config@ClientConfig{..} = do
  ocClientEnv <- IO.getClientEnv config
  ocLastBlockHash <- throwClientError $ getLastBlockHash ocClientEnv
  revealKeyForAlias v ccTezosClientExecutable ccUserAlias
  (ocSourceAddr, _) <- throwLeft $ getAddressAndPKForAlias v ccTezosClientExecutable ccUserAlias
  ocBlockConstants <-
    throwClientError $ getCurrentBlockConstants ocClientEnv ocLastBlockHash
  ocCounter <- throwClientError $ getAddressCounter ocClientEnv ocSourceAddr
  pure OperationConstants{..}

-- | Function which waits for operation to be included into
-- the chain.
postProcessOperation :: Bool -> ClientConfig -> TezosInt64 -> Text -> IO ()
postProcessOperation v config fee opHash = do
  putStrLn $ "Fee: " <> toTezString fee <> " Tez"
  putStrLn $ "Operation hash: " <> opHash
  waitForOperationInclusion v (ccTezosClientExecutable config) opHash

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
  => Bool -> Address -> EntrypointParam param -> TezosInt64 -> ClientConfig -> Maybe TezosInt64 -> IO ()
runTransactions v to param transferAmount config@ClientConfig{..} mbFees = do
  OperationConstants{..} <- preProcessOperation v config
  let opsToRun = dumbOp
        { toDestination = to
        , toSource = ocSourceAddr
        , toCounter = ocCounter + 1
        , toAmount = transferAmount
        , toParameters = toParametersInternals param
        }
  let runOp = RunOperation
        { roOperation =
          RunOperationInternal
          { roiBranch = ocLastBlockHash
          , roiContents = [Left opsToRun]
          , roiSignature = stubSignature
          }
        , roChainId = bcChainId ocBlockConstants
        }
  -- Perform run_operation with dumb signature in order
  -- to estimate gas cost, storage size and paid storage diff
  [results] <- getAppliedResults ocClientEnv (Left runOp)

  bakerFee <- case mbFees of
    Just f -> pure f
    Nothing -> srComputedFees <$> IO.simulateTransaction v config Nothing to param

  -- Forge operation with given limits and get its hexadecimal representation
  hex <- throwClientError $ getOperationHex ocClientEnv ForgeOperation
    { foBranch = ocLastBlockHash
    , foContents = [Left $ opsToRun
                          { toGasLimit = (arConsumedGas results) + 200
                          , toStorageLimit = (arStorageSize results) + 20
                          , toFee = bakerFee
                          }]
    }
  -- Sign operation with sender secret key
  signRes <- IO.signWithTezosClient v ccTezosClientExecutable (Left $ addOperationPrefix hex) ccUserAlias
  case signRes of
    Left err -> putStrLn err
    Right signature' -> do
      -- Sign and inject the operation
      injectOp (formatByteString hex) signature' config >>=
        (postProcessOperation v config bakerFee)

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

originateContract
  :: (NiceParameterFull cp, NiceStorage st)
  => Bool -> Contract cp st -> st -> ClientConfig -> Maybe TezosInt64 -> IO (Either TzbtcClientError Address)
originateContract v contract initialStorage config@ClientConfig{..} mbFees = do
  OperationConstants{..} <- preProcessOperation v config
  let origOp = OriginationOperation
        { ooKind = "origination"
        , ooSource = ocSourceAddr
        , ooFee = 50000
        , ooCounter = ocCounter + 1
        , ooGasLimit = 800000
        , ooStorageLimit = 60000
        , ooBalance = 0
        , ooScript =
          mkOriginationScript contract initialStorage
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


  bakerFee <- case mbFees of
    Just f -> pure $ f
    Nothing -> srComputedFees <$> IO.simulateOrigination v config Nothing contract initialStorage

  -- Update limits and fee
  let updOrigOp = origOp { ooGasLimit = arConsumedGas ar1 + 200
                         , ooStorageLimit = arStorageSize ar1 + 1000
                         , ooFee = bakerFee
                         }
  hex <- throwClientError $ getOperationHex ocClientEnv ForgeOperation
    { foBranch = ocLastBlockHash
    , foContents = [Right updOrigOp]
    }
  signRes <- IO.signWithTezosClient v ccTezosClientExecutable (Left $ addOperationPrefix hex) ccUserAlias
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
        (postProcessOperation v config bakerFee)
      case arOriginatedContracts ar2 of
        [contractAddr] -> pure $ Right contractAddr
        _ -> pure $ Left $ TzbtcOriginationError
          "Error during contract origination, expecting to \
          \originate exactly one contract."

deployTzbtcContract :: Bool -> ClientConfig -> Maybe TezosInt64 -> V1DeployParameters -> IO Address
deployTzbtcContract v config@ClientConfig{..} mbFees V1DeployParameters{..} = do
  putTextLn "Originate contract"
  contractAddr <- throwLeft $
    originateContract v tzbtcContract (mkEmptyStorageV0 v1Owner) config mbFees
  let transactionsToTzbtc params amt = runTransactions v contractAddr params amt config mbFees
  putTextLn "Upgrade contract to V1"
  throwClientErrorAfterRetry (10, delayFn) $ try $
    -- We retry here because it was found that in some cases, the storage
    -- is unavailable for a short time since contract origination.
    let param = Upgrade @TZBTCv0 $ upgradeParameters v1MigrationParams
    in transactionsToTzbtc (DefaultEntrypoint . fromFlatParameter $ param) 0
  pure contractAddr
  where
    delayFn = do
      hPutStrLn stderr ("Upgrading failed, retrying after 10 seconds..." :: String)
      threadDelay (Time @Second 10)

getStorage :: ClientConfig -> Text -> IO Expression
getStorage config addr = do
  clientEnv <- IO.getClientEnv config
  throwClientError $ runClientM (API.getStorage addr) clientEnv
