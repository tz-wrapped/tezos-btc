{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Types
  ( AddrOrAlias
  , AlmostStorage (..)
  , AppliedResult (..)
  , BlockConstants (..)
  , ClientArgs (..)
  , ClientArgsRaw (..)
  , ClientConfig (..)
  , ConfirmationResult (..)
  , ConfigOverride (..)
  , DeployContractOptions (..)
  , EntrypointParam (..)
  , ForgeOperation (..)
  , InternalOperation (..)
  , MichelsonExpression (..)
  , OperationContent (..)
  , OriginationOperation (..)
  , OriginationScript (..)
  , ParametersInternal (..)
  , PreApplyOperation (..)
  , RunError (..)
  , RunMetadata (..)
  , RunOperation (..)
  , RunOperationInternal (..)
  , RunOperationResult (..)
  , RunRes (..)
  , TransactionOperation (..)
  , TezosClientConfig (..)
  , combineResults
  ) where

import Data.Aeson
  (FromJSON(..), ToJSON(..), Value(..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Data.List (isSuffixOf)
import Data.Vector (fromList)
import Fmt (Buildable(..), (+|), (|+))
import Tezos.Common.Base16ByteString (Base16ByteString(..))
import Tezos.Common.Json (StringEncode(..), TezosInt64)
import Tezos.V005.Micheline
  (Expression(..), MichelinePrimAp(..), MichelinePrimitive(..), annotToText)

import Michelson.Text (MText)
import Michelson.Typed (IsoValue)
import Tezos.Address (Address)
import Tezos.Crypto (PublicKey, Signature, encodeBase58Check, formatSignature)
import Util.Named

import Lorentz.Contracts.Multisig
import Lorentz.Contracts.TZBTC.Types

-- | Client argument with optional dry-run flag
data ClientArgs =
  ClientArgs
    ClientArgsRaw
      ("userOverride" :! Maybe AddrOrAlias)
      ("multisigOverride" :! Maybe AddrOrAlias) Bool

type AddrOrAlias = Text

data ClientArgsRaw
  = CmdMint AddrOrAlias Natural (Maybe FilePath)
  | CmdBurn BurnParams (Maybe FilePath)
  | CmdTransfer AddrOrAlias AddrOrAlias Natural
  | CmdApprove AddrOrAlias Natural
  | CmdGetAllowance (AddrOrAlias, AddrOrAlias) (Maybe AddrOrAlias)
  | CmdGetBalance AddrOrAlias (Maybe AddrOrAlias)
  | CmdAddOperator AddrOrAlias (Maybe FilePath)
  | CmdRemoveOperator AddrOrAlias (Maybe FilePath)
  | CmdPause (Maybe FilePath)
  | CmdUnpause (Maybe FilePath)
  | CmdSetRedeemAddress AddrOrAlias (Maybe FilePath)
  | CmdTransferOwnership AddrOrAlias (Maybe FilePath)
  | CmdAcceptOwnership AcceptOwnershipParams (Maybe FilePath)
  | CmdGetTotalSupply (Maybe AddrOrAlias)
  | CmdGetTotalMinted (Maybe AddrOrAlias)
  | CmdGetTotalBurned (Maybe AddrOrAlias)
  | CmdGetOwner (Maybe AddrOrAlias)
  | CmdGetTokenName (Maybe AddrOrAlias)
  | CmdGetTokenCode (Maybe AddrOrAlias)
  | CmdGetRedeemAddress (Maybe AddrOrAlias)
  | CmdGetOperators
  | CmdGetOpDescription FilePath
  | CmdGetBytesToSign FilePath
  | CmdAddSignature PublicKey Signature FilePath
  | CmdSignPackage FilePath
  | CmdCallMultisig (NonEmpty FilePath)
  | CmdDeployContract !DeployContractOptions
  | CmdDeployMultisigContract Threshold Keys Bool
  | CmdShowConfig

data DeployContractOptions = DeployContractOptions
  { dcoOwner :: !(Maybe AddrOrAlias)
  , dcoRedeem :: !AddrOrAlias
  , dcoTokenName :: !MText
  , dcoTokenCode :: !MText
  }

data ConfigOverride = ConfigOverride
  { coTzbtcUser :: Maybe AddrOrAlias
  , coTzbtcMultisig :: Maybe AddrOrAlias
  } deriving stock Show

newtype MichelsonExpression = MichelsonExpression Expression
  deriving newtype FromJSON

instance Buildable MichelsonExpression where
  build (MichelsonExpression expr) = case expr of
    Expression_Int (StringEncode i) -> build $ i
    Expression_String s -> build s
    Expression_Bytes b ->
      build $ encodeBase58Check $ unbase16ByteString b
    Expression_Seq s -> "(" +| buildSeq (build . MichelsonExpression) s |+ ")"
    Expression_Prim (MichelinePrimAp (MichelinePrimitive text) s annots) ->
      text <> " " |+ "(" +|
      buildSeq (build . MichelsonExpression) s +| ") " +|
      buildSeq (build . annotToText) annots
    where
      buildSeq buildElem =
        mconcat . intersperse ", " . map
        buildElem . toList

data AlmostStorage sign = AlmostStorage
  { asBigMapId :: Natural
  , asFields :: StorageFields sign
  } deriving stock (Show, Generic)
    deriving anyclass IsoValue

data ForgeOperation = ForgeOperation
  { foBranch :: Text
  , foContents :: [Either TransactionOperation OriginationOperation]
  }


contentsToJSON :: [Either TransactionOperation OriginationOperation] -> Value
contentsToJSON = Array . fromList .
  map (\case
          Right transOp -> toJSON transOp
          Left origOp -> toJSON origOp
      )

instance ToJSON ForgeOperation where
  toJSON ForgeOperation{..} = object
    [ "branch" .= toString foBranch
    , ("contents", contentsToJSON foContents)
    ]

data RunOperationInternal = RunOperationInternal
  { roiBranch :: Text
  , roiContents :: [Either TransactionOperation OriginationOperation]
  , roiSignature :: Signature
  }

instance ToJSON RunOperationInternal where
  toJSON RunOperationInternal{..} = object
    [ "branch" .= toString roiBranch
    , ("contents", contentsToJSON roiContents)
    , "signature" .= toJSON roiSignature
    ]

data RunOperation = RunOperation
  { roOperation :: RunOperationInternal
  , roChainId :: Text
  }

data PreApplyOperation = PreApplyOperation
  { paoProtocol :: Text
  , paoBranch :: Text
  , paoContents :: [Either TransactionOperation OriginationOperation]
  , paoSignature :: Signature
  }

instance ToJSON PreApplyOperation where
  toJSON PreApplyOperation{..} = object
    [ "branch" .= toString paoBranch
    , ("contents", contentsToJSON paoContents)
    , "protocol" .= toString paoProtocol
    , "signature" .= formatSignature paoSignature
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

data BlockConstants = BlockConstants
  { bcProtocol :: Text
  , bcChainId :: Text
  }

data EntrypointParam param
  = DefaultEntrypoint param
  | Entrypoint Text param

data RunError
  = RuntimeError Address
  | ScriptRejected MichelsonExpression
  | BadContractParameter Address
  | InvalidConstant MichelsonExpression MichelsonExpression
  | InconsistentTypes MichelsonExpression MichelsonExpression
  | InvalidPrimitive [Text] Text
  | InvalidSyntacticConstantError MichelsonExpression MichelsonExpression
  | UnexpectedContract
  | IllFormedType MichelsonExpression
  | UnexpectedOperation

instance FromJSON RunError where
  parseJSON = withObject "preapply error" $ \o -> do
    id' <- o .: "id"
    case id' of
      x | "runtime_error" `isSuffixOf` x ->
          RuntimeError <$> o .: "contract_handle"
      x | "script_rejected" `isSuffixOf` x ->
          ScriptRejected <$> o .: "with"
      x | "bad_contract_parameter" `isSuffixOf` x ->
          BadContractParameter <$> o .: "contract"
      x | "invalid_constant" `isSuffixOf` x ->
          InvalidConstant <$> o .: "expected_type" <*> o .: "wrong_expression"
      x | "inconsistent_types" `isSuffixOf` x ->
          InconsistentTypes <$> o .: "first_type" <*> o .: "other_type"
      x | "invalid_primitive" `isSuffixOf` x ->
          InvalidPrimitive <$> o .: "expected_primitive_names" <*> o .: "wrong_primitive_name"
      x | "invalidSyntacticConstantError" `isSuffixOf` x ->
          InvalidSyntacticConstantError <$> o .: "expectedForm" <*> o .: "wrongExpression"
      x | "unexpected_contract" `isSuffixOf` x ->
          pure UnexpectedContract
      x | "ill_formed_type" `isSuffixOf` x ->
          IllFormedType <$> o .: "ill_formed_expression"
      x | "unexpected_operation" `isSuffixOf` x ->
          pure UnexpectedOperation
      _ -> fail ("unknown id: " <> id')

instance Buildable RunError where
  build = \case
    RuntimeError addr -> "Runtime error for contract: " +| addr |+ ""
    ScriptRejected expr -> "Script rejected with: " +| expr |+ ""
    BadContractParameter addr -> "Bad contract parameter for: " +| addr |+ ""
    InvalidConstant expectedType expr ->
      "Invalid type: " +| expectedType |+ "\n" +|
      "For: " +| expr |+ ""
    InconsistentTypes type1 type2 ->
      "Inconsistent types: " +| type1 |+ " and " +| type2 |+ ""
    InvalidPrimitive expectedPrimitives wrongPrimitive ->
      "Invalid primitive: " +| wrongPrimitive |+ "\n" +|
      "Expecting on of: " +|
      mconcat (map ((<> " ") . build) expectedPrimitives) |+ ""
    InvalidSyntacticConstantError expectedForm wrongExpression ->
      "Invalid syntatic constant error, expecting: " +| expectedForm |+ "\n" +|
      "But got: " +| wrongExpression |+ ""
    UnexpectedContract ->
      "When parsing script, a contract type was found in \
      \the storage or parameter field."
    IllFormedType expr ->
      "Ill formed type: " +| expr |+ ""
    UnexpectedOperation ->
      "When parsing script, an operation type was found in \
      \the storage or parameter field"

data RunOperationResult
  = RunOperationApplied AppliedResult
  | RunOperationFailed [RunError]

data AppliedResult = AppliedResult
  { arConsumedGas :: TezosInt64
  , arStorageSize :: TezosInt64
  , arPaidStorageDiff :: TezosInt64
  , arOriginatedContracts :: [Address]
  } deriving Show

instance Semigroup AppliedResult where
  (<>) ar1 ar2 = AppliedResult
    { arConsumedGas = arConsumedGas ar1 + arConsumedGas ar2
    , arStorageSize = arStorageSize ar1 + arStorageSize ar2
    , arPaidStorageDiff = arPaidStorageDiff ar1 + arPaidStorageDiff ar2
    , arOriginatedContracts = arOriginatedContracts ar1 <> arOriginatedContracts ar2
    }

instance Monoid AppliedResult where
  mempty = AppliedResult 0 0 0 []

combineResults :: RunOperationResult -> RunOperationResult -> RunOperationResult
combineResults
  (RunOperationApplied res1) (RunOperationApplied res2) =
  RunOperationApplied $ res1 <> res2
combineResults (RunOperationApplied _) (RunOperationFailed e) =
  RunOperationFailed e
combineResults (RunOperationFailed e) (RunOperationApplied _) =
  RunOperationFailed e
combineResults (RunOperationFailed e1) (RunOperationFailed e2) =
  RunOperationFailed $ e1 <> e2

instance FromJSON RunOperationResult where
  parseJSON = withObject "operation_costs" $ \o -> do
    status <- o .: "status"
    case status of
      "applied" -> RunOperationApplied <$>
        (AppliedResult <$>
          o .: "consumed_gas" <*> o .: "storage_size" <*>
          o .:? "paid_storage_size_diff" .!= 0 <*>
          o .:? "originated_contracts" .!= []
        )
      "failed" -> RunOperationFailed <$> o .: "errors"
      "backtracked" ->
        RunOperationFailed <$> o .:? "errors" .!= []
      "skipped" ->
        RunOperationFailed <$> o .:? "errors" .!= []
      _ -> fail ("unexpected status " ++ status)

data ParametersInternal = ParametersInternal
  { piEntrypoint :: Text
  , piValue :: Expression
  }

data TransactionOperation = TransactionOperation
  { toKind :: Text
  , toSource :: Address
  , toFee :: TezosInt64
  , toCounter :: TezosInt64
  , toGasLimit :: TezosInt64
  , toStorageLimit :: TezosInt64
  , toAmount :: TezosInt64
  , toDestination :: Address
  , toParameters :: ParametersInternal
  }

data OriginationScript = OriginationScript
  { osCode :: Expression
  , osStorage :: Expression
  }

data OriginationOperation = OriginationOperation
  { ooKind :: Text
  , ooSource :: Address
  , ooFee :: TezosInt64
  , ooCounter :: TezosInt64
  , ooGasLimit :: TezosInt64
  , ooStorageLimit :: TezosInt64
  , ooBalance :: TezosInt64
  , ooScript :: OriginationScript
  }

data ClientConfig = ClientConfig
  { ccNodeAddress :: Text
  , ccNodePort :: Int
  , ccNodeUseHttps :: Bool
  , ccContractAddress :: Maybe Address
  , ccMultisigAddress :: Maybe Address
  , ccUserAlias :: Text
  , ccTezosClientExecutable :: FilePath
  } deriving (Generic, Show, Eq)

instance Buildable ClientConfig where
  build ClientConfig {..} =
    "Node address: " +| ccNodeAddress |+ "\n" +|
    "Node port: " +| ccNodePort |+ "\n" +|
    "Use HTTPS: " +| ccNodeUseHttps |+ "\n" +|
    "Contract address: " +| ccContractAddress |+ "\n" +|
    "Multisig contract address: " +| ccMultisigAddress |+ "\n" +|
    "User alias: " +| ccUserAlias |+ "\n" +|
    "tezos-client path: " +| ccTezosClientExecutable |+ "\n"

data ConfirmationResult = Confirmed | Canceled

data TezosClientConfig = TezosClientConfig
  { tcNodeAddr :: Text
  , tcNodePort :: Int
  , tcTls :: Bool
  } deriving (Show)

deriveToJSON (aesonPrefix snakeCase) ''ParametersInternal
deriveToJSON (aesonPrefix snakeCase) ''TransactionOperation
deriveToJSON (aesonPrefix snakeCase) ''OriginationScript
deriveToJSON (aesonPrefix snakeCase) ''OriginationOperation
deriveToJSON (aesonPrefix snakeCase) ''RunOperation
deriveFromJSON (aesonPrefix snakeCase) ''BlockConstants
deriveFromJSON (aesonPrefix snakeCase) ''TezosClientConfig
