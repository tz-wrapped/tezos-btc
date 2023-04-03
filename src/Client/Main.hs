{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Client.Main
  ( mainProgram
  , runAppM
  ) where

import Debug qualified (show)

import Fmt (Buildable, pretty)

import Lorentz hiding (address, balance, chainId, cons, map)
import Lorentz.Contracts.Metadata
import Lorentz.Contracts.Multisig
import Morley.Client (awaAddress)
import Morley.Client.Logging (WithClientLog)
import Morley.Client.RPC.Class
import Morley.Client.TezosClient.Class
import Morley.Tezos.Address (Constrained(..))
import Morley.Tezos.Address.Alias (ContractAddressOrAlias)
import Morley.Util.Named
import Morley.Util.TypeLits

import Client.Env
import Client.IO
import Client.Parser
import Client.Types
import Lorentz.Contracts.TZBTC
import Util.AbstractIO
import Util.MultiSig

mainProgram
  :: forall m env.
  ( MonadThrow m
  , HasTezosRpc m
  , HasTezosClient m
  , HasFilesystem m
  , HasCmdLine m
  , WithClientLog env m
  , ExtTezosClient m
  , HasEnv m
  ) => ClientArgsRaw -> m ()
mainProgram cmd = case cmd of
  CmdMint to' value mbMultisig -> do
    to <- addressOrAliasToAddr to'
    runMultisigTzbtcContract mbMultisig $
      fromFlatParameter $ Mint (#to :! toAddress to, #value :! value)
  CmdBurn burnParams mbMultisig ->
    runMultisigTzbtcContract mbMultisig $
      fromFlatParameter $ Burn burnParams
  CmdTransfer from' to' value -> do
    from <- addressOrAliasToAddr from'
    to <- addressOrAliasToAddr to'
    runTzbtcContract $
      fromFlatParameter $ Transfer (#from :! toAddress from, #to :! toAddress to, #value :! value)
  CmdApprove spender' value -> do
    spender <- addressOrAliasToAddr spender'
    runTzbtcContract $
      fromFlatParameter $ Approve (#spender :! toAddress spender, #value :! value)
  CmdGetAllowance (owner', spender') mbCallback' ->
    case mbCallback' of
      Just callback' -> do
        owner <- addressOrAliasToAddr owner'
        spender <- addressOrAliasToAddr spender'
        callback <- addressOrAliasToAddr callback'
        runTzbtcContract $ fromFlatParameter $ GetAllowance $
          mkView_ (#owner :! toAddress owner, #spender :! toAddress spender)
                  (toTAddress callback)
      Nothing -> do
        owner <- addressOrAliasToAddr owner'
        spender <- addressOrAliasToAddr spender'
        allowance <- getAllowance (toAddress owner) (toAddress spender)
        printStringLn $ "Allowance: " <> show allowance
  CmdGetBalance owner' mbCallback' -> do
    case mbCallback' of
      Just callback' -> do
        owner <- addressOrAliasToAddr owner'
        callback <- addressOrAliasToAddr callback'
        runTzbtcContract $
          fromFlatParameter $ GetBalance $
            mkView_ (#owner :! toAddress owner) (toTAddress callback)
      Nothing -> do
        owner <- addressOrAliasToAddr owner'
        balance <- getBalance $ toAddress owner
        printStringLn $ "Balance: " <> show balance
  CmdAddOperator operator' mbMultisig -> do
    operator <- addressOrAliasToAddr operator'
    runMultisigTzbtcContract mbMultisig $
      fromFlatParameter $ AddOperator (#operator :! toAddress operator)
  CmdRemoveOperator operator' mbMultisig -> do
    operator <- addressOrAliasToAddr operator'
    runMultisigTzbtcContract mbMultisig $
      fromFlatParameter $ RemoveOperator (#operator :! toAddress operator)
  CmdPause mbMultisig -> runMultisigTzbtcContract mbMultisig $
    fromFlatParameter $ Pause ()
  CmdUnpause mbMultisig -> runMultisigTzbtcContract mbMultisig $
    fromFlatParameter $ Unpause ()
  CmdSetRedeemAddress redeem' mbMultisig -> do
    redeem <- addressOrAliasToAddr redeem'
    runMultisigTzbtcContract mbMultisig $
      fromFlatParameter $ SetRedeemAddress (#redeem :! toAddress redeem)
  CmdTransferOwnership newOwner' mbMultisig -> do
    newOwner <- addressOrAliasToAddr newOwner'
    runMultisigTzbtcContract mbMultisig $
      fromFlatParameter $ TransferOwnership (#newOwner :! toAddress newOwner)
  CmdAcceptOwnership p mbMultisig -> do
    runMultisigTzbtcContract mbMultisig $
      fromFlatParameter $ AcceptOwnership p
  CmdGetTotalSupply callback -> do
    simpleGetter #totalSupply "Total supply" GetTotalSupply callback
  CmdGetTotalMinted callback -> do
    simpleGetter #totalMinted "Total minted" GetTotalMinted callback
  CmdGetTotalBurned callback -> do
    simpleGetter #totalBurned "Total burned" GetTotalBurned callback
  CmdGetOwner callback ->
    simpleGetter #owner "Owner" GetOwner callback
  CmdGetTokenMetadata callback ->
    case callback of
      Just callback' -> do
        callback'' <- addressOrAliasToAddr callback'
        runTzbtcContract $
          fromFlatParameter $
          GetTokenMetadata $
          View_ [singleTokenTokenId] $
          callingDefAddress $
          toTAddress @[TokenMetadata] callback''
      Nothing -> do
        printFieldFromStorage #tokenMetadata "Token metadata"
  CmdGetRedeemAddress callback ->
    simpleGetter #redeemAddress "Redeem address" GetRedeemAddress callback
  CmdGetOperators ->
    printFieldFromStorage #operators "List of contract operators"
  CmdGetOpDescription packageFilePath -> do
    pkg <- getPackageFromFile packageFilePath
    case pkg of
      Left err -> printTextLn err
      Right package -> printStringLn $ pretty package
  CmdGetBytesToSign packageFilePath -> do
    pkg <- getPackageFromFile packageFilePath
    case pkg of
      Left err -> printTextLn err
      Right package -> printTextLn $ getBytesToSign package
  CmdAddSignature pk sign packageFilePath -> do
    pkg <- getPackageFromFile packageFilePath
    case pkg of
      Left err -> printTextLn err
      Right package -> case addSignature package (pk, TSignature sign) of
        Right signedPackage -> writePackageToFile signedPackage packageFilePath
        Left err -> printStringLn err
  CmdSignPackage packageFilePath -> do
    pkg <- getPackageFromFile packageFilePath
    case pkg of
      Left err -> printTextLn err
      Right package -> do
        signRes <- signPackageForConfiguredUser package
        case signRes of
          Left err -> printStringLn err
          Right signedPackage -> writePackageToFile signedPackage packageFilePath
  CmdCallMultisig packagesFilePaths -> do
    pkgs <- fmap sequence $ mapM getPackageFromFile packagesFilePaths
    case pkgs of
      Left err -> printTextLn err
      Right packages -> runMultisigContract packages
  CmdDeployContract (arg #owner -> mOwner) deployOptions -> do
    owner <- maybe (Constrained . awaAddress <$> getTzbtcUserAddress) addressOrAliasToAddr mOwner
    let toDeployParamsV1 :: DeployContractOptionsV1 -> m V1DeployParameters
        toDeployParamsV1 DeployContractOptionsV1{..} = do
          redeem <- addressOrAliasToAddr dcoRedeem
          return V1DeployParameters
              { v1Owner = owner
              , v1MigrationParams = V1Parameters
                { v1RedeemAddress = toAddress redeem
                , v1TokenMetadata = dcoTokenMetadata
                , v1Balances = mempty
                }
              }
    let toDeployParamsV2 :: DeployContractOptionsV2 -> m V2DeployParameters
        toDeployParamsV2 (DeployContractOptionsV2 optsV1) = do
          V1DeployParameters{..} <- toDeployParamsV1 optsV1
          return V2DeployParameters
            { v2Owner = v1Owner
            , v2MigrationParams = v1MigrationParams
            }
    case deployOptions of
      DeployContractV1 opts ->
        deployTzbtcContractV1 =<< toDeployParamsV1 opts
      DeployContractV2 opts ->
        deployTzbtcContractV2 =<< toDeployParamsV2 opts
  CmdDeployMultisigContract threshold keys' useCustomErrors -> do
    deployMultisigContract ((Counter 0), (threshold, keys')) useCustomErrors
  CmdShowConfig -> do
    config <- getTezosClientConfig
    printStringLn $ Debug.show config
  where
    runMultisigTzbtcContract :: Maybe FilePath -> Parameter SomeTZBTCVersion -> m ()
    runMultisigTzbtcContract mbMultisig param =
      case mbMultisig of
        Just fp -> case toSafeParam param of
          Just subParam -> createMultisigPackage fp subParam
          _ -> printStringLn "Unable to call multisig for View_ entrypoints"
        Nothing -> runTzbtcContract param
    printFieldFromStorage
      :: forall t name. (HasStoreTemplateField t name, Buildable t)
      => Label name -> Text -> m ()
    printFieldFromStorage _ descr = do
      mbField <- getFieldFromTzbtcUStore @name @t
      case mbField of
        Just field' -> printTextLn $ descr <> ": " <> pretty field'
        Nothing -> printTextLn $ "Field " <>
          symbolValT' @name <> " not found in the contract storage"
    simpleGetter ::
      forall a name.
      ( HasStoreTemplateField a name, Buildable a
      , NiceParameterFull a, NoExplicitDefaultEntrypoint a
      ) =>
      Label name -> Text -> (View_ () a -> FlatParameter SomeTZBTCVersion) ->
      Maybe ContractAddressOrAlias -> m ()
    simpleGetter label descr mkFlatParam = \case
      Just callback' -> do
        callback <- addressOrAliasToAddr callback'
        runTzbtcContract $
          fromFlatParameter $ mkFlatParam $ View_ () (callingDefAddress $ toTAddress @a callback)
      Nothing -> do
        printFieldFromStorage @a label descr
