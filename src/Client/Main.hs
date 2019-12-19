{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Main
  ( mainProgram
  , mkInitEnv
  , runAppM
  ) where

import Data.Version (showVersion)
import Data.Vinyl
import Fmt (Buildable, pretty)
import Options.Applicative
  (footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import Options.Applicative.Help.Pretty (Doc, linebreak)

import Lorentz hiding (address, balance, chainId, cons, map)
import Lorentz.Macro (View(..))
import Paths_tzbtc (version)
import Util.Named ((.!))
import Util.TypeLits

import Client.Env
import Client.IO
import Client.Parser
import Client.Types
import Client.Util
import Lorentz.Contracts.TZBTC
import Util.AbstractIO
import Util.MultiSig

mainProgram
  :: forall m.
  ( MonadThrow m
  , MonadFail m
  , HasTezosRpc m
  , HasFilesystem m
  , HasCmdLine m
  ) => m ()
mainProgram = do
  ClientArgs cmd maybeuser dryRunFlag <- parseCmdLine programInfo
  -- Change the reader environment to include the user alias
  -- override.
  withLocal (\e -> case maybeuser of
      Just u -> e { aeConfigOverride = (aeConfigOverride e) { coTzbtcUser = Just u } }
      Nothing -> e) $ do
    case dryRunFlag of
      True -> pass
      False -> case cmd of
        CmdMint to' value mbMultisig -> do
          to <- addrOrAliasToAddr to'
          runMultisigTzbtcContract mbMultisig $
            fromFlatParameter $ Mint (#to .! to, #value .! value)
        CmdBurn burnParams mbMultisig ->
          runMultisigTzbtcContract mbMultisig $
            fromFlatParameter $ Burn burnParams
        CmdTransfer from' to' value -> do
          [from, to] <- mapM addrOrAliasToAddr [from', to']
          runTzbtcContract $
            fromFlatParameter $ Transfer (#from .! from, #to .! to, #value .! value)
        CmdApprove spender' value -> do
          spender <- addrOrAliasToAddr spender'
          runTzbtcContract $
            fromFlatParameter $ Approve (#spender .! spender, #value .! value)
        CmdGetAllowance (owner', spender') mbCallback' ->
          case mbCallback' of
            Just callback' -> do
              [owner, spender, callback] <- mapM addrOrAliasToAddr [owner', spender', callback']
              runTzbtcContract $ fromFlatParameter $ GetAllowance $
                mkView (#owner .! owner, #spender .! spender)
                       (toTAddress callback)
            Nothing -> do
              [owner, spender] <- mapM addrOrAliasToAddr [owner', spender']
              allowance <- getAllowance owner spender
              printStringLn $ "Allowance: " <> show allowance
        CmdGetBalance owner' mbCallback' -> do
          case mbCallback' of
            Just callback' -> do
              [owner, callback] <- mapM addrOrAliasToAddr [owner', callback']
              runTzbtcContract $
                fromFlatParameter $ GetBalance $
                  mkView (#owner .! owner) (toTAddress callback)
            Nothing -> do
              owner <- addrOrAliasToAddr owner'
              balance <- getBalance owner
              printStringLn $ "Balance: " <> show balance
        CmdAddOperator operator' mbMultisig -> do
          operator <- addrOrAliasToAddr operator'
          runMultisigTzbtcContract mbMultisig $
            fromFlatParameter $ AddOperator (#operator .! operator)
        CmdRemoveOperator operator' mbMultisig -> do
          operator <- addrOrAliasToAddr operator'
          runMultisigTzbtcContract mbMultisig $
            fromFlatParameter $ RemoveOperator (#operator .! operator)
        CmdPause mbMultisig -> runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ Pause ()
        CmdUnpause mbMultisig -> runMultisigTzbtcContract mbMultisig $
          fromFlatParameter $ Unpause ()
        CmdSetRedeemAddress redeem' mbMultisig -> do
          redeem <- addrOrAliasToAddr redeem'
          runMultisigTzbtcContract mbMultisig $
            fromFlatParameter $ SetRedeemAddress (#redeem .! redeem)
        CmdTransferOwnership newOwner' mbMultisig -> do
          newOwner <- addrOrAliasToAddr newOwner'
          runMultisigTzbtcContract mbMultisig $
            fromFlatParameter $ TransferOwnership (#newOwner .! newOwner)
        CmdAcceptOwnership p -> runTzbtcContract $
          fromFlatParameter $ AcceptOwnership p
        CmdGetTotalSupply callback -> do
          simpleGetter #totalSupply "Total supply" GetTotalSupply callback
        CmdGetTotalMinted callback -> do
          simpleGetter #totalMinted "Total minted" GetTotalMinted callback
        CmdGetTotalBurned callback -> do
          simpleGetter #totalBurned "Total burned" GetTotalBurned callback
        CmdGetOwner callback ->
          simpleGetter #owner "Owner" GetOwner callback
        CmdGetTokenName callback ->
          simpleGetter #tokenName "Token name" GetTokenName callback
        CmdGetTokenCode callback ->
          simpleGetter #tokenCode "Token code" GetTokenCode callback
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
            Right package -> case addSignature package (pk, sign) of
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
        CmdDeployContract DeployContractOptions {..} -> do
          ownerAlias <- case dcoOwner of
            Just o -> pure o
            Nothing  -> ccUserAlias <$> throwLeft readConfig
          [owner, redeem] <- mapM addrOrAliasToAddr [ownerAlias, dcoRedeem]
          let
            originationParams = OriginationParameters
              { opOwner = owner
              , opRedeemAddress = redeem
              , opBalances = mempty
              , opTokenName = dcoTokenName
              , opTokenCode = dcoTokenCode
              }
          deployTzbtcContract originationParams
        CmdShowConfig -> do
          config <- readConfig
          case config of
            Right c -> printStringLn $ pretty c
            Left err -> printTextLn $ "There was an error reading config:" ++ pretty err
  where
    runMultisigTzbtcContract :: Maybe FilePath -> Parameter SomeTZBTCVersion -> m ()
    runMultisigTzbtcContract mbMultisig param =
      case mbMultisig of
        Just fp -> case toSafeParam param of
          Just subParam -> createMultisigPackage fp subParam
          _ -> printStringLn "Unable to call multisig for View entrypoints"
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
    programInfo =
      info (helper <*> versionOption <*> clientArgParser) $
      mconcat
        [ fullDesc
        , progDesc
            "TZBTC - Wrapped bitcoin on tezos blockchain"
        , header "TZBTC Client"
        , footerDoc $ usageDoc
        ]
    versionOption =
      infoOption
        ("tzbtc-" <> showVersion version)
        (long "version" <> help "Show version.")
    usageDoc :: Maybe Doc
    usageDoc =
      Just $ mconcat
      [ "You can use help for specific COMMAND", linebreak
      , "EXAMPLE:", linebreak
      , "  tzbtc-client mint --help", linebreak
      , "USAGE EXAMPLE:", linebreak
      , "  tzbtc-client mint --to tz1U1h1YzBJixXmaTgpwDpZnbrYHX3fMSpvb --value 100500", linebreak
      , linebreak
      , "  This command will perform transaction insertion", linebreak
      , "  to the chain.", linebreak
      , "  Operation hash is returned as a result.", linebreak
      ]

    simpleGetter ::
      forall a name.
      ( HasStoreTemplateField a name, Buildable a
      , NiceParameterFull a, NoExplicitDefaultEntryPoint a
      ) =>
      Label name -> Text -> (View () a -> FlatParameter SomeTZBTCVersion) ->
      Maybe AddrOrAlias -> m ()
    simpleGetter label descr mkFlatParam = \case
      Just callback' -> do
        callback <- addrOrAliasToAddr callback'
        runTzbtcContract $
          fromFlatParameter $ mkFlatParam $ View () (callingDefTAddress $ toTAddress @a callback)
      Nothing -> do
        printFieldFromStorage @a label descr
