{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
{-# LANGUAGE ApplicativeDo #-}

module Client.Parser
  ( ClientArgs(..)
  , ClientArgsRaw(..)
  , DeployContractOptions (..)
  , clientArgParser
  , parseContractAddressFromOutput
  ) where

import Data.Char (isAlpha, isDigit)
import Fmt (pretty)
import Options.Applicative (help, long, metavar, option, short, str, switch)
import qualified Options.Applicative as Opt
import qualified Text.Megaparsec as P (Parsec, customFailure, many, parse, satisfy, skipManyTill)
import Text.Megaparsec.Char (newline, printChar, space)
import Text.Megaparsec.Char.Lexer (symbol)
import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent(..))

import Lorentz.Contracts.Multisig
import Morley.CLI (mutezOption)
import Morley.Client.Parser (clientConfigParser)
import Morley.Client.TezosClient.Types (AddressOrAlias(..))
import Morley.Tezos.Address (Address, parseAddress)
import Morley.Tezos.Crypto (PublicKey, Signature, parsePublicKey, parseSignature)
import Morley.Util.CLI
import Morley.Util.Named

import CLI.Parser
import Client.Types
import Lorentz.Contracts.TZBTC.Common.Types

clientArgParser :: Opt.Parser ClientArgs
clientArgParser =
  ClientArgs
    <$> (clientConfigParser (pure Nothing))
    <*> clientArgRawParser
    <*> (#userOverride <:!> userOption)
    <*> (#multisigOverride <:!> multisigOption)
    <*> (#contractOverride <:!> contractOverride)
    <*> (#fee <:!> explictFee)
    <*> dryRunSwitch
  where
    multisigOption = mbAddrOrAliasOption "multisig-addr" "The multisig contract address/alias to use"
    contractOverride = mbAddrOrAliasOption "contract-addr" "The tzbtc contract address/alias to use"
    userOption = mbAddrOrAliasOption "user" "User to send operations as"
    explictFee =
      optional $ mutezOption
            Nothing
            (#name :! "fee")
            (#help :! "Fee that is going to be used for the transaction. \
                      \By default fee will be computed automatically."
            )
    dryRunSwitch =
      switch (long "dry-run" <>
              help "Dry run command to ensure correctness of the arguments")

clientArgRawParser :: Opt.Parser ClientArgsRaw
clientArgRawParser = Opt.hsubparser $
  mintCmd <> burnCmd <> transferCmd <> approveCmd
  <> getAllowanceCmd <> getBalanceCmd <> addOperatorCmd
  <> removeOperatorCmd <> pauseCmd <> unpauseCmd
  <> setRedeemAddressCmd <> transferOwnershipCmd <> acceptOwnershipCmd
  <> getTotalSupplyCmd <>  getTotalMintedCmd <> getTotalBurnedCmd
  <> getOwnerCmd <> getTokenMetadataCmd <> getRedeemAddressCmd
  <> getOperatorsCmd <> getOpDescriptionCmd
  <> getBytesToSignCmd <> getTotalBurnedCmd
  <> addSignatureCmd <> signPackageCmd <> callMultisigCmd
  <> deployCmd
  <> deployMultisigCmd
  <> showConfigCmd
  where
    multisigOption :: Opt.Parser (Maybe FilePath)
    multisigOption =
      Opt.optional $ Opt.strOption $ mconcat
      [ long "multisig-package"
      , short 'm'
      , metavar "FILEPATH"
      , help "Create package for multisig transaction and write it to the given file"
      ]
    mintCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    mintCmd =
      (mkCommandParser
         "mint"
         (CmdMint <$> addrOrAliasOption "to" "Address to mint to" <*>
          natOption "value" "Amount to mint" <*> multisigOption)
         "Mint tokens for an account")
    showConfigCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    showConfigCmd =
      (mkCommandParser
         "config"
         (pure CmdShowConfig)
         "Show active configuration")
    burnCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    burnCmd =
      (mkCommandParser
         "burn"
         (CmdBurn <$> burnParamsParser <*> multisigOption)
         "Burn tokens from an account")
    transferCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    transferCmd =
      (mkCommandParser
         "transfer"
         (CmdTransfer <$>
          addrOrAliasOption "from" "Address to transfer from" <*>
          addrOrAliasOption "to" "Address to transfer to" <*>
          natOption "value" "Amount to transfer"
         )
         "Transfer tokens from one account to another")
    approveCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    approveCmd =
      (mkCommandParser
         "approve"
         (CmdApprove <$>
          addrOrAliasOption "spender" "Address of the spender" <*>
          natOption "value" "Amount to approve"
         )
         "Approve transfer of tokens from one account to another")
    getAllowanceCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getAllowanceCmd =
      (mkCommandParser
         "getAllowance"
         (CmdGetAllowance <$>
          ((,) <$> addrOrAliasOption "owner" "Address of the owner" <*>
          addrOrAliasOption "spender" "Address of the spender") <*>
          callbackParser
         )
         "Get allowance for an account")
    getBalanceCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getBalanceCmd =
      (mkCommandParser
         "getBalance"
         (CmdGetBalance <$>
          addrOrAliasOption "address" "Address of the owner" <*>
          callbackParser
         )
         "Get balance for an account")
    addOperatorCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    addOperatorCmd =
      (mkCommandParser
         "addOperator"
         (CmdAddOperator <$>
          addrOrAliasOption "operator" "Address of the operator" <*>
          multisigOption
         )
         "Add an operator")
    removeOperatorCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    removeOperatorCmd =
      (mkCommandParser
         "removeOperator"
         (CmdRemoveOperator <$>
          addrOrAliasOption "operator" "Address of the operator" <*>
          multisigOption
         )
         "Remove an operator")
    pauseCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    pauseCmd =
      (mkCommandParser
         "pause"
         (CmdPause <$> multisigOption)
         "Pause the contract")
    unpauseCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    unpauseCmd =
      (mkCommandParser
         "unpause"
         (CmdUnpause <$> multisigOption)
         "Unpause the contract")
    setRedeemAddressCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    setRedeemAddressCmd =
      (mkCommandParser
         "setRedeemAddress"
         (CmdSetRedeemAddress <$>
          addrOrAliasArg "Redeem address" <*>
          multisigOption
         )
         "Set redeem address")
    transferOwnershipCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    transferOwnershipCmd =
      (mkCommandParser
         "transferOwnership"
         (CmdTransferOwnership <$>
          addrOrAliasArg "new-owner" <*>
          multisigOption
         )
         "Transfer ownership")
    acceptOwnershipCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    acceptOwnershipCmd =
      (mkCommandParser
         "acceptOwnership"
         (CmdAcceptOwnership () <$> multisigOption)
         "Accept ownership")

    getTotalSupplyCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getTotalSupplyCmd =
      (mkCommandParser
         "getTotalSupply"
         (CmdGetTotalSupply <$> callbackParser)
         "Get total supply")

    getTotalMintedCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getTotalMintedCmd =
      (mkCommandParser
         "getTotalMinted"
         (CmdGetTotalMinted <$> callbackParser)
         "Get amount of minted tokens")

    getTotalBurnedCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getTotalBurnedCmd =
      (mkCommandParser "getTotalBurned"
         (CmdGetTotalBurned <$> callbackParser)
         "Get amount of burned tokens")

    getOwnerCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getOwnerCmd =
      (mkCommandParser "getOwner"
         (CmdGetOwner <$> callbackParser)
         "Get current contract owner")

    getTokenMetadataCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getTokenMetadataCmd =
      (mkCommandParser "getTokenMetadata"
         (CmdGetTokenMetadata <$> callbackParser)
         "Get the token metadata")

    getRedeemAddressCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getRedeemAddressCmd =
      (mkCommandParser "getRedeemAddress"
         (CmdGetRedeemAddress <$> callbackParser)
         "Get the redeem address code")

    getOperatorsCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getOperatorsCmd =
      (mkCommandParser "getOperators" (pure CmdGetOperators) "Get list of contract operators")

    getOpDescriptionCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getOpDescriptionCmd =
      mkCommandParser
      "getOpDescription"
      (CmdGetOpDescription <$> namedFilePathOption "package" "Package filepath")
      "Get operation description from given multisig package"

    getBytesToSignCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getBytesToSignCmd =
      mkCommandParser
      "getBytesToSign"
      (CmdGetBytesToSign <$> namedFilePathOption "package" "Package filepath")
      "Get bytes that need to be signed from given multisig package"

    addSignatureCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    addSignatureCmd =
      mkCommandParser
      "addSignature"
      (CmdAddSignature <$> publicKeyOption <*> signatureOption <*>
       namedFilePathOption "package" "Package filepath"
      )
      "Add signature assosiated with the given public key to the given package"

    signPackageCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    signPackageCmd =
      mkCommandParser
      "signPackage"
      (CmdSignPackage <$> namedFilePathOption "package" "Package filepath"
      )
      "Sign given multisig package using secret key from `tezos-client` \
      \assotiated with the user alias from ClientConfig and add signature \
      \to the package."

    callMultisigCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    callMultisigCmd =
      mkCommandParser
      "callMultisig"
      (CmdCallMultisig <$>
       nonEmptyParser (namedFilePathOption "package" "Package filepath")
      )
      "Call multisig contract with the given packages"

    deployCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    deployCmd =
      mkCommandParser
      "deployTzbtcContract"
      (CmdDeployContract
        <$> (#owner <:!> mbAddrOrAliasOption "owner" "Address of the owner")
        <*> deployContractOptions)
      "Deploy TZBTC contract to the chain"
      where
        deployContractOptions :: Opt.Parser DeployContractOptions
        deployContractOptions = asum
          [ Opt.hsubparser $ mconcat
            [ mkCommandParser "v1"
                (DeployContractV1 <$> deployContractOptionsV1)
                "Deploy V1 version of the contract."
            , mkCommandParser "v2"
                (DeployContractV2 <$> deployContractOptionsV2)
                "Deploy V2 version of the contract."
            ]
          , DeployContractV1 <$> deployContractOptionsV1
          ]
        deployContractOptionsV1 :: Opt.Parser DeployContractOptionsV1
        deployContractOptionsV1 = do
          dcoRedeem <- addrOrAliasOption "redeem" "Redeem address"
          dcoTokenMetadata <- parseSingleTokenMetadata
          pure DeployContractOptionsV1 {..}
        deployContractOptionsV2 :: Opt.Parser DeployContractOptionsV2
        deployContractOptionsV2 =
          DeployContractOptionsV2 <$> deployContractOptionsV1

    deployMultisigCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    deployMultisigCmd =
      mkCommandParser
      "deployMultisigContract"
      (CmdDeployMultisigContract <$>
       (Threshold <$> natOption "threshold" "Specialized multisig threshold") <*>
       (Keys <$> many publicKeyOption) <*>
       customErrorsFlag
      )
      "Deploy specialized multisig contract to the chain"
      where
        customErrorsFlag = switch
          (long "use-custom-errors" <>
           help "By default specialized multisig contract fails with 'unit' in all error cases.\n\
                \This flag will deploy the custom version of specialized multisig\n\
                \contract with human-readable string errors.")

    callbackParser :: Opt.Parser (Maybe AddressOrAlias)
    callbackParser = mbAddrOrAliasOption "callback" "Callback address"

addrOrAliasOption :: String -> String -> Opt.Parser AddressOrAlias
addrOrAliasOption name hInfo =
  mkCLOptionParser Nothing (#name :! name) (#help :! hInfo)

mbAddrOrAliasOption :: String -> String -> Opt.Parser (Maybe AddressOrAlias)
mbAddrOrAliasOption = optional ... addrOrAliasOption

addrOrAliasArg :: String -> Opt.Parser AddressOrAlias
addrOrAliasArg hInfo = mkCLArgumentParser Nothing (#help :! hInfo)

natOption :: String -> String -> Opt.Parser Natural
natOption name hInfo = mkCLOptionParser Nothing (#name :! name) (#help :! hInfo)

burnParamsParser :: Opt.Parser BurnParams
burnParamsParser = namedParser Nothing "Amount to burn"

signatureOption :: Opt.Parser Signature
signatureOption = option (eitherReader parseSignatureDo) $ mconcat
  [ long "signature", metavar "SIGNATURE"]

parseSignatureDo :: String -> Either String Signature
parseSignatureDo sig =
  either (Left . mappend "Failed to parse signature: " . pretty) Right $
  parseSignature $ toText sig

publicKeyOption :: Opt.Parser PublicKey
publicKeyOption = option (eitherReader parsePublicKeyDo) $ mconcat
  [ long "public-key", metavar "PUBLIC KEY", help "Address public key"]

parsePublicKeyDo :: String -> Either String PublicKey
parsePublicKeyDo pk =
  either (Left . mappend "Failed to parse signature: " . pretty) Right $
  parsePublicKey $ toText pk

namedFilePathOption :: String -> String -> Opt.Parser FilePath
namedFilePathOption name hInfo = option str $
  mconcat [long name, metavar "FILEPATH", help hInfo]

nonEmptyParser :: Opt.Parser a -> Opt.Parser (NonEmpty a)
nonEmptyParser p = (:|) <$> p <*> many p

-- | Tezos-client output parsers
data OutputParseError = OutputParseError Text Text
  deriving stock (Eq, Show, Ord)

instance ShowErrorComponent OutputParseError where
  showErrorComponent (OutputParseError name err) = toString $
    "Failed to parse " <> name <> ": " <> err

type Parser = P.Parsec OutputParseError Text

isBase58Char :: Char -> Bool
isBase58Char c =
  (isDigit c && c /= '0') || (isAlpha c && c /= 'O' && c /= 'I' && c /= 'l')

tzbtcClientAddressParser :: Parser Address
tzbtcClientAddressParser = do
  P.skipManyTill (printChar <|> newline) $ do
    void $ symbol space "Contract address:"
    rawAddr <- P.many (P.satisfy isBase58Char)
    case parseAddress (fromString rawAddr) of
      Left err -> P.customFailure $ OutputParseError "address" $ pretty err
      Right addr -> return addr

parseContractAddressFromOutput
  :: Text -> Either (ParseErrorBundle Text OutputParseError) Address
parseContractAddressFromOutput output = P.parse tzbtcClientAddressParser "" output
