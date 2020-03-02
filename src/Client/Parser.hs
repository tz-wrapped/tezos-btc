{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
{-# LANGUAGE ApplicativeDo #-}

module Client.Parser
  ( AddrOrAlias
  , ClientArgs(..)
  , ClientArgsRaw(..)
  , DeployContractOptions (..)
  , clientArgParser
  , parseAddressFromOutput
  , parseSignatureFromOutput
  ) where

import Data.Char (isAlpha, isDigit)
import Fmt (Buildable, pretty)
import Options.Applicative (eitherReader, help, long, metavar, option, optional, short, str, switch)
import qualified Options.Applicative as Opt
import qualified Text.Megaparsec as P (Parsec, customFailure, many, parse, satisfy)
import Text.Megaparsec.Char (eol, space)
import Text.Megaparsec.Char.Lexer (symbol)
import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent(..))

import Lorentz.Contracts.Multisig
import Michelson.Text (mt)
import Tezos.Address (Address, parseAddress)
import Tezos.Crypto (PublicKey, Signature, parsePublicKey, parseSignature)
import Util.CLI
import Util.Named

import CLI.Parser
import Client.Types
import Lorentz.Contracts.TZBTC.Types
import Morley.CLI

clientArgParser :: Opt.Parser ClientArgs
clientArgParser =
  ClientArgs
    <$> clientArgRawParser
    <*> (#userOverride <.!> userOption)
    <*> (#multisigOverride <.!> multisigOption)
    <*> dryRunSwitch
  where
    multisigOption = mbAddrOrAliasOption "multisig-addr" "The multisig contract address/alias to use"
    userOption = mbAddrOrAliasOption "user" "User to send operations as"
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
  <> getOwnerCmd <> getTokenNameCmd <> getTokenCodeCmd <> getRedeemAddressCmd
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

    getTokenNameCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getTokenNameCmd =
      (mkCommandParser "getTokenName"
         (CmdGetTokenName <$> callbackParser)
         "Get the token name")

    getTokenCodeCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getTokenCodeCmd =
      (mkCommandParser "getTokenCode"
         (CmdGetTokenCode <$> callbackParser)
         "Get the token code")

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
        <$> deployContractOptions)
      "Deploy TZBTC contract to the chain"
      where
        deployContractOptions :: Opt.Parser DeployContractOptions
        deployContractOptions = do
          dcoOwner <- mbAddrOrAliasOption "owner" "Address of the owner"
          dcoRedeem <- addrOrAliasOption "redeem" "Redeem address"
          dcoTokenName <-
            mTextOption (Just [mt|TZBTC|])
              (#name .! "token-name") (#help .! "Name of this token")
          dcoTokenCode <-
            mTextOption (Just [mt|TZBTC|])
              (#name .! "token-code") (#help .! "Token code")
          pure DeployContractOptions {..}

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

    callbackParser :: Opt.Parser (Maybe AddrOrAlias)
    callbackParser = mbAddrOrAliasOption "callback" "Callback address"

-- This wrapper is needed to override 'getMetavar' for 'AddrOrAlias'
-- because it is a type synonym for 'Text'. I think one day we will
-- be end up using @AddrOrAlias@ from @morley-nettest@.
newtype AddrOrAliasWrapper =
  AddrOrAliasWrapper
    { unAddrOrAliasWrapper :: AddrOrAlias
    } deriving newtype Buildable

instance HasCLReader AddrOrAliasWrapper where
  getReader = AddrOrAliasWrapper <$> getReader
  getMetavar = "ADDRESS | ALIAS"

addrOrAliasOption :: String -> String -> Opt.Parser AddrOrAlias
addrOrAliasOption name hInfo =
  unAddrOrAliasWrapper <$>
  mkCLOptionParser Nothing (#name .! name) (#help .! hInfo)

mbAddrOrAliasOption :: String -> String -> Opt.Parser (Maybe AddrOrAlias)
mbAddrOrAliasOption = optional ... addrOrAliasOption

addrOrAliasArg :: String -> Opt.Parser AddrOrAlias
addrOrAliasArg hInfo =
  unAddrOrAliasWrapper <$> mkCLArgumentParser Nothing (#help .! hInfo)

natOption :: String -> String -> Opt.Parser Natural
natOption name hInfo = mkCLOptionParser Nothing (#name .! name) (#help .! hInfo)

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

-- Tezos-client output parsers
data OutputParseError = OutputParseError Text Text
  deriving stock (Eq, Show, Ord)

instance ShowErrorComponent OutputParseError where
  showErrorComponent (OutputParseError name err) = toString $
    "Failed to parse " <> name <> ": " <> err

type Parser = P.Parsec OutputParseError Text

isBase58Char :: Char -> Bool
isBase58Char c =
  (isDigit c && c /= '0') || (isAlpha c && c /= 'O' && c /= 'I' && c /= 'l')

tezosClientSignatureParser :: Parser Signature
tezosClientSignatureParser = do
  void $ symbol space "Signature:"
  rawSignature <- P.many (P.satisfy isBase58Char)
  case parseSignature (fromString rawSignature) of
    Left err -> P.customFailure $ OutputParseError "signature" $ pretty err
    Right sign -> return sign

tezosClientAddressParser :: Parser (Address, PublicKey)
tezosClientAddressParser = do
  void $ symbol space "Hash:"
  rawAddress <- fromString <$> P.many (P.satisfy isBase58Char)
  void $ eol
  void $ symbol space "Public Key:"
  rawPublicKey <- fromString <$> P.many (P.satisfy isBase58Char)
  case (parseAddress rawAddress, parsePublicKey rawPublicKey) of
    (Right addr, Right pk) -> return (addr, pk)
    (Left err, Right _) -> P.customFailure $ OutputParseError "address" $ pretty err
    (Right _, Left err) -> P.customFailure $ OutputParseError "public key" $ pretty err
    (Left err1, Left err2) -> P.customFailure $
      OutputParseError "address and public key" $ pretty err1 <> "\n" <> pretty err2

parseSignatureFromOutput
  :: Text -> Either (ParseErrorBundle Text OutputParseError) Signature
parseSignatureFromOutput output = P.parse tezosClientSignatureParser "" output

parseAddressFromOutput
  :: Text -> Either (ParseErrorBundle Text OutputParseError) (Address, PublicKey)
parseAddressFromOutput output = P.parse tezosClientAddressParser "" output
