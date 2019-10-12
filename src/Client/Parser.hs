{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -}
module Client.Parser
  ( AddrOrAlias
  , ClientArgs(..)
  , ClientArgsRaw(..)
  , clientArgParser
  , parseAddressFromOutput
  , parseSignatureFromOutput
  ) where

import Data.Char (isAlpha, isDigit, toUpper)
import Fmt (pretty)
import Named ((!))
import Options.Applicative
  (argument, auto, eitherReader, help, long, metavar, option, optional,
  showDefaultWith, str, strOption, switch, value)
import qualified Options.Applicative as Opt
import qualified Text.Megaparsec as P
  (Parsec, customFailure, many, parse, satisfy)
import Text.Megaparsec.Char (space, newline)
import Text.Megaparsec.Char.Lexer (symbol)
import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent(..))

import Tezos.Crypto (PublicKey, Signature, parsePublicKey, parseSignature)
import Tezos.Address (Address, parseAddress)

import CLI.Parser
import Client.Types
import Lorentz.Contracts.TZBTC.Types

-- | Client argument with optional dry-run flag
data ClientArgs = ClientArgs ClientArgsRaw Bool

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
  | CmdAcceptOwnership AcceptOwnershipParams
  | CmdGetTotalSupply (Maybe AddrOrAlias)
  | CmdGetTotalMinted (Maybe AddrOrAlias)
  | CmdGetTotalBurned (Maybe AddrOrAlias)
  | CmdGetAdministrator (Maybe AddrOrAlias)
  | CmdSetupClient ClientConfigPartial
  | CmdGetOpDescription FilePath
  | CmdGetBytesToSign FilePath
  | CmdAddSignature PublicKey Signature FilePath
  | CmdSignPackage FilePath
  | CmdCallMultisig (NonEmpty FilePath)
  | CmdConfig Bool ClientConfigPartial

clientArgParser :: Opt.Parser ClientArgs
clientArgParser = ClientArgs <$> clientArgRawParser <*> dryRunSwitch
  where
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
  <> getAdministratorCmd <> setupUserCmd <> getOpDescriptionCmd
  <> getBytesToSignCmd <> getTotalBurnedCmd
  <> addSignatureCmd <> signPackageCmd <> callMultisigCmd
  <> configCmd
  where
    multisigOption :: Opt.Parser (Maybe FilePath)
    multisigOption =
      Opt.optional $ Opt.strOption $ mconcat
      [ long "multisig"
      , metavar "FILEPATH"
      , help "Create package for multisig transaction and write it to the given file"
      ]
    configCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    configCmd =
      mkCommandParser "config"
        (CmdConfig <$> editSwitch <*> clientConfigParserEdit)
        "Show or edit config. Use the --edit flag with required options to set indvidual fields."
      where
        editSwitch =
          switch (long "edit" <>
                  help "Edit config using command arguments")
    clientConfigParser :: Opt.Parser ClientConfigPartial
    clientConfigParser = ClientConfig <$>
      (partialParser $ urlOption "node-url" "Node url") <*>
      (partialParser $ intOption "node-port" "Node port") <*>
      (partialParser $ namedAddressOption Nothing "contract-address"
      "Contract's address") <*>
      (partialParserMaybe $ namedAddressOption Nothing "multisig-address" "Multisig contract address") <*>
      (partialParser $ option str $ mconcat
       [ long "alias"
       , metavar "ADDRESS_ALIAS"
       , help "tezos-client alias for user."
       ])
      <*> (partialParser $ tezosClientFilePathOption)
    clientConfigParserEdit :: Opt.Parser ClientConfigPartial
    clientConfigParserEdit = ClientConfig <$>
      (partialParser $ urlOption "node-url" "Node url") <*>
      (partialParser $ intOption "node-port" "Node port") <*>
      (partialParser $ namedAddressOption Nothing "contract-address"
      "Contract's address") <*>
      (partialParserFlattenMaybe $
        optional $ nullableAddressOption
          ! #name "multisig-address"
          ! #hinfo "Multisig contract address. Use 'null' to clear current value.") <*>
      (partialParser $ option str $ mconcat
       [ long "alias"
       , metavar "ADDRESS_ALIAS"
       , help "tezos-client alias for user."
       ])
      <*> (partialParser $ tezosClientFilePathOptionWithoutDefault)
      where
        -- Handles the case where the value is explicitly provided to be null
        -- by using the special 'null' value.
        partialParserFlattenMaybe :: Opt.Parser (Maybe (Maybe a)) -> (Opt.Parser (Partial s (Maybe a)))
        partialParserFlattenMaybe p = unwrapOuter <$> p
          where
            unwrapOuter :: Maybe (Maybe a) -> Partial s (Maybe a)
            unwrapOuter (Just a) = Available a
            unwrapOuter Nothing = Unavilable

    setupUserCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    setupUserCmd =
      (mkCommandParser
         "setupClient"
         (CmdSetupClient <$> clientConfigParser)
         ("Create a configuration file using node url, node port, contract address, \
          \multi-sig contract address(optional), user address, user address alias and \
          \filepath to the tezos-client executable"))
    mintCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    mintCmd =
      (mkCommandParser
         "mint"
         (CmdMint <$> addrOrAliasOption "to" "Address to mint to" <*>
          natOption "value" "Amount to mint" <*> multisigOption)
         "Mint tokens for an account")
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
          mbAddrOrAliasOption "callback" "Callback address"
         )
         "Get allowance for an account")
    getBalanceCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getBalanceCmd =
      (mkCommandParser
         "getBalance"
         (CmdGetBalance <$>
          addrOrAliasOption "address" "Address of the owner" <*>
          mbAddrOrAliasOption "callback" "Callback address"
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
         (pure $ CmdAcceptOwnership ())
         "Accept ownership")

    getTotalSupplyCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getTotalSupplyCmd =
      (mkCommandParser
         "getTotalSupply"
         (CmdGetTotalSupply <$>
          mbAddrOrAliasOption "callback" "Callback address")
         "Get total supply")

    getTotalMintedCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getTotalMintedCmd =
      (mkCommandParser
         "getTotalMinted"
         (CmdGetTotalMinted <$>
          mbAddrOrAliasOption "callback" "Callback address")
         "Get amount of minted tokens")

    getTotalBurnedCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getTotalBurnedCmd =
      (mkCommandParser
         "getTotalBurned"
         (CmdGetTotalBurned <$>
          mbAddrOrAliasOption "callback" "Callback address")
         "Get amount of burned tokens")

    getAdministratorCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getAdministratorCmd =
      (mkCommandParser
         "getAdministrator"
         (CmdGetAdministrator <$>
          mbAddrOrAliasOption "callback" "Callback address")
         "Get current contract administrator")

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

addrOrAliasOption :: String -> String -> Opt.Parser AddrOrAlias
addrOrAliasOption name hInfo =
  option str $ mconcat
  [ metavar "ADDRESS | ALIAS"
  , long name
  , help hInfo
  ]

mbAddrOrAliasOption :: String -> String -> Opt.Parser (Maybe AddrOrAlias)
mbAddrOrAliasOption name hInfo =
  optional $ strOption $ mconcat
  [ metavar "ADDRESS | ALIAS"
  , long name
  , help hInfo
  ]

addrOrAliasArg :: String -> Opt.Parser AddrOrAlias
addrOrAliasArg hInfo =
  argument str $ mconcat
  [ metavar "ADDRESS | ALIAS"
  , help hInfo
  ]

natOption :: String -> String -> Opt.Parser Natural
natOption name hInfo =
  option auto $ mconcat
  [ metavar $ toUpper <$> name
  , long name
  , help hInfo
  ]

burnParamsParser :: Opt.Parser BurnParams
burnParamsParser = getParser "Amount to burn"

urlOption :: String -> String -> Opt.Parser Text
urlOption name hInfo = option str $
  mconcat [long name, metavar "URL", help hInfo]

signatureOption :: Opt.Parser Signature
signatureOption = option (eitherReader parseSignatureDo) $ mconcat
  [ long "signature", metavar "SIGNATURE"]

parseSignatureDo :: String -> Either String Signature
parseSignatureDo sig =
  either (Left . mappend "Failed to parse signature: " . pretty) Right $
  parseSignature $ toText sig

publicKeyOption :: Opt.Parser PublicKey
publicKeyOption = option (eitherReader parsePublicKeyDo) $ mconcat
  [ long "public-key", metavar "PUBLIC KEY"]

parsePublicKeyDo :: String -> Either String PublicKey
parsePublicKeyDo pk =
  either (Left . mappend "Failed to parse signature: " . pretty) Right $
  parsePublicKey $ toText pk

intOption :: String -> String -> Opt.Parser Int
intOption name hInfo = option auto $
  mconcat [long name, metavar "PORT", help hInfo]

tezosClientFilePathOption :: Opt.Parser FilePath
tezosClientFilePathOption = option str $
  mconcat [ long "tezos-client", metavar "FILEPATH", help "tezos-client executable"
          , value "tezos-client", showDefaultWith (<> " from $PATH")
          ]

tezosClientFilePathOptionWithoutDefault :: Opt.Parser FilePath
tezosClientFilePathOptionWithoutDefault = option str $
  mconcat [ long "tezos-client", metavar "FILEPATH", help "tezos-client executable"]

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
  void $ newline
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
