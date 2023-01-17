{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}
module Client.Types
  ( AlmostStorage (..)
  , ClientArgs (..)
  , ClientArgsRaw (..)
  , ConfigOverride (..)
  , ConfirmationResult (..)
  , DeployContractOptions (..)
  , DeployContractOptionsV1 (..)
  , DeployContractOptionsV2 (..)
  ) where

import Morley.Client (MorleyClientConfig)
import Morley.Michelson.Typed (IsoValue)
import Morley.Tezos.Address.Alias
  (ContractAddressOrAlias, ImplicitAddressOrAlias, SomeAddressOrAlias)
import Morley.Tezos.Core (Mutez)
import Morley.Tezos.Crypto (PublicKey, Signature)
import Morley.Util.Named

import Lorentz.Contracts.Metadata
import Lorentz.Contracts.Multisig
import Lorentz.Contracts.TZBTC.Common.Types

-- | Client argument with optional dry-run flag
data ClientArgs =
  ClientArgs
    MorleyClientConfig
    ClientArgsRaw
      ("userOverride" :! Maybe ImplicitAddressOrAlias)
      ("multisigOverride" :! Maybe ContractAddressOrAlias)
      ("contractOverride" :! Maybe ContractAddressOrAlias)
      ("fee" :! Maybe Mutez)
      Bool

data ClientArgsRaw
  = CmdMint SomeAddressOrAlias Natural (Maybe FilePath)
  | CmdBurn BurnParams (Maybe FilePath)
  | CmdTransfer SomeAddressOrAlias SomeAddressOrAlias Natural
  | CmdApprove SomeAddressOrAlias Natural
  | CmdGetAllowance (SomeAddressOrAlias, SomeAddressOrAlias) (Maybe ContractAddressOrAlias)
  | CmdGetBalance SomeAddressOrAlias (Maybe ContractAddressOrAlias)
  | CmdAddOperator SomeAddressOrAlias (Maybe FilePath)
  | CmdRemoveOperator SomeAddressOrAlias (Maybe FilePath)
  | CmdPause (Maybe FilePath)
  | CmdUnpause (Maybe FilePath)
  | CmdSetRedeemAddress SomeAddressOrAlias (Maybe FilePath)
  | CmdTransferOwnership SomeAddressOrAlias (Maybe FilePath)
  | CmdAcceptOwnership AcceptOwnershipParams (Maybe FilePath)
  | CmdGetTotalSupply (Maybe ContractAddressOrAlias)
  | CmdGetTotalMinted (Maybe ContractAddressOrAlias)
  | CmdGetTotalBurned (Maybe ContractAddressOrAlias)
  | CmdGetOwner (Maybe ContractAddressOrAlias)
  | CmdGetTokenMetadata (Maybe ContractAddressOrAlias)
  | CmdGetRedeemAddress (Maybe ContractAddressOrAlias)
  | CmdGetOperators
  | CmdGetOpDescription FilePath
  | CmdGetBytesToSign FilePath
  | CmdAddSignature PublicKey Signature FilePath
  | CmdSignPackage FilePath
  | CmdCallMultisig (NonEmpty FilePath)
  | CmdDeployContract ("owner" :! Maybe SomeAddressOrAlias) !DeployContractOptions
  | CmdDeployMultisigContract Threshold Keys Bool
  | CmdShowConfig

data DeployContractOptions
  = DeployContractV1 DeployContractOptionsV1
  | DeployContractV2 DeployContractOptionsV2

data DeployContractOptionsV1 = DeployContractOptionsV1
  { dcoRedeem :: !SomeAddressOrAlias
  , dcoTokenMetadata :: !TokenMetadata
  }

data DeployContractOptionsV2 = DeployContractOptionsV2
  { dcoV1 :: DeployContractOptionsV1
  }

data ConfigOverride = ConfigOverride
  { coTzbtcUser :: Maybe ImplicitAddressOrAlias
  , coTzbtcMultisig :: Maybe ContractAddressOrAlias
  , coTzbtcContract :: Maybe ContractAddressOrAlias
  } deriving stock Show

data AlmostStorage ver = AlmostStorage
  { asBigMapId :: Natural
  , asFields :: StorageFields ver
  } deriving stock (Show, Generic)
    deriving anyclass IsoValue

data ConfirmationResult = Confirmed | Canceled
