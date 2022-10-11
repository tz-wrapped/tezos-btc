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
import Morley.Tezos.Address.Alias (ContractAddressOrAlias, ImplicitAddressOrAlias)
import Morley.Tezos.Core (Mutez)
import Morley.Tezos.Crypto (PublicKey, Signature)
import Morley.Util.Named

import CLI.L1AddressOrAlias
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
  = CmdMint L1AddressOrAlias Natural (Maybe FilePath)
  | CmdBurn BurnParams (Maybe FilePath)
  | CmdTransfer L1AddressOrAlias L1AddressOrAlias Natural
  | CmdApprove L1AddressOrAlias Natural
  | CmdGetAllowance (L1AddressOrAlias, L1AddressOrAlias) (Maybe ContractAddressOrAlias)
  | CmdGetBalance L1AddressOrAlias (Maybe ContractAddressOrAlias)
  | CmdAddOperator L1AddressOrAlias (Maybe FilePath)
  | CmdRemoveOperator L1AddressOrAlias (Maybe FilePath)
  | CmdPause (Maybe FilePath)
  | CmdUnpause (Maybe FilePath)
  | CmdSetRedeemAddress L1AddressOrAlias (Maybe FilePath)
  | CmdTransferOwnership L1AddressOrAlias (Maybe FilePath)
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
  | CmdDeployContract ("owner" :! Maybe L1AddressOrAlias) !DeployContractOptions
  | CmdDeployMultisigContract Threshold Keys Bool
  | CmdShowConfig

data DeployContractOptions
  = DeployContractV1 DeployContractOptionsV1
  | DeployContractV2 DeployContractOptionsV2

data DeployContractOptionsV1 = DeployContractOptionsV1
  { dcoRedeem :: !L1AddressOrAlias
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
