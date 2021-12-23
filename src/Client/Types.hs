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
import Morley.Client.TezosClient.Types (AddressOrAlias(..))
import Morley.Michelson.Typed (IsoValue)
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
      ("userOverride" :! Maybe AddressOrAlias)
      ("multisigOverride" :! Maybe AddressOrAlias)
      ("contractOverride" :! Maybe AddressOrAlias)
      ("fee" :! Maybe Mutez)
      Bool

data ClientArgsRaw
  = CmdMint AddressOrAlias Natural (Maybe FilePath)
  | CmdBurn BurnParams (Maybe FilePath)
  | CmdTransfer AddressOrAlias AddressOrAlias Natural
  | CmdApprove AddressOrAlias Natural
  | CmdGetAllowance (AddressOrAlias, AddressOrAlias) (Maybe AddressOrAlias)
  | CmdGetBalance AddressOrAlias (Maybe AddressOrAlias)
  | CmdAddOperator AddressOrAlias (Maybe FilePath)
  | CmdRemoveOperator AddressOrAlias (Maybe FilePath)
  | CmdPause (Maybe FilePath)
  | CmdUnpause (Maybe FilePath)
  | CmdSetRedeemAddress AddressOrAlias (Maybe FilePath)
  | CmdTransferOwnership AddressOrAlias (Maybe FilePath)
  | CmdAcceptOwnership AcceptOwnershipParams (Maybe FilePath)
  | CmdGetTotalSupply (Maybe AddressOrAlias)
  | CmdGetTotalMinted (Maybe AddressOrAlias)
  | CmdGetTotalBurned (Maybe AddressOrAlias)
  | CmdGetOwner (Maybe AddressOrAlias)
  | CmdGetTokenMetadata (Maybe AddressOrAlias)
  | CmdGetRedeemAddress (Maybe AddressOrAlias)
  | CmdGetOperators
  | CmdGetOpDescription FilePath
  | CmdGetBytesToSign FilePath
  | CmdAddSignature PublicKey Signature FilePath
  | CmdSignPackage FilePath
  | CmdCallMultisig (NonEmpty FilePath)
  | CmdDeployContract ("owner" :! Maybe AddressOrAlias) !DeployContractOptions
  | CmdDeployMultisigContract Threshold Keys Bool
  | CmdShowConfig

data DeployContractOptions
  = DeployContractV1 DeployContractOptionsV1
  | DeployContractV2 DeployContractOptionsV2

data DeployContractOptionsV1 = DeployContractOptionsV1
  { dcoRedeem :: !AddressOrAlias
  , dcoTokenMetadata :: !TokenMetadata
  }

data DeployContractOptionsV2 = DeployContractOptionsV2
  { dcoV1 :: DeployContractOptionsV1
  }

data ConfigOverride = ConfigOverride
  { coTzbtcUser :: Maybe AddressOrAlias
  , coTzbtcMultisig :: Maybe AddressOrAlias
  , coTzbtcContract :: Maybe AddressOrAlias
  } deriving stock Show

data AlmostStorage ver = AlmostStorage
  { asBigMapId :: Natural
  , asFields :: StorageFields ver
  } deriving stock (Show, Generic)
    deriving anyclass IsoValue

data ConfirmationResult = Confirmed | Canceled
