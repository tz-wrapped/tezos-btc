<!---
- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
-
- SPDX-License-Identifier: LicenseRef-Proprietary
-->

# TZBTC

**Code revision:** [6bea302](https://github.com/serokell/tezos-btc/commit/6bea302e5f45be7d4e38f5e840bdef1f74614bc2) *(Wed Nov 27 13:02:06 2019 +0300)*

This contract is implemented using Lorentz language.
Basically, this contract is [FA1.2](https://gitlab.com/serokell/morley/tzip/blob/master/A/FA1.2.md)-compatible approvable ledger that maps user addresses to their token balances. The main idea of this token contract is to provide 1-to-1 correspondance with BTC.
There are two special entities for this contract:
* `owner` -- owner of the TZBTC contract, capable in unpausing contract, adding/removing operators, transfering ownership and upgrading contract. There is only one owner of the contract.
* `operator` -- entity which is capable in pausing the contract minting and burning tokens. There may be several operators added by the owner.

## Top-level entry points of upgradeable contract.

These are entry points of the contract.

---

### `GetVersion`

This entry point is used to get contract version.

**Parameter:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `GetVersion` constructor.
    + **In Haskell:** `GetVersion (·)`
    + **In Michelson:** `Left (Left (Left (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>





---

### `GetAllowance`

This entry point is used to get allowance for an account.

**Parameter:** [`View`](#types-View) (***owner*** : [`Address`](#types-Address-simplified), ***spender*** : [`Address`](#types-Address-simplified)) [`Natural`](#types-Natural)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `GetAllowance` constructor.
    + **In Haskell:** `GetAllowance (·)`
    + **In Michelson:** `Left (Left (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `GetBalance`

This entry point is used to get balance in an account.

**Parameter:** [`View`](#types-View) (***owner*** : [`Address`](#types-Address-simplified)) [`Natural`](#types-Natural)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `GetBalance` constructor.
    + **In Haskell:** `GetBalance (·)`
    + **In Michelson:** `Left (Right (Left (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `GetTotalSupply`

This entry point is used to get total number of tokens.

**Parameter:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `GetTotalSupply` constructor.
    + **In Haskell:** `GetTotalSupply (·)`
    + **In Michelson:** `Left (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `GetTotalMinted`

This entry point is used to get total number of minted tokens.

**Parameter:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `GetTotalMinted` constructor.
    + **In Haskell:** `GetTotalMinted (·)`
    + **In Michelson:** `Right (Left (Left (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `GetTotalBurned`

This entry point is used to get total number of burned tokens.

**Parameter:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `GetTotalBurned` constructor.
    + **In Haskell:** `GetTotalBurned (·)`
    + **In Michelson:** `Right (Left (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `GetOwner`

This entry point is used to get current owner.

**Parameter:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Address`](#types-Address-simplified)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `GetOwner` constructor.
    + **In Haskell:** `GetOwner (·)`
    + **In Michelson:** `Right (Right (Left (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `SafeEntrypoints`

This entry point is used to call the safe entrypoints of the contract. Entrypoints are 'safe' because they don't have unsafe arguments, such as arguments with type `contract p` so that they can be safely used in operations that add them to the chain (since in babylon values with type `contract p` are prohibited in storage and code constants), in contrast to various Get* entrypoints, which have `contract p` and have to be handled additionally (basically, we have to pass simple `address` instead of `contract p` and call `CONTRACT`, which can fail).

**Parameter:** [`Parameter.SafeParameter`](#types-Parameter.SafeParameter)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



#### Top-level entry points of upgradeable contract.

These are entry points of the contract.

---

##### `Run`

This entry point is used to call the packed entrypoints in the contract.

**Parameter:** [`UParam`](#types-Upgradable-parameter)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Run` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Left (Left (Left (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

##### `Upgrade`

This entry point is used to update the contract to a new version.

**Parameter:** (***newVersion*** : [`Natural`](#types-Natural), ***migrationScript*** : [`MigrationScript`](#types-MigrationScript), ***newCode*** : [`UContractRouter`](#types-UContractRouter))

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Upgrade` constructor.
    + **In Haskell:** `Upgrade (·)`
    + **In Michelson:** `Left (Left (Left (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration

* [`UpgVersionMismatch`](#errors-UpgVersionMismatch) — The expected version does not match the version of the supplied code.



---

##### `EpwBeginUpgrade`

This entry point is used to start an entrypoint wise upgrade of the contract.

**Parameter:** [`Natural`](#types-Natural)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `EpwBeginUpgrade` constructor.
    + **In Haskell:** `EpwBeginUpgrade (·)`
    + **In Michelson:** `Left (Left (Right (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration

* [`UpgVersionMismatch`](#errors-UpgVersionMismatch) — The expected version does not match the version of the supplied code.



---

##### `EpwApplyMigration`

This entry point is used to apply an migration script as part of an upgrade.

**Parameter:** ***migrationscript*** : [`MigrationScript`](#types-MigrationScript)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `EpwApplyMigration` constructor.
    + **In Haskell:** `EpwApplyMigration (·)`
    + **In Michelson:** `Left (Left (Right (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



---

##### `EpwSetCode`

This entry point is used to set the dispatching code that calls the packed entrypoints.

**Parameter:** ***contractcode*** : [`UContractRouter`](#types-UContractRouter)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `EpwSetCode` constructor.
    + **In Haskell:** `EpwSetCode (·)`
    + **In Michelson:** `Left (Right (Left (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



---

##### `EpwFinishUpgrade`

This entry point is used to mark that an upgrade has been finsihed.

**Parameter:** none (pass unit)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `EpwFinishUpgrade` constructor.
    + **In Haskell:** `EpwFinishUpgrade (·)`
    + **In Michelson:** `Left (Right (Left (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



---

##### `Transfer`

This entry point is used transfer tokens from one account to another.

**Parameter:** (***from*** : [`Address`](#types-Address-simplified), ***to*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Transfer` constructor.
    + **In Haskell:** `Transfer (·)`
    + **In Michelson:** `Left (Right (Right (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

##### `Approve`

This entry point is used approve transfer of tokens from one account to another.

**Parameter:** (***spender*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Approve` constructor.
    + **In Haskell:** `Approve (·)`
    + **In Michelson:** `Left (Right (Right (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

##### `Mint`

This entry point is used mint new tokes for an account.

**Parameter:** (***to*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Mint` constructor.
    + **In Haskell:** `Mint (·)`
    + **In Michelson:** `Right (Left (Left (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

##### `Burn`

This entry point is used burn tokes from the redeem address.

**Parameter:** ***value*** : [`Natural`](#types-Natural)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Burn` constructor.
    + **In Haskell:** `Burn (·)`
    + **In Michelson:** `Right (Left (Left (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

##### `AddOperator`

This entry point is used to add a new operator.

**Parameter:** ***operator*** : [`Address`](#types-Address-simplified)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `AddOperator` constructor.
    + **In Haskell:** `AddOperator (·)`
    + **In Michelson:** `Right (Left (Right (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

##### `RemoveOperator`

This entry point is used to remove an operator.

**Parameter:** ***operator*** : [`Address`](#types-Address-simplified)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `RemoveOperator` constructor.
    + **In Haskell:** `RemoveOperator (·)`
    + **In Michelson:** `Right (Left (Right (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

##### `SetRedeemAddress`

This entry point is used to set the redeem address.

**Parameter:** ***redeem*** : [`Address`](#types-Address-simplified)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `SetRedeemAddress` constructor.
    + **In Haskell:** `SetRedeemAddress (·)`
    + **In Michelson:** `Right (Right (Left (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

##### `Pause`

This entry point is used to pause the contract.

**Parameter:** [`()`](#types-lparenrparen)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Pause` constructor.
    + **In Haskell:** `Pause (·)`
    + **In Michelson:** `Right (Right (Left (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

##### `Unpause`

This entry point is used to resume the contract during a paused state.

**Parameter:** [`()`](#types-lparenrparen)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Unpause` constructor.
    + **In Haskell:** `Unpause (·)`
    + **In Michelson:** `Right (Right (Right (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

##### `TransferOwnership`

This entry point is used to transfer ownership to a new owner.

**Parameter:** ***newOwner*** : [`Address`](#types-Address-simplified)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `TransferOwnership` constructor.
    + **In Haskell:** `TransferOwnership (·)`
    + **In Michelson:** `Right (Right (Right (Right (Left (·)))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

##### `AcceptOwnership`

This entry point is used to accept ownership by a new owner.

**Parameter:** [`()`](#types-lparenrparen)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `AcceptOwnership` constructor.
    + **In Haskell:** `AcceptOwnership (·)`
    + **In Michelson:** `Right (Right (Right (Right (Right (·)))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `SafeEntrypoints (·)`
    + **In Michelson:** `Right (Right (Right (·)))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration





---

### `Run`

This entry point is used to call the packed entrypoints in the contract.

**Parameter:** [`UParam`](#types-Upgradable-parameter)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Run` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Left (Left (Left (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `Upgrade`

This entry point is used to update the contract to a new version.

**Parameter:** (***newVersion*** : [`Natural`](#types-Natural), ***migrationScript*** : [`MigrationScript`](#types-MigrationScript), ***newCode*** : [`UContractRouter`](#types-UContractRouter))

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Upgrade` constructor.
    + **In Haskell:** `Upgrade (·)`
    + **In Michelson:** `Left (Left (Left (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration

* [`UpgVersionMismatch`](#errors-UpgVersionMismatch) — The expected version does not match the version of the supplied code.



---

### `EpwBeginUpgrade`

This entry point is used to start an entrypoint wise upgrade of the contract.

**Parameter:** [`Natural`](#types-Natural)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `EpwBeginUpgrade` constructor.
    + **In Haskell:** `EpwBeginUpgrade (·)`
    + **In Michelson:** `Left (Left (Right (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration

* [`UpgVersionMismatch`](#errors-UpgVersionMismatch) — The expected version does not match the version of the supplied code.



---

### `EpwApplyMigration`

This entry point is used to apply an migration script as part of an upgrade.

**Parameter:** ***migrationscript*** : [`MigrationScript`](#types-MigrationScript)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `EpwApplyMigration` constructor.
    + **In Haskell:** `EpwApplyMigration (·)`
    + **In Michelson:** `Left (Left (Right (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



---

### `EpwSetCode`

This entry point is used to set the dispatching code that calls the packed entrypoints.

**Parameter:** ***contractcode*** : [`UContractRouter`](#types-UContractRouter)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `EpwSetCode` constructor.
    + **In Haskell:** `EpwSetCode (·)`
    + **In Michelson:** `Left (Right (Left (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



---

### `EpwFinishUpgrade`

This entry point is used to mark that an upgrade has been finsihed.

**Parameter:** none (pass unit)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `EpwFinishUpgrade` constructor.
    + **In Haskell:** `EpwFinishUpgrade (·)`
    + **In Michelson:** `Left (Right (Left (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



---

### `Transfer`

This entry point is used transfer tokens from one account to another.

**Parameter:** (***from*** : [`Address`](#types-Address-simplified), ***to*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Transfer` constructor.
    + **In Haskell:** `Transfer (·)`
    + **In Michelson:** `Left (Right (Right (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `Approve`

This entry point is used approve transfer of tokens from one account to another.

**Parameter:** (***spender*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Approve` constructor.
    + **In Haskell:** `Approve (·)`
    + **In Michelson:** `Left (Right (Right (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `Mint`

This entry point is used mint new tokes for an account.

**Parameter:** (***to*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Mint` constructor.
    + **In Haskell:** `Mint (·)`
    + **In Michelson:** `Right (Left (Left (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `Burn`

This entry point is used burn tokes from the redeem address.

**Parameter:** ***value*** : [`Natural`](#types-Natural)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Burn` constructor.
    + **In Haskell:** `Burn (·)`
    + **In Michelson:** `Right (Left (Left (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `AddOperator`

This entry point is used to add a new operator.

**Parameter:** ***operator*** : [`Address`](#types-Address-simplified)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `AddOperator` constructor.
    + **In Haskell:** `AddOperator (·)`
    + **In Michelson:** `Right (Left (Right (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `RemoveOperator`

This entry point is used to remove an operator.

**Parameter:** ***operator*** : [`Address`](#types-Address-simplified)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `RemoveOperator` constructor.
    + **In Haskell:** `RemoveOperator (·)`
    + **In Michelson:** `Right (Left (Right (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `SetRedeemAddress`

This entry point is used to set the redeem address.

**Parameter:** ***redeem*** : [`Address`](#types-Address-simplified)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `SetRedeemAddress` constructor.
    + **In Haskell:** `SetRedeemAddress (·)`
    + **In Michelson:** `Right (Right (Left (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `Pause`

This entry point is used to pause the contract.

**Parameter:** [`()`](#types-lparenrparen)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Pause` constructor.
    + **In Haskell:** `Pause (·)`
    + **In Michelson:** `Right (Right (Left (Right (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `Unpause`

This entry point is used to resume the contract during a paused state.

**Parameter:** [`()`](#types-lparenrparen)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `Unpause` constructor.
    + **In Haskell:** `Unpause (·)`
    + **In Michelson:** `Right (Right (Right (Left (·))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `TransferOwnership`

This entry point is used to transfer ownership to a new owner.

**Parameter:** ***newOwner*** : [`Address`](#types-Address-simplified)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `TransferOwnership` constructor.
    + **In Haskell:** `TransferOwnership (·)`
    + **In Michelson:** `Right (Right (Right (Right (Left (·)))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `AcceptOwnership`

This entry point is used to accept ownership by a new owner.

**Parameter:** [`()`](#types-lparenrparen)

<details>
  <summary><b>How to call this entry point</b></summary>

0. Construct parameter for the entry point.
1. Wrap into `AcceptOwnership` constructor.
    + **In Haskell:** `AcceptOwnership (·)`
    + **In Michelson:** `Right (Right (Right (Right (Right (·)))))`
1. Wrap into `SafeEntrypoints` constructor.
    + **In Haskell:** `Run (·)`
    + **In Michelson:** `Right (Right (Right ((·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration







# Definitions

## Types

<a name="types-lparenrparen"></a>

---

### `()`

Unit primitive.

**Structure:** ()

**Final Michelson representation:** `unit`



<a name="types-lparenacomma-brparen"></a>

---

### `(a, b)`

Pair primitive.

**Final Michelson representation (example):** `(Integer,Natural)` = `pair int nat`



<a name="types-lparenacomma-bcomma-crparen"></a>

---

### `(a, b, c)`

Tuple of size 3.

**Final Michelson representation (example):** `(Integer,Natural,MText)` = `pair int (pair nat string)`



<a name="types-Address-simplified"></a>

---

### `Address simplified`

This is similar to Michelson Address, but does not retain entrypoint name if it refers to a contract.

**Final Michelson representation:** `address`



<a name="types-BigMap"></a>

---

### `BigMap`

BigMap primitive.

**Final Michelson representation (example):** `BigMap Integer Natural` = `big_map int nat`



<a name="types-ByteString"></a>

---

### `ByteString`

Bytes primitive.

**Final Michelson representation:** `bytes`



<a name="types-Code-lparenextended-lambdarparen"></a>

---

### `Code (extended lambda)`

`Code i o` stands for a sequence of instructions which accepts stack of type `i` and returns stack of type `o`.

When both `i` and `o` are of length 1, this primitive corresponds to the Michelson lambda. In more complex cases code is surrounded with `pair`and `unpair` instructions until fits into mentioned restriction.

**Final Michelson representation (example):** `Code [Integer, Natural, MText, ()] [ByteString]` = `lambda (pair (pair (pair int nat) string) unit) bytes`



<a name="types-Contract"></a>

---

### `Contract`

Contract primitive with given type of parameter.

**Final Michelson representation (example):** `ContractRef Integer` = `contract int`



<a name="types-Integer"></a>

---

### `Integer`

Signed number.

**Final Michelson representation:** `int`



<a name="types-List"></a>

---

### `List`

List primitive.

**Final Michelson representation (example):** `[Integer]` = `list int`



<a name="types-MigrationScript"></a>

---

### `MigrationScript`

A code which updates storage in order to make it compliant with the new version of the contract.

**Structure:** ***migrationScript*** :[`Code`](#types-Code-lparenextended-lambdarparen) **[**[`UStore`](#types-Upgradeable-storage)**]** **[**[`UStore`](#types-Upgradeable-storage)**]**

**Final Michelson representation:** `lambda (big_map bytes bytes) (big_map bytes bytes)`



<a name="types-Named-entry"></a>

---

### `Named entry`

Some entries have names for clarity.

In resulting Michelson names may be mapped to annotations.

**Final Michelson representation (example):** `number: Integer` = `int`



<a name="types-Natural"></a>

---

### `Natural`

Unsigned number.

**Final Michelson representation:** `nat`



<a name="types-Operation"></a>

---

### `Operation`

Operation primitive.

**Final Michelson representation:** `operation`



<a name="types-Parameter.SafeParameter"></a>

---

### `Parameter.SafeParameter`

Parameter which does not have unsafe arguments, like raw `Contract p` values.

**Structure:** *one of* 
+ **Run** [`UParam`](#types-Upgradable-parameter)
+ **Upgrade** (***newVersion*** : [`Natural`](#types-Natural), ***migrationScript*** : [`MigrationScript`](#types-MigrationScript), ***newCode*** : [`UContractRouter`](#types-UContractRouter))
+ **EpwBeginUpgrade** [`Natural`](#types-Natural)
+ **EpwApplyMigration** (***migrationscript*** : [`MigrationScript`](#types-MigrationScript))
+ **EpwSetCode** (***contractcode*** : [`UContractRouter`](#types-UContractRouter))
+ **EpwFinishUpgrade** ()
+ **Transfer** (***from*** : [`Address`](#types-Address-simplified), ***to*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))
+ **Approve** (***spender*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))
+ **Mint** (***to*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))
+ **Burn** (***value*** : [`Natural`](#types-Natural))
+ **AddOperator** (***operator*** : [`Address`](#types-Address-simplified))
+ **RemoveOperator** (***operator*** : [`Address`](#types-Address-simplified))
+ **SetRedeemAddress** (***redeem*** : [`Address`](#types-Address-simplified))
+ **Pause** [`()`](#types-lparenrparen)
+ **Unpause** [`()`](#types-lparenrparen)
+ **TransferOwnership** (***newOwner*** : [`Address`](#types-Address-simplified))
+ **AcceptOwnership** [`()`](#types-lparenrparen)


**Final Michelson representation:** `or (or (or (or (pair string bytes) (pair nat (pair (lambda (big_map bytes bytes) (big_map bytes bytes)) (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))))) (or nat (lambda (big_map bytes bytes) (big_map bytes bytes)))) (or (or (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))) unit) (or (pair address (pair address nat)) (pair address nat)))) (or (or (or (pair address nat) nat) (or address address)) (or (or address unit) (or unit (or address unit))))`



<a name="types-Text"></a>

---

### `Text`

Michelson string.
Not every text literal is valid string, see list of constraints in the [Official Michelson documentation](http://tezos.gitlab.io/zeronet/whitedoc/michelson.html#constants).

**Final Michelson representation:** `string`



<a name="types-UContractRouter"></a>

---

### `UContractRouter`

Parameter dispatching logic, main purpose of this code is to pass control to an entrypoint carrying the main logic of the contract.

**Structure:** ***unContractCode*** :[`Code`](#types-Code-lparenextended-lambdarparen) **[**([`UParam`](#types-Upgradable-parameter), [`UStore`](#types-Upgradeable-storage))**]** **[**([`List`](#types-List) [`Operation`](#types-Operation), [`UStore`](#types-Upgradeable-storage))**]**

**Final Michelson representation:** `lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))`



<a name="types-Upgradable-parameter"></a>

---

### `Upgradable parameter`

<UParam description>

**Structure:** ([`Text`](#types-Text), [`ByteString`](#types-ByteString))

**Final Michelson representation:** `pair string bytes`



<a name="types-Upgradeable-storage"></a>

---

### `Upgradeable storage`

Storage with not hardcoded structure, which allows upgrading the contract in place.

**Structure:** ***unUStore*** :[`BigMap`](#types-BigMap) [`ByteString`](#types-ByteString) [`ByteString`](#types-ByteString)

**Final Michelson representation:** `big_map bytes bytes`



<a name="types-View"></a>

---

### `View`

`View a r` accepts an argument of type `a` and callback contract which accepts `r` and returns result via calling that contract.
Read more in [A1 conventions document](https://gitlab.com/tzip/tzip/blob/master/A/A1.md#view-entry-points).

**Structure (example):** `View () Integer` = ([`()`](#types-lparenrparen), [`ContractRef`](#types-Contract) [`Integer`](#types-Integer))

**Final Michelson representation (example):** `View () Integer` = `pair unit (contract int)`



## Errors

Our contract implies the possibility of error scenarios, this section enlists
all values which the contract can produce via calling `FAILWITH` instruction
on them. In case of error, no changes to contract state will be applied.

Each entrypoint also contains a list of errors which can be raised during its
execution; only for no-throw entrypoints this list will be omitted.
Errors in these lists are placed in the order in which the corresponding
properties are checked unless the opposite is specified. I.e., if for a
given entrypoint call two different errors may take place, the one which
appears in the list first will be thrown.

Most of the errors are represented according to the same
`(error name, error argument)` pattern. See the list of errors below
for details.

We distinquish several error classes:
+ **Action exception**: given action cannot be performed with
  regard to the current contract state.

  Examples: "insufficient balance", "wallet does not exist".

  If you are implementing a middleware, such errors should be propagated to
  the client.

+ **Bad argument**: invalid argument supplied to the entrypoint.

  Examples: entrypoint accepts a natural number from `0-3` range, and you
  supply `5`.

  If you are implementing a middleware, you should care about not letting
  such errors happen.

+ **Internal**: contract-internal error.

  In ideal case, such errors should not take place, but still, make sure
  that you are ready to handle them. They can signal either invalid contract
  deployment or a bug in contract implementation.

  If an internal error is thrown, please report it to the author of this contract.


<a name="errors-InternalError"></a>

---

### `InternalError`

**Class:** Internal

**Fires if:** Internal error occured.

**Representation:** Textual error message, see [`Text`](#types-Text).

<a name="errors-SenderIsNotOwner"></a>

---

### `SenderIsNotOwner`

**Class:** Bad argument

**Fires if:** Sender has to be an owner to call this entrypoint

**Representation:** `("SenderIsNotOwner", ())`.

<a name="errors-UpgContractIsMigrating"></a>

---

### `UpgContractIsMigrating`

**Class:** Action exception

**Fires if:** An operation was requested when contract is in a state of migration

**Representation:** `("UpgContractIsMigrating", ())`.

<a name="errors-UpgContractIsNotMigrating"></a>

---

### `UpgContractIsNotMigrating`

**Class:** Action exception

**Fires if:** An migration related operation was requested when contract is not in a state of migration

**Representation:** `("UpgContractIsNotMigrating", ())`.

<a name="errors-UpgVersionMismatch"></a>

---

### `UpgVersionMismatch`

**Class:** Action exception

**Fires if:** The expected version does not match the version of the supplied code.

**Representation:** `("UpgVersionMismatch", <error argument>)`.

Provided error argument will be of type (***expected*** : [`Natural`](#types-Natural), ***actual*** : [`Natural`](#types-Natural)).

