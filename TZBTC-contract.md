<!---
- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
-
- SPDX-License-Identifier: LicenseRef-Proprietary
-->

# TZBTC

**Code revision:** [0f3bfc0](https://github.com/serokell/tezos-btc/commit/0f3bfc04a8492af4e4107a3d21ed4515c21548a0) *(Thu Feb 20 23:05:48 2020 +0300)*

This contract is implemented using Lorentz language.
Basically, this contract is [FA1.2](https://gitlab.com/serokell/morley/tzip/blob/master/A/FA1.2.md)-compatible approvable ledger that maps user addresses to their token balances. The main idea of this token contract is to provide 1-to-1 correspondance with BTC.
There are two special entities for this contract:
* `owner` -- owner of the TZBTC contract, capable in unpausing contract, adding/removing operators, transfering ownership and upgrading contract. There is only one owner of the contract.
* `operator` -- entity which is capable in pausing the contract minting and burning tokens. There may be several operators added by the owner.

## Contract upgradeability

This contract uses upgradeability approach described [here](https://gitlab.com/morley-framework/morley/-/blob/a30dddb633ee880761c3cbf1d4a69ee040ffad25/docs/upgradeableContracts.md#section-2-administrator-forced-upgrades).
This mechanism provides adminstrator-forced address-preserving upgradeability
approach. For more information check out the doc referenced earlier.


## Top-level entry points of upgradeable contract.

These are entry points of the contract.

---

### `getVersion`

This entry point is used to get contract version.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Version`](#types-Version)
  + **In Michelson:** `(pair unit (contract nat))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>





---

### `getAllowance`

Returns the approval value between two given addresses.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) (***owner*** : [`Address`](#types-Address-simplified), ***spender*** : [`Address`](#types-Address-simplified)) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (pair (address :owner) (address :spender)) (contract nat))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `getBalance`

Returns the balance of the address in the ledger.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) (***owner*** : [`Address`](#types-Address-simplified)) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (address :owner) (contract nat))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `getTotalSupply`

Returns total number of tokens.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair unit (contract nat))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `getTotalMinted`

This view returns the total number of minted tokens.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair unit (contract nat))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `getTotalBurned`

This view returns the total number of burned tokens.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair unit (contract nat))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `getOwner`

This view returns the current contract owner.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Address`](#types-Address-simplified)
  + **In Michelson:** `(pair unit (contract address))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `getTokenName`

This view returns the token name.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Text`](#types-Text)
  + **In Michelson:** `(pair unit (contract string))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `getTokenCode`

This view returns the token code.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Text`](#types-Text)
  + **In Michelson:** `(pair unit (contract string))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `getRedeemAddress`

This view returns the redeem address.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Address`](#types-Address-simplified)
  + **In Michelson:** `(pair unit (contract address))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

### `safeEntrypoints`

This entry point is used to call the safe entrypoints of the contract. Entrypoints are 'safe' because they don't have unsafe arguments, such as arguments with type `contract p` so that they can be safely used in operations that add them to the chain (since in babylon values with type `contract p` are prohibited in storage and code constants), in contrast to various Get* entrypoints, which have `contract p` and have to be handled additionally (basically, we have to pass simple `address` instead of `contract p` and call `CONTRACT`, which can fail).

**Argument:** 
  + **In Haskell:** [`Parameter.SafeParameter`](#types-Parameter.SafeParameter)
  + **In Michelson:** `(or (or (or (or (pair string bytes) (pair (pair (nat :currentVersion) (nat :newVersion)) (pair (lambda :migrationScript (big_map bytes bytes) (big_map bytes bytes)) (pair (option :newCode (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))) (option :newPermCode (lambda (pair unit (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))))))) (or (pair (nat :current) (nat :new)) (lambda :migrationscript (big_map bytes bytes) (big_map bytes bytes)))) (or (or (lambda :contractcode (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))) unit) (or (pair (address :from) (pair (address :to) (nat :value))) (pair (address :spender) (nat :value))))) (or (or (or (pair (address :to) (nat :value)) (nat :value)) (or (address :operator) (address :operator))) (or (or (address :redeem) unit) (or unit (or (address :newOwner) unit)))))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



---

#### `run`

This entrypoint extracts contract code kept in storage under the corresponding name and executes it on an argument supplied via `UParam`.

**Argument:** 
  + **In Haskell:** [`UParam`](#types-Upgradable-parameter)
  + **In Michelson:** `(pair string bytes)`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

#### `upgrade`

This entry point is used to update the contract to a new version.
Consider using this entrypoint when your upgrade to the new version isn't very large,
otherwise, transaction with this entrypoint call won't fit instruction size limit.
If this is your case, consider using entrypoint-wise upgrade. This entrypoint
basically exchange `code` field in the storage and upgrade `dataMap` using
provided migration lambda.


**Argument:** 
  + **In Haskell:** (***currentVersion*** : [`Version`](#types-Version), ***newVersion*** : [`Version`](#types-Version), ***migrationScript*** : [`MigrationScript`](#types-MigrationScript), ***newCode*** : [`Maybe`](#types-Maybe) [`UContractRouter`](#types-UContractRouter), ***newPermCode*** : [`Maybe`](#types-Maybe) [`PermanentImpl`](#types-PermanentImpl))
  + **In Michelson:** `(pair (pair (nat :currentVersion) (nat :newVersion)) (pair (lambda :migrationScript (big_map bytes bytes) (big_map bytes bytes)) (pair (option :newCode (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))) (option :newPermCode (lambda (pair unit (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))))))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration

* [`UpgVersionMismatch`](#errors-UpgVersionMismatch) — Current contract version differs from the one passed in the upgrade.



---

#### `epwBeginUpgrade`

This entry point is used to start an entrypoint wise upgrade of the contract.

**Argument:** 
  + **In Haskell:** (***current*** : [`Version`](#types-Version), ***new*** : [`Version`](#types-Version))
  + **In Michelson:** `(pair (nat :current) (nat :new))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration

* [`UpgVersionMismatch`](#errors-UpgVersionMismatch) — Current contract version differs from the one passed in the upgrade.



---

#### `epwApplyMigration`

This entry point is used to apply a storage migration script as part of an upgrade.

**Argument:** 
  + **In Haskell:** ***migrationscript*** : [`MigrationScript`](#types-MigrationScript)
  + **In Michelson:** `(lambda :migrationscript (big_map bytes bytes) (big_map bytes bytes))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



---

#### `epwSetCode`

This entry point is used to set the dispatching code that calls the packed entrypoints.

**Argument:** 
  + **In Haskell:** ***contractcode*** : [`UContractRouter`](#types-UContractRouter)
  + **In Michelson:** `(lambda :contractcode (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



---

#### `epwFinishUpgrade`

This entry point is used to mark that an upgrade has been finsihed.

**Argument:** none (pass unit)

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



---

#### `transfer`

Transfers tokens between two given accounts.

This entrypoint serves multiple purposes:
* When called with `"from"` account equal to the transaction sender, we assume that
the user transfers their own money and this does not require approval.
* Otherwise, the transaction sender must be previously authorized to transfer at least the requested number of tokens from the `"from"` account using the `approve` entrypoint.
In this case current number of tokens that sender is allowed to withdraw from the `"from"` address is decreased by the number of transferred tokens.



**Argument:** 
  + **In Haskell:** (***from*** : [`Address`](#types-Address-simplified), ***to*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :from) (pair (address :to) (nat :value)))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`NotEnoughAllowance`](#errors-NotEnoughAllowance) — Not enough funds allowance to perform the operation.

* [`NotEnoughBalance`](#errors-NotEnoughBalance) — Not enough funds to perform the operation.

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

#### `approve`

When called with `(address :spender, nat :value)`
parameters allows `spender` account to withdraw from the sender, multiple times,
up to the `value` amount.
Each call of `transfer` entrypoint decreases the allowance amount on the transferred amount of tokens unless `transfer` is called with `from` account equal to sender.

If this entrypoint is called again, it overwrites the current allowance
with `value`.

Changing allowance value from non-zero value to a non-zero value is
forbidden to prevent the [corresponding attack vector](https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM).


**Argument:** 
  + **In Haskell:** (***spender*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :spender) (nat :value))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`UnsafeAllowanceChange`](#errors-UnsafeAllowanceChange) — Allowance change from non-zero value to non-zero value is performed.

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

#### `mint`

This entry point is used mint new tokes for an account.

**Argument:** 
  + **In Haskell:** (***to*** : [`Address`](#types-Address-simplified), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :to) (nat :value))`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



The sender has to be the `operator`.

**Possible errors:**
* [`SenderIsNotOperator`](#errors-SenderIsNotOperator) — Sender has to be an operator to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

#### `burn`

Burn some tokens from the `redeem` address.

**Argument:** 
  + **In Haskell:** ***value*** : [`Natural`](#types-Natural)
  + **In Michelson:** `(nat :value)`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



The sender has to be the `operator`.

**Possible errors:**
* [`SenderIsNotOperator`](#errors-SenderIsNotOperator) — Sender has to be an operator to call this entrypoint

* [`NotEnoughBalance`](#errors-NotEnoughBalance) — Not enough funds to perform the operation.

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

#### `addOperator`

This entry point is used to add a new operator.

**Argument:** 
  + **In Haskell:** ***operator*** : [`Address`](#types-Address-simplified)
  + **In Michelson:** `(address :operator)`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



The sender has to be the `owner`.

**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

#### `removeOperator`

This entry point is used to remove an operator.

**Argument:** 
  + **In Haskell:** ***operator*** : [`Address`](#types-Address-simplified)
  + **In Michelson:** `(address :operator)`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



The sender has to be the `owner`.

**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

#### `setRedeemAddress`

This entry point is used to set the redeem address.

**Argument:** 
  + **In Haskell:** ***redeem*** : [`Address`](#types-Address-simplified)
  + **In Michelson:** `(address :redeem)`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



The sender has to be the `owner`.

**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

#### `pause`

This entry point is used to pause the contract.

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



The sender has to be the `operator`.

**Possible errors:**
* [`SenderIsNotOperator`](#errors-SenderIsNotOperator) — Sender has to be an operator to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

#### `unpause`

This entry point is used to resume the contract during a paused state.

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



The sender has to be the `owner`.

**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

#### `transferOwnership`

This entry point is used to transfer ownership to a new owner.

**Argument:** 
  + **In Haskell:** ***newOwner*** : [`Address`](#types-Address-simplified)
  + **In Michelson:** `(address :newOwner)`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



The sender has to be the `owner`.

**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



---

#### `acceptOwnership`

This entry point is used to accept ownership by a new owner.

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Make a transfer to the contract passing this entrypoint's name and the constructed value as an argument.
</details>
<p>



The sender has to be the `new owner`.

**Possible errors:**
* [`NotInTransferOwnershipMode`](#errors-NotInTransferOwnershipMode) — Cannot accept ownership before transfer process has been initiated by calling transferOwnership entrypoint

* [`SenderIsNotNewOwner`](#errors-SenderIsNotNewOwner) — Cannot accept ownership because the sender address is different from the address passed to the transferOwnership entrypoint previously

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration





## Storage

---

### `Storage`

Type which defines storage of the upgradeable contract.
It contains UStore with data related to actual contract logic and fields which relate to upgradeability logic.

**Structure:** (***dataMap*** :[`UStore`](#types-Upgradeable-storage), ***fields*** :[`StorageFields`](#types-StorageFields))

**Final Michelson representation:** `pair (big_map bytes bytes) (pair (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))) (pair nat bool))`







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



<a name="types-lparenacomma-bcomma-ccomma-dcomma-erparen"></a>

---

### `(a, b, c, d, e)`

Tuple of size 5.

**Final Michelson representation (example):** `((),(),(),(),())` = `pair (pair unit unit) (pair unit (pair unit unit))`



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



<a name="types-Bool"></a>

---

### `Bool`

Bool primitive.

**Final Michelson representation:** `bool`



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



<a name="types-Empty"></a>

---

### `Empty`

Type which should never be constructed.

If appears as part of entrypoint argument, this means that the entrypoint should never be called.

**Structure:** [`()`](#types-lparenrparen)

**Final Michelson representation:** `unit`



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



<a name="types-Maybe"></a>

---

### `Maybe`

Option primitive.

**Final Michelson representation (example):** `Maybe Integer` = `option int`



<a name="types-MigrationScript"></a>

---

### `MigrationScript`

A code which updates storage in order to make it compliant with the new version of the contract.

**Structure:** ***unMigrationScript*** :[`Code`](#types-Code-lparenextended-lambdarparen) **[**[`UStore`](#types-Upgradeable-storage)**]** **[**[`UStore`](#types-Upgradeable-storage)**]**

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
+ **Upgrade** (***currentVersion*** : [`Version`](#types-Version), ***newVersion*** : [`Version`](#types-Version), ***migrationScript*** : [`MigrationScript`](#types-MigrationScript), ***newCode*** : [`Maybe`](#types-Maybe) [`UContractRouter`](#types-UContractRouter), ***newPermCode*** : [`Maybe`](#types-Maybe) [`PermanentImpl`](#types-PermanentImpl))
+ **EpwBeginUpgrade** (***current*** : [`Version`](#types-Version), ***new*** : [`Version`](#types-Version))
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


**Final Michelson representation:** `or (or (or (or (pair string bytes) (pair (pair nat nat) (pair (lambda (big_map bytes bytes) (big_map bytes bytes)) (pair (option (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))) (option (lambda (pair unit (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))))))) (or (pair nat nat) (lambda (big_map bytes bytes) (big_map bytes bytes)))) (or (or (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))) unit) (or (pair address (pair address nat)) (pair address nat)))) (or (or (or (pair address nat) nat) (or address address)) (or (or address unit) (or unit (or address unit))))`



<a name="types-PermanentImpl"></a>

---

### `PermanentImpl`

Implementation of permanent entrypoints.

**Structure:** ***unPermanentImpl*** :[`Code`](#types-Code-lparenextended-lambdarparen) **[**[`Empty`](#types-Empty)**,** [`UStore`](#types-Upgradeable-storage)**]** **[**([`List`](#types-List) [`Operation`](#types-Operation), [`UStore`](#types-Upgradeable-storage))**]**

**Final Michelson representation:** `lambda (pair unit (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))`



<a name="types-StorageFields"></a>

---

### `StorageFields`

StorageFields of upgradeable contract.
This type keeps general information about upgradeable contract and the logic responsible for calling entrypoints implementations kept in UStore.

**Structure:** (***contractRouter*** :[`UContractRouter`](#types-UContractRouter), ***currentVersion*** :[`Version`](#types-Version), ***migrating*** :[`Bool`](#types-Bool))

**Final Michelson representation:** `pair (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))) (pair nat bool)`



<a name="types-Text"></a>

---

### `Text`

Michelson string.
Not every text literal is valid string, see list of constraints in the [Official Michelson documentation](https://tezos.gitlab.io/whitedoc/michelson.html#constants).

**Final Michelson representation:** `string`



<a name="types-UContractRouter"></a>

---

### `UContractRouter`

Parameter dispatching logic, main purpose of this code is to pass control to an entrypoint carrying the main logic of the contract.

**Structure:** ***unUContractRouter*** :[`Code`](#types-Code-lparenextended-lambdarparen) **[**([`UParam`](#types-Upgradable-parameter), [`UStore`](#types-Upgradeable-storage))**]** **[**([`List`](#types-List) [`Operation`](#types-Operation), [`UStore`](#types-Upgradeable-storage))**]**

**Final Michelson representation:** `lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))`



<a name="types-Upgradable-parameter"></a>

---

### `Upgradable parameter`

This type encapsulates parameter for one of entry points. It keeps entry point name and corresponding argument serialized.

**Structure:** ([`Text`](#types-Text), [`ByteString`](#types-ByteString))

**Final Michelson representation:** `pair string bytes`



<a name="types-Upgradeable-storage"></a>

---

### `Upgradeable storage`

Storage with not hardcoded structure, which allows upgrading the contract in place. UStore is capable of storing simple fields and multiple submaps. For simple fields key is serialized field name. For submap element big_map key is serialized `(submapName, keyValue)`.

**Structure:** ***unUStore*** :[`BigMap`](#types-BigMap) [`ByteString`](#types-ByteString) [`ByteString`](#types-ByteString)

**Final Michelson representation:** `big_map bytes bytes`



<a name="types-Version"></a>

---

### `Version`

Contract version.

**Structure:** ***version*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `nat`



<a name="types-View"></a>

---

### `View`

`View a r` accepts an argument of type `a` and callback contract which accepts `r` and returns result via calling that contract.
Read more in [A1 conventions document](https://gitlab.com/tzip/tzip/-/blob/c42e3f0f5e73669e84e615d69bee73281572eb0a/proposals/tzip-4/tzip-4.md#view-entrypoints).

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

<a name="errors-NotEnoughAllowance"></a>

---

### `NotEnoughAllowance`

**Class:** Action exception

**Fires if:** Not enough funds allowance to perform the operation.

**Representation:** `("NotEnoughAllowance", <error argument>)`.

Provided error argument will be of type (***required*** : [`Natural`](#types-Natural), ***present*** : [`Natural`](#types-Natural)).

<a name="errors-NotEnoughBalance"></a>

---

### `NotEnoughBalance`

**Class:** Action exception

**Fires if:** Not enough funds to perform the operation.

**Representation:** `("NotEnoughBalance", <error argument>)`.

Provided error argument will be of type (***required*** : [`Natural`](#types-Natural), ***present*** : [`Natural`](#types-Natural)).

<a name="errors-NotInTransferOwnershipMode"></a>

---

### `NotInTransferOwnershipMode`

**Class:** Action exception

**Fires if:** Cannot accept ownership before transfer process has been initiated by calling transferOwnership entrypoint

**Representation:** `("NotInTransferOwnershipMode", ())`.

<a name="errors-SenderIsNotNewOwner"></a>

---

### `SenderIsNotNewOwner`

**Class:** Bad argument

**Fires if:** Cannot accept ownership because the sender address is different from the address passed to the transferOwnership entrypoint previously

**Representation:** `("SenderIsNotNewOwner", ())`.

<a name="errors-SenderIsNotOperator"></a>

---

### `SenderIsNotOperator`

**Class:** -

**Fires if:** Sender has to be an operator to call this entrypoint

**Representation:** `("SenderIsNotOperator", ())`.

<a name="errors-SenderIsNotOwner"></a>

---

### `SenderIsNotOwner`

**Class:** Bad argument

**Fires if:** Sender has to be an owner to call this entrypoint

**Representation:** `("SenderIsNotOwner", ())`.

<a name="errors-TokenOperationsArePaused"></a>

---

### `TokenOperationsArePaused`

**Class:** Action exception

**Fires if:** Token functionality (`transfer` and similar entrypoints) is suspended.

**Representation:** `("TokenOperationsArePaused", ())`.

<a name="errors-UnsafeAllowanceChange"></a>

---

### `UnsafeAllowanceChange`

**Class:** Action exception

**Fires if:** Allowance change from non-zero value to non-zero value is performed. This contract does not allow such an update, see the [corresponding attack vector](https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM) for explanation.

**Representation:** `("UnsafeAllowanceChange", <error argument>)`.

Provided error argument will be of type [`Natural`](#types-Natural) and stand for the previous value of approval.

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

**Fires if:** Current contract version differs from the one passed in the upgrade.

**Representation:** `("UpgVersionMismatch", <error argument>)`.

Provided error argument will be of type (***expectedCurrent*** : [`Version`](#types-Version), ***actualCurrent*** : [`Version`](#types-Version)).

