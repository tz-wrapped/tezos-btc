<!---
- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
-
- SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
-->

# TZBTC

**Code revision:** [33a28ea](https://github.com/tz-wrapped/tezos-btc/commit/33a28ea097f44627a3e1cfd424d250313a350928) *(Thu Dec 24 22:47:55 2020 +0300)*



This contract is implemented using Lorentz language.
Basically, this contract is [FA1.2](https://gitlab.com/serokell/morley/tzip/-/blob/be56b0322363259af938e102ac84b0f488695872/Proposals/TZIP-0007/FA1.2.md)-compatible approvable ledger that maps user addresses to their token balances. The main idea of this token contract is to provide 1-to-1 correspondance with BTC.
There are two special entities for this contract:
* `owner` -- owner of the TZBTC contract, capable in unpausing contract, adding/removing operators, transfering ownership and upgrading contract. There is only one owner of the contract.
* `operator` -- entity which is capable in pausing the contract minting and burning tokens. There may be several operators added by the owner.

<a name="section-Table-of-contents"></a>

## Table of contents

- [Haskell ⇄ Michelson conversion](#section-Haskell-c8644-Michelson-conversion)
- [Contract upgradeability](#section-Contract-upgradeability)
- [Storage](#section-Storage)
  - [Upgradeable storage](#storage-Upgradeable-storage)
- [Top-level entrypoints of upgradeable contract](#section-Top-level-entrypoints-of-upgradeable-contract)
  - [getVersion](#entrypoints-getVersion)
  - [getAllowance](#entrypoints-getAllowance)
  - [getBalance](#entrypoints-getBalance)
  - [getTotalSupply](#entrypoints-getTotalSupply)
  - [getTotalMinted](#entrypoints-getTotalMinted)
  - [getTotalBurned](#entrypoints-getTotalBurned)
  - [getOwner](#entrypoints-getOwner)
  - [getRedeemAddress](#entrypoints-getRedeemAddress)
  - [getTokenMetadata](#entrypoints-getTokenMetadata)
  - [safeEntrypoints](#entrypoints-safeEntrypoints)

**[Definitions](#definitions)**

- [Types](#section-Types)
  - [()](#types-lparenrparen)
  - [(a, b)](#types-lparenacomma-brparen)
  - [(a, b, c)](#types-lparenacomma-bcomma-crparen)
  - [(a, b, c, d, e)](#types-lparenacomma-bcomma-ccomma-dcomma-erparen)
  - [Address](#types-Address)
  - [BigMap](#types-BigMap)
  - [Bool](#types-Bool)
  - [ByteString](#types-ByteString)
  - [Code (extended lambda)](#types-Code-lparenextended-lambdarparen)
  - [Contract](#types-Contract)
  - [Empty](#types-Empty)
  - [Integer](#types-Integer)
  - [List](#types-List)
  - [Map](#types-Map)
  - [Maybe](#types-Maybe)
  - [MigrationScript](#types-MigrationScript)
  - [Named entry](#types-Named-entry)
  - [Natural](#types-Natural)
  - [Operation](#types-Operation)
  - [Parameter.SafeParameter](#types-Parameter.SafeParameter)
  - [PermanentImpl](#types-PermanentImpl)
  - [Set](#types-Set)
  - [Text](#types-Text)
  - [TokenId](#types-TokenId)
  - [TokenMetadata](#types-TokenMetadata)
  - [UContractRouter](#types-UContractRouter)
  - [Upgradable parameter](#types-Upgradable-parameter)
  - [Upgradeable storage](#types-Upgradeable-storage)
  - [Version](#types-Version)
  - [View](#types-View)
- [Errors](#section-Errors)
  - [InternalError](#errors-InternalError)
  - [InvalidSingleTokenId](#errors-InvalidSingleTokenId)
  - [NotEnoughAllowance](#errors-NotEnoughAllowance)
  - [NotEnoughBalance](#errors-NotEnoughBalance)
  - [NotInTransferOwnershipMode](#errors-NotInTransferOwnershipMode)
  - [SenderIsNotNewOwner](#errors-SenderIsNotNewOwner)
  - [SenderIsNotOperator](#errors-SenderIsNotOperator)
  - [SenderIsNotOwner](#errors-SenderIsNotOwner)
  - [TokenOperationsArePaused](#errors-TokenOperationsArePaused)
  - [UnsafeAllowanceChange](#errors-UnsafeAllowanceChange)
  - [UpgContractIsMigrating](#errors-UpgContractIsMigrating)
  - [UpgContractIsNotMigrating](#errors-UpgContractIsNotMigrating)
  - [UpgVersionMismatch](#errors-UpgVersionMismatch)
- [Used upgradeable storage formats](#section-Used-upgradeable-storage-formats)



<a name="section-Haskell-c8644-Michelson-conversion"></a>

## Haskell ⇄ Michelson conversion

This smart contract is developed in Haskell using the [Morley framework](https://gitlab.com/morley-framework/morley). Documentation mentions Haskell types that can be used for interaction with this contract from Haskell, but for each Haskell type we also mention its Michelson representation to make interactions outside of Haskell possible.

There are multiple ways to interact with this contract:

* Use this contract in your Haskell application, thus all operation submissions should be handled separately, e.g. via calling `tezos-client`, which will communicate with the `tezos-node`. In order to be able to call `tezos-client` you'll need to be able to construct Michelson values from Haskell.

  The easiest way to do that is to serialize Haskell value using `lPackValue` function from [`Lorentz.Pack`](https://gitlab.com/morley-framework/morley/-/blob/2441e26bebd22ac4b30948e8facbb698d3b25c6d/code/lorentz/src/Lorentz/Pack.hs) module, encode resulting bytestring to hexadecimal representation using `encodeHex` function. Resulting hexadecimal encoded bytes sequence can be decoded back to Michelson value via `tezos-client unpack michelson data`.

  Reverse conversion from Michelson value to the Haskell value can be done by serializing Michelson value using `tezos-client hash data` command, resulting `Raw packed data` should be decoded from the hexadecimal representation using `decodeHex` and deserialized to the Haskell value via `lUnpackValue` function from [`Lorentz.Pack`](https://gitlab.com/morley-framework/morley/-/blob/2441e26bebd22ac4b30948e8facbb698d3b25c6d/code/lorentz/src/Lorentz/Pack.hs).

* Construct values for this contract directly on Michelson level using types provided in the documentation.

<a name="section-Contract-upgradeability"></a>

## Contract upgradeability

This contract uses upgradeability approach described [here](https://gitlab.com/morley-framework/morley/-/blob/a30dddb633ee880761c3cbf1d4a69ee040ffad25/docs/upgradeableContracts.md#section-2-administrator-forced-upgrades).
This mechanism provides adminstrator-forced address-preserving upgradeability
approach. For more information check out the doc referenced earlier.

Initially originated contract has V0 which should be empty. However, it's possible to originate contract with some entrypoints implementation, but such origination will highly likely exceed operation size limit, so it's recomended to originate empty V0 contract.

 Once the V0 is originated, it should be upgraded to V1 in order to be usable.

 The easiest way to originate and upgrade contract to V1 is to use `tzbtc-client deployTzbtcContract` command.

<a name="section-Storage"></a>

## Storage

<a name="storage-Upgradeable-storage"></a>

---

### `Upgradeable storage`

Storage with not hardcoded structure, which allows upgrading the contract
in place. UStore is capable of storing simple fields and multiple submaps.


**Structure:** 
[`BigMap`](#types-BigMap) [`ByteString`](#types-ByteString) [`ByteString`](#types-ByteString)

**Final Michelson representation:** `big_map bytes bytes`



<a name="section-Top-level-entrypoints-of-upgradeable-contract"></a>

## Top-level entrypoints of upgradeable contract

These entrypoints may change in new versions of the contract.

Also they have a special calling routing, see the respective subsection in every entrypoint description.

<a name="entrypoints-getVersion"></a>

---

### `getVersion`

This entry point is used to get contract version.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Version`](#types-Version)
  + **In Michelson:** `(pair (unit %viewParam) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair Unit "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getVersion` entrypoint passing the constructed argument.
</details>
<p>





<a name="entrypoints-getAllowance"></a>

---

### `getAllowance`

Returns the approval value between two given addresses.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) (***owner*** : [`Address`](#types-Address), ***spender*** : [`Address`](#types-Address)) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (pair %viewParam (address :owner) (address :spender)) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB") "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getAllowance` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-getBalance"></a>

---

### `getBalance`

Returns the balance of the address in the ledger.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) (***owner*** : [`Address`](#types-Address)) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (address :owner %viewParam) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getBalance` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-getTotalSupply"></a>

---

### `getTotalSupply`

Returns total number of tokens.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (unit %viewParam) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair Unit "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getTotalSupply` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-getTotalMinted"></a>

---

### `getTotalMinted`

This view returns the total number of minted tokens.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (unit %viewParam) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair Unit "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getTotalMinted` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-getTotalBurned"></a>

---

### `getTotalBurned`

This view returns the total number of burned tokens.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (unit %viewParam) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair Unit "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getTotalBurned` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-getOwner"></a>

---

### `getOwner`

This view returns the current contract owner.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Address`](#types-Address)
  + **In Michelson:** `(pair (unit %viewParam) (contract %viewCallbackTo address))`
    + **Example:** <span id="example-id">`Pair Unit "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getOwner` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-getRedeemAddress"></a>

---

### `getRedeemAddress`

This view returns the redeem address.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Address`](#types-Address)
  + **In Michelson:** `(pair (unit %viewParam) (contract %viewCallbackTo address))`
    + **Example:** <span id="example-id">`Pair Unit "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getRedeemAddress` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-getTokenMetadata"></a>

---

### `getTokenMetadata`

This view returns the token metadata.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) ([`List`](#types-List) [`TokenId`](#types-TokenId)) ([`List`](#types-List) [`TokenMetadata`](#types-TokenMetadata))
  + **In Michelson:** `(pair (list %viewParam nat) (contract %viewCallbackTo (list (pair (nat %token_id) (pair (string %symbol) (pair (string %name) (pair (nat %decimals) (map %extras string string))))))))`
    + **Example:** <span id="example-id">`Pair { 0 } "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getTokenMetadata` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`InvalidSingleTokenId`](#errors-InvalidSingleTokenId) — The only valid token id is 0

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-safeEntrypoints"></a>

---

### `safeEntrypoints`

This entry point is used to call the safe entrypoints of the contract. Entrypoints are 'safe' because they don't have unsafe arguments, such as arguments with type `contract p` so that they can be safely used in operations that add them to the chain (since in babylon values with type `contract p` are prohibited in storage and code constants), in contrast to various Get* entrypoints, which have `contract p` and have to be handled additionally (basically, we have to pass simple `address` instead of `contract p` and call `CONTRACT`, which can fail).

**Argument:** 
  + **In Haskell:** [`Parameter.SafeParameter`](#types-Parameter.SafeParameter)
  + **In Michelson:** `(or (or (or (or (pair string bytes) (pair (pair (nat :currentVersion) (nat :newVersion)) (pair (lambda :migrationScript (big_map bytes bytes) (big_map bytes bytes)) (pair (option :newCode (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))) (option :newPermCode (lambda (pair unit (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))))))) (or (pair (nat :current) (nat :new)) (lambda :migrationscript (big_map bytes bytes) (big_map bytes bytes)))) (or (or (lambda :contractcode (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))) unit) (or (pair (address :from) (pair (address :to) (nat :value))) (pair (address :spender) (nat :value))))) (or (or (or (pair (address :to) (nat :value)) (nat :value)) (or (address :operator) (address :operator))) (or (or (address :redeem) unit) (or unit (or (address :newOwner) unit)))))`
    + **Example:** <span id="example-id">`Left (Left (Left (Left (Pair "hello" 0x0a))))`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `safeEntrypoints` entrypoint passing the constructed argument.
</details>
<p>



<a name="section-Entrypoints-that-will-remain-intact-in-all-versions-of-the-contract."></a>

#### Entrypoints that will remain intact in all versions of the contract.

<a name="entrypoints-run"></a>

---

##### `run`

This entrypoint extracts contract code kept in storage under the corresponding name and executes it on an argument supplied via `UParam`.

**Argument:** 
  + **In Haskell:** [`UParam`](#types-Upgradable-parameter)
  + **In Michelson:** `(pair string bytes)`
    + **Example:** <span id="example-id">`Pair "hello" 0x0a`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `run` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-upgrade"></a>

---

##### `upgrade`

This entry point is used to update the contract to a new version.
Consider using this entrypoint when your upgrade to the new version isn't very large,
otherwise, transaction with this entrypoint call won't fit instruction size limit.
If this is your case, consider using entrypoint-wise upgrade. This entrypoint
basically exchange `code` field in the storage and upgrade `dataMap` using
provided migration lambda.


**Argument:** 
  + **In Haskell:** (***currentVersion*** : [`Version`](#types-Version), ***newVersion*** : [`Version`](#types-Version), ***migrationScript*** : [`MigrationScript`](#types-MigrationScript) [`Store template V0`](#ustore-template-Store-template-V0) [`Some`](#ustore-template-Some), ***newCode*** : [`Maybe`](#types-Maybe) [`UContractRouter`](#types-UContractRouter), ***newPermCode*** : [`Maybe`](#types-Maybe) [`PermanentImpl`](#types-PermanentImpl))
  + **In Michelson:** `(pair (pair (nat :currentVersion) (nat :newVersion)) (pair (lambda :migrationScript (big_map bytes bytes) (big_map bytes bytes)) (pair (option :newCode (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))) (option :newPermCode (lambda (pair unit (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))))))`
    + **Example:** <span id="example-id">`Pair (Pair 0 0) (Pair { PUSH string "lambda sample"; FAILWITH } (Pair (Some { PUSH string "lambda sample"; FAILWITH }) (Some { PUSH string "lambda sample"; FAILWITH })))`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `upgrade` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration

* [`UpgVersionMismatch`](#errors-UpgVersionMismatch) — Current contract version differs from the one passed in the upgrade.



<a name="entrypoints-epwBeginUpgrade"></a>

---

##### `epwBeginUpgrade`

This entry point is used to start an entrypoint wise upgrade of the contract.

**Argument:** 
  + **In Haskell:** (***current*** : [`Version`](#types-Version), ***new*** : [`Version`](#types-Version))
  + **In Michelson:** `(pair (nat :current) (nat :new))`
    + **Example:** <span id="example-id">`Pair 0 0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `epwBeginUpgrade` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration

* [`UpgVersionMismatch`](#errors-UpgVersionMismatch) — Current contract version differs from the one passed in the upgrade.



<a name="entrypoints-epwApplyMigration"></a>

---

##### `epwApplyMigration`

This entry point is used to apply a storage migration script as part of an upgrade.

**Argument:** 
  + **In Haskell:** ***migrationscript*** : [`MigrationScript`](#types-MigrationScript) [`Store template V0`](#ustore-template-Store-template-V0) [`Some`](#ustore-template-Some)
  + **In Michelson:** `(lambda :migrationscript (big_map bytes bytes) (big_map bytes bytes))`
    + **Example:** <span id="example-id">`{ PUSH string "lambda sample"; FAILWITH }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `epwApplyMigration` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



<a name="entrypoints-epwSetCode"></a>

---

##### `epwSetCode`

This entry point is used to set the dispatching code that calls the packed entrypoints.

**Argument:** 
  + **In Haskell:** ***contractcode*** : [`UContractRouter`](#types-UContractRouter)
  + **In Michelson:** `(lambda :contractcode (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))`
    + **Example:** <span id="example-id">`{ PUSH string "lambda sample"; FAILWITH }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `epwSetCode` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



<a name="entrypoints-epwFinishUpgrade"></a>

---

##### `epwFinishUpgrade`

This entry point is used to mark that an upgrade has been finsihed.

**Argument:** none (pass unit)

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `epwFinishUpgrade` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsNotMigrating`](#errors-UpgContractIsNotMigrating) — An migration related operation was requested when contract is not in a state of migration



<a name="entrypoints-transfer"></a>

---

##### `transfer`

Transfers tokens between two given accounts.

This entrypoint serves multiple purposes:
* When called with `"from"` account equal to the transaction sender, we assume that
the user transfers their own money and this does not require approval.
* Otherwise, the transaction sender must be previously authorized to transfer at least the requested number of tokens from the `"from"` account using the `approve` entrypoint.
In this case current number of tokens that sender is allowed to withdraw from the `"from"` address is decreased by the number of transferred tokens.



**Argument:** 
  + **In Haskell:** (***from*** : [`Address`](#types-Address), ***to*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :from) (pair (address :to) (nat :value)))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0)`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `transfer` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`NotEnoughAllowance`](#errors-NotEnoughAllowance) — Not enough funds allowance to perform the operation.

* [`NotEnoughBalance`](#errors-NotEnoughBalance) — Not enough funds to perform the operation.

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-approve"></a>

---

##### `approve`

When called with `(address :spender, nat :value)`
parameters allows `spender` account to withdraw from the sender, multiple times,
up to the `value` amount.
Each call of `transfer` entrypoint decreases the allowance amount on the transferred amount of
tokens, unless `transfer` is called with `from` account equal to sender, in which case allowance
is always ignored.
In other terms self-approval, where 'from` is equal to sender, is redundant and will never be consumed by a 'transfer'.

If this entrypoint is called again, it overwrites the current allowance with `value`.

**DISCLAIMER**: this suffers from an [attack vector](https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM),
that is a known issue of `ERC20` and, as a consequence, of `FA1.2` (which is
based on it).
It is not safe to change the approval from a non-zero value to a non-zero value.
This is the reason why performing such a change directly is not allowed by the contract.
However this is not enough on its own, a token holder that intends to
safely change the allowance for `X` to `K` token must:
1. read the current allowance `M` for `X` from the latest transaction `S`.
2. send a transaction `T` that sets the allowance to `0`.
3. wait for the blockchain to confirm that `T` is included.
4. scan all transactions between `S` and `T`.
5. calculate the allowance `N <= M` spent by `X` in those transactions.
6. set the allowance to `K - N` iff `N < K`.


**Argument:** 
  + **In Haskell:** (***spender*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :spender) (nat :value))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `approve` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`UnsafeAllowanceChange`](#errors-UnsafeAllowanceChange) — Allowance change from non-zero value to non-zero value is performed.

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-mint"></a>

---

##### `mint`

This entry point is used mint new tokes for an account.

**Argument:** 
  + **In Haskell:** (***to*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :to) (nat :value))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `mint` entrypoint passing the constructed argument.
</details>
<p>



The sender has to be the `operator`.

**Possible errors:**
* [`SenderIsNotOperator`](#errors-SenderIsNotOperator) — Sender has to be an operator to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-burn"></a>

---

##### `burn`

Burn some tokens from the `redeem` address.

**Argument:** 
  + **In Haskell:** ***value*** : [`Natural`](#types-Natural)
  + **In Michelson:** `(nat :value)`
    + **Example:** <span id="example-id">`0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `burn` entrypoint passing the constructed argument.
</details>
<p>



The sender has to be the `operator`.

**Possible errors:**
* [`SenderIsNotOperator`](#errors-SenderIsNotOperator) — Sender has to be an operator to call this entrypoint

* [`NotEnoughBalance`](#errors-NotEnoughBalance) — Not enough funds to perform the operation.

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-addOperator"></a>

---

##### `addOperator`

This entry point is used to add a new operator.

**Argument:** 
  + **In Haskell:** ***operator*** : [`Address`](#types-Address)
  + **In Michelson:** `(address :operator)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `addOperator` entrypoint passing the constructed argument.
</details>
<p>



The sender has to be the `owner`.

**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-removeOperator"></a>

---

##### `removeOperator`

This entry point is used to remove an operator.

**Argument:** 
  + **In Haskell:** ***operator*** : [`Address`](#types-Address)
  + **In Michelson:** `(address :operator)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `removeOperator` entrypoint passing the constructed argument.
</details>
<p>



The sender has to be the `owner`.

**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-setRedeemAddress"></a>

---

##### `setRedeemAddress`

This entry point is used to set the redeem address.

**Argument:** 
  + **In Haskell:** ***redeem*** : [`Address`](#types-Address)
  + **In Michelson:** `(address :redeem)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `setRedeemAddress` entrypoint passing the constructed argument.
</details>
<p>



The sender has to be the `owner`.

**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-pause"></a>

---

##### `pause`

This entry point is used to pause the contract.

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `pause` entrypoint passing the constructed argument.
</details>
<p>



The sender has to be the `operator`.

**Possible errors:**
* [`SenderIsNotOperator`](#errors-SenderIsNotOperator) — Sender has to be an operator to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-unpause"></a>

---

##### `unpause`

This entry point is used to resume the contract during a paused state.

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `unpause` entrypoint passing the constructed argument.
</details>
<p>



The sender has to be the `owner`.

**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-transferOwnership"></a>

---

##### `transferOwnership`

This entry point is used to transfer ownership to a new owner.

**Argument:** 
  + **In Haskell:** ***newOwner*** : [`Address`](#types-Address)
  + **In Michelson:** `(address :newOwner)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `transferOwnership` entrypoint passing the constructed argument.
</details>
<p>



The sender has to be the `owner`.

**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender has to be an owner to call this entrypoint

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration



<a name="entrypoints-acceptOwnership"></a>

---

##### `acceptOwnership`

This entry point is used to accept ownership by a new owner.

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `acceptOwnership` entrypoint passing the constructed argument.
</details>
<p>



The sender has to be the `new owner`.

**Possible errors:**
* [`NotInTransferOwnershipMode`](#errors-NotInTransferOwnershipMode) — Cannot accept ownership before transfer process has been initiated by calling transferOwnership entrypoint

* [`SenderIsNotNewOwner`](#errors-SenderIsNotNewOwner) — Cannot accept ownership because the sender address is different from the address passed to the transferOwnership entrypoint previously

* [`UpgContractIsMigrating`](#errors-UpgContractIsMigrating) — An operation was requested when contract is in a state of migration









# Definitions

<a name="section-Types"></a>

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



<a name="types-Address"></a>

---

### `Address`

Address primitive.

Unlike Michelson's `address`, it is assumed not to contain an entrypoint name,
even if it refers to a contract; this won't be checked, so passing an entrypoint
name may result in unexpected errors.


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

**Final Michelson representation (example):** `Code [Integer, Natural, MText, ()] [ByteString]` = `lambda (pair int (pair nat (pair string unit))) bytes`



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

**Structure:** 
[`()`](#types-lparenrparen)

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



<a name="types-Map"></a>

---

### `Map`

Map primitive.

**Final Michelson representation (example):** `Map Integer Natural` = `map int nat`



<a name="types-Maybe"></a>

---

### `Maybe`

Option primitive.

**Final Michelson representation (example):** `Maybe Integer` = `option int`



<a name="types-MigrationScript"></a>

---

### `MigrationScript`

A code which updates storage in order to make it compliant with the new version of the contract.
It is common to have a group of migration scripts because each of it is to be used in Tezos transaction and thus should fit into gas and operation size limits.

**Structure:** 
[`Code`](#types-Code-lparenextended-lambdarparen) **[**[`UStore`](#types-Upgradeable-storage) [`Some`](#ustore-template-Some)**]** **[**[`UStore`](#types-Upgradeable-storage) [`Some`](#ustore-template-Some)**]**

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
+ **Run**
[`UParam`](#types-Upgradable-parameter)
+ **Upgrade**
(***currentVersion*** : [`Version`](#types-Version), ***newVersion*** : [`Version`](#types-Version), ***migrationScript*** : [`MigrationScript`](#types-MigrationScript) [`Store template V0`](#ustore-template-Store-template-V0) [`Some`](#ustore-template-Some), ***newCode*** : [`Maybe`](#types-Maybe) [`UContractRouter`](#types-UContractRouter), ***newPermCode*** : [`Maybe`](#types-Maybe) [`PermanentImpl`](#types-PermanentImpl))
+ **EpwBeginUpgrade**
(***current*** : [`Version`](#types-Version), ***new*** : [`Version`](#types-Version))
+ **EpwApplyMigration**
(***migrationscript*** : [`MigrationScript`](#types-MigrationScript) [`Store template V0`](#ustore-template-Store-template-V0) [`Some`](#ustore-template-Some))
+ **EpwSetCode**
(***contractcode*** : [`UContractRouter`](#types-UContractRouter))
+ **EpwFinishUpgrade**()
+ **Transfer**
(***from*** : [`Address`](#types-Address), ***to*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural))
+ **Approve**
(***spender*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural))
+ **Mint**
(***to*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural))
+ **Burn**
(***value*** : [`Natural`](#types-Natural))
+ **AddOperator**
(***operator*** : [`Address`](#types-Address))
+ **RemoveOperator**
(***operator*** : [`Address`](#types-Address))
+ **SetRedeemAddress**
(***redeem*** : [`Address`](#types-Address))
+ **Pause**
[`()`](#types-lparenrparen)
+ **Unpause**
[`()`](#types-lparenrparen)
+ **TransferOwnership**
(***newOwner*** : [`Address`](#types-Address))
+ **AcceptOwnership**
[`()`](#types-lparenrparen)


**Final Michelson representation:** `or (or (or (or (pair string bytes) (pair (pair nat nat) (pair (lambda (big_map bytes bytes) (big_map bytes bytes)) (pair (option (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))) (option (lambda (pair unit (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes)))))))) (or (pair nat nat) (lambda (big_map bytes bytes) (big_map bytes bytes)))) (or (or (lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))) unit) (or (pair address (pair address nat)) (pair address nat)))) (or (or (or (pair address nat) nat) (or address address)) (or (or address unit) (or unit (or address unit))))`



<a name="types-PermanentImpl"></a>

---

### `PermanentImpl`

Implementation of permanent entrypoints.

**Structure:** 
[`Code`](#types-Code-lparenextended-lambdarparen) **[**[`Empty`](#types-Empty)**,** [`UStore`](#types-Upgradeable-storage) [`Some`](#ustore-template-Some)**]** **[**([`List`](#types-List) [`Operation`](#types-Operation), [`UStore`](#types-Upgradeable-storage) [`Some`](#ustore-template-Some))**]**

**Final Michelson representation:** `lambda (pair unit (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))`



<a name="types-Set"></a>

---

### `Set`

Set primitive.

**Final Michelson representation (example):** `Set Integer` = `set int`



<a name="types-Text"></a>

---

### `Text`

Michelson string.

This has to contain only ASCII characters with codes from [32; 126] range; additionally, newline feed character is allowed.

**Final Michelson representation:** `string`



<a name="types-TokenId"></a>

---

### `TokenId`

Token identifier as defined by [TZIP-12](https://gitlab.com/tzip/tzip/-/blob/eb1da57684599a266334a73babd7ba82dbbbce66/proposals/tzip-12/tzip-12.md#general).

**Structure:** 
[`Natural`](#types-Natural)

**Final Michelson representation:** `nat`



<a name="types-TokenMetadata"></a>

---

### `TokenMetadata`

Contract's storage holding a big_map with all balances and the operators.

**Structure:** 
  * ***tmTokenId*** :[`TokenId`](#types-TokenId)
  * ***tmSymbol*** :[`Text`](#types-Text)
  * ***tmName*** :[`Text`](#types-Text)
  * ***tmDecimals*** :[`Natural`](#types-Natural)
  * ***tmExtras*** :[`Map`](#types-Map) [`Text`](#types-Text) [`Text`](#types-Text)

**Final Michelson representation:** `pair nat (pair string (pair string (pair nat (map string string))))`



<a name="types-UContractRouter"></a>

---

### `UContractRouter`

Parameter dispatching logic, main purpose of this code is to pass control to an entrypoint carrying the main logic of the contract.

**Structure:** 
[`Code`](#types-Code-lparenextended-lambdarparen) **[**([`UParam`](#types-Upgradable-parameter), [`UStore`](#types-Upgradeable-storage) [`Some`](#ustore-template-Some))**]** **[**([`List`](#types-List) [`Operation`](#types-Operation), [`UStore`](#types-Upgradeable-storage) [`Some`](#ustore-template-Some))**]**

**Final Michelson representation:** `lambda (pair (pair string bytes) (big_map bytes bytes)) (pair (list operation) (big_map bytes bytes))`



<a name="types-Upgradable-parameter"></a>

---

### `Upgradable parameter`

This type encapsulates parameter for one of entry points. It keeps entry point name and corresponding argument serialized.

**Structure:** 
([`Text`](#types-Text), [`ByteString`](#types-ByteString))

**Final Michelson representation:** `pair string bytes`



<a name="types-Upgradeable-storage"></a>

---

### `Upgradeable storage`

Storage with not hardcoded structure, which allows upgrading the contract
in place. UStore is capable of storing simple fields and multiple submaps.


**Structure:** 
[`BigMap`](#types-BigMap) [`ByteString`](#types-ByteString) [`ByteString`](#types-ByteString)

**Final Michelson representation:** `big_map bytes bytes`



<a name="types-Version"></a>

---

### `Version`

Contract version.

**Structure:** 
[`Natural`](#types-Natural)

**Final Michelson representation:** `nat`



<a name="types-View"></a>

---

### `View`

`View a r` accepts an argument of type `a` and callback contract which accepts `r` and returns result via calling that contract.
Read more in [A1 conventions document](https://gitlab.com/tzip/tzip/-/blob/c42e3f0f5e73669e84e615d69bee73281572eb0a/proposals/tzip-4/tzip-4.md#view-entrypoints).

**Structure (example):** `View () Integer` = 
[`()`](#types-lparenrparen)
[`ContractRef`](#types-Contract) [`Integer`](#types-Integer)

**Final Michelson representation (example):** `View () Integer` = `pair unit (contract int)`



<a name="section-Errors"></a>

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
`(error tag, error argument)` pattern. See the list of errors below
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

**Fires if:** Some internal error occured.

**Representation:** Textual error message, see [`Text`](#types-Text).

<a name="errors-InvalidSingleTokenId"></a>

---

### `InvalidSingleTokenId`

**Class:** -

**Fires if:** The only valid token id is 0

**Representation:** `("InvalidSingleTokenId", ())`.

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

<a name="section-Used-upgradeable-storage-formats"></a>

## Used upgradeable storage formats

This section describes formats (aka _templates_) of upgradeable storages mentioned across the given document. Each format describes set of fields and virtual submaps which the storage must have.

<a name="ustore-template-Some"></a>

---

### `Some`

This is a dummy template, usually designates that any format can be used here.

**Contents:** *unspecified*



<a name="ustore-template-Store-template-V0"></a>

---

### `Store template V0`

Contains entries for V0 contract storage.
This includes administrator address which is necessary for a secure upgrade to V1.


**Contents:** 
* **Field** `owner`: [`Address`](#types-Address)
<details>
  <summary><b>Encoding</b></summary>

  + `key = pack ("owner")`

  + `value = pack (<field value>)`

---


</details>
<p>





<a name="ustore-template-Store-template-V1"></a>

---

### `Store template V1`

Contains all the storage entries for V1 contract.


**Contents:** 
* **Field** `owner`: [`Address`](#types-Address)
<details>
  <summary><b>Encoding</b></summary>

  + `key = pack ("owner")`

  + `value = pack (<field value>)`

---


</details>
<p>

* **Field** `paused`: [`Bool`](#types-Bool)
<details>
  <summary><b>Encoding</b></summary>

  + `key = pack ("paused")`

  + `value = pack (<field value>)`

---


</details>
<p>

* **Field** `totalSupply`: [`Natural`](#types-Natural)
<details>
  <summary><b>Encoding</b></summary>

  + `key = pack ("totalSupply")`

  + `value = pack (<field value>)`

---


</details>
<p>

* **Field** `totalBurned`: [`Natural`](#types-Natural)
<details>
  <summary><b>Encoding</b></summary>

  + `key = pack ("totalBurned")`

  + `value = pack (<field value>)`

---


</details>
<p>

* **Field** `totalMinted`: [`Natural`](#types-Natural)
<details>
  <summary><b>Encoding</b></summary>

  + `key = pack ("totalMinted")`

  + `value = pack (<field value>)`

---


</details>
<p>

* **Field** `newOwner`: [`Maybe`](#types-Maybe) [`Address`](#types-Address)
<details>
  <summary><b>Encoding</b></summary>

  + `key = pack ("newOwner")`

  + `value = pack (<field value>)`

---


</details>
<p>

* **Field** `operators`: [`Set`](#types-Set) [`Address`](#types-Address)
<details>
  <summary><b>Encoding</b></summary>

  + `key = pack ("operators")`

  + `value = pack (<field value>)`

---


</details>
<p>

* **Field** `redeemAddress`: [`Address`](#types-Address)
<details>
  <summary><b>Encoding</b></summary>

  + `key = pack ("redeemAddress")`

  + `value = pack (<field value>)`

---


</details>
<p>

* **Field** `tokenMetadata`: [`TokenMetadata`](#types-TokenMetadata)
<details>
  <summary><b>Encoding</b></summary>

  + `key = pack ("tokenMetadata")`

  + `value = pack (<field value>)`

---


</details>
<p>

* **Submap** `code`: [`Text`](#types-Text) -> [`Code`](#types-Code-lparenextended-lambdarparen) **[**([`ByteString`](#types-ByteString), [`UStore`](#types-Upgradeable-storage) [`Store template V1`](#ustore-template-Store-template-V1))**]** **[**([`List`](#types-List) [`Operation`](#types-Operation), [`UStore`](#types-Upgradeable-storage) [`Store template V1`](#ustore-template-Store-template-V1))**]**
<details>
  <summary><b>Encoding</b></summary>

 + `key = pack (<submap key>)`

 + `value = pack (<submap value>)`

---


</details>
<p>

* **Field** `fallback`: [`Code`](#types-Code-lparenextended-lambdarparen) **[**(([`Text`](#types-Text), [`ByteString`](#types-ByteString)), [`UStore`](#types-Upgradeable-storage) [`Store template V1`](#ustore-template-Store-template-V1))**]** **[**([`List`](#types-List) [`Operation`](#types-Operation), [`UStore`](#types-Upgradeable-storage) [`Store template V1`](#ustore-template-Store-template-V1))**]**
<details>
  <summary><b>Encoding</b></summary>

  + `key = pack ("fallback")`

  + `value = pack (<field value>)`

---


</details>
<p>

* **Submap** `ledger`: [`Address`](#types-Address) -> (***balance*** : [`Natural`](#types-Natural), ***approvals*** : [`Map`](#types-Map) [`Address`](#types-Address) [`Natural`](#types-Natural))
<details>
  <summary><b>Encoding</b></summary>

 + `key = pack (<submap key>)`

 + `value = pack (<submap value>)`

---


</details>
<p>

