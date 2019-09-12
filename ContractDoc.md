<!---
- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
-
- SPDX-License-Identifier: LicenseRef-Proprietary
-->

# TZBTC

This contract is implemented using Lorentz language

## Entry-points of TZBTC contract

---

### `Transfer`

Transfers tokens between two given accounts.

This entrypoint serves multiple purposes:
* When called with `"from"` account equal to the transaction sender, we assume that
the user transfers their own money and this does not require approval.
* Otherwise, the transaction sender must be previously authorized to transfer at least the requested number of tokens from the `"from"` account using the `approve` entrypoint.
In this case current number of tokens that sender is allowed to withdraw from the `"from"` address is decreased by the number of transferred tokens.

**Parameter:** (***from*** : [`Address`](#types-Address), ***to*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural))

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `Transfer` constructor.
    + **In Haskell:** `Transfer (·)`
    + **In Michelson:** `Left (Left (Left (Left (·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.



---

### `TransferViaProxy`

Proxy version of Transfer entrypoint.

**Parameter:** (***sender*** : [`Address`](#types-Address), (***from*** : [`Address`](#types-Address), ***to*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural)))

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `TransferViaProxy` constructor.
    + **In Haskell:** `TransferViaProxy (·)`
    + **In Michelson:** `Left (Left (Left (Right (Left (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `proxy`.

**Pausable:** Cannot be executed when token operations are paused.



---

### `Approve`

When called with `(address :spender, nat :value)`
parameters allows `spender` account to withdraw from the sender, multiple times,
up to the `value` amount.
Each call of `transfer` entrypoint decreases the allowance amount on the transferred amount of tokens unless `transfer` is called with `from` account equal to sender.

If this entrypoint is called again, it overwrites the current allowance
with `value`.

Changing allowance value from non-zero value to a non-zero value is
forbidden to prevent the [corresponding attack vector](https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM).

**Parameter:** (***spender*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural))

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `Approve` constructor.
    + **In Haskell:** `Approve (·)`
    + **In Michelson:** `Left (Left (Left (Right (Right (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.



---

### `ApproveViaProxy`

Proxy version of Approve entrypoint.

**Parameter:** (***sender*** : [`Address`](#types-Address), (***spender*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural)))

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `ApproveViaProxy` constructor.
    + **In Haskell:** `ApproveViaProxy (·)`
    + **In Michelson:** `Left (Left (Right (Left (·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `proxy`.

**Pausable:** Cannot be executed when token operations are paused.



---

### `GetAllowance`

Returns the approval value between two given addresses.

**Parameter:** [`View`](#types-View) (***owner*** : [`Address`](#types-Address), ***spender*** : [`Address`](#types-Address)) [`Natural`](#types-Natural)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `GetAllowance` constructor.
    + **In Haskell:** `GetAllowance (·)`
    + **In Michelson:** `Left (Left (Right (Right (Left (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>





---

### `GetBalance`

Returns the balance of the address in the ledger.

**Parameter:** [`View`](#types-View) [`Address`](#types-Address) [`Natural`](#types-Natural)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `GetBalance` constructor.
    + **In Haskell:** `GetBalance (·)`
    + **In Michelson:** `Left (Left (Right (Right (Right (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>





---

### `GetTotalSupply`

**Parameter:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `GetTotalSupply` constructor.
    + **In Haskell:** `GetTotalSupply (·)`
    + **In Michelson:** `Left (Right (Left (Left (·))))`

Pass resulting value as parameter to the contract.

</details>
<p>





---

### `GetTotalMinted`

**Parameter:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `GetTotalMinted` constructor.
    + **In Haskell:** `GetTotalMinted (·)`
    + **In Michelson:** `Left (Right (Left (Right (Left (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>





---

### `GetTotalBurned`

**Parameter:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `GetTotalBurned` constructor.
    + **In Haskell:** `GetTotalBurned (·)`
    + **In Michelson:** `Left (Right (Left (Right (Right (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>





---

### `SetAdministrator`

Change the current administrator.

**Parameter:** [`Address`](#types-Address)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `SetAdministrator` constructor.
    + **In Haskell:** `SetAdministrator (·)`
    + **In Michelson:** `Left (Right (Right (Left (·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Authorization:** The sender has to be `administrator`.



---

### `GetAdministrator`

This view returns the current administrator.

**Parameter:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Address`](#types-Address)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `GetAdministrator` constructor.
    + **In Haskell:** `GetAdministrator (·)`
    + **In Michelson:** `Left (Right (Right (Right (Left (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>





---

### `Mint`

Mint tokens to the given address.

**Parameter:** (***to*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural))

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `Mint` constructor.
    + **In Haskell:** `Mint (·)`
    + **In Michelson:** `Left (Right (Right (Right (Right (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `operator`.



---

### `Burn`

Burn some tokens from the `redeem` address.

**Parameter:** ***value*** : [`Natural`](#types-Natural)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `Burn` constructor.
    + **In Haskell:** `Burn (·)`
    + **In Michelson:** `Right (Left (Left (Left (·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `operator`.



---

### `AddOperator`

Add operator with given address.

**Parameter:** ***operator*** : [`Address`](#types-Address)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `AddOperator` constructor.
    + **In Haskell:** `AddOperator (·)`
    + **In Michelson:** `Right (Left (Left (Right (Left (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `admin`.



---

### `RemoveOperator`

Remove operator with given address.

**Parameter:** ***operator*** : [`Address`](#types-Address)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `RemoveOperator` constructor.
    + **In Haskell:** `RemoveOperator (·)`
    + **In Michelson:** `Right (Left (Left (Right (Right (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `admin`.



---

### `SetRedeemAddress`

Update `redeem` address, from which tokens can be burned.

**Parameter:** ***redeem*** : [`Address`](#types-Address)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `SetRedeemAddress` constructor.
    + **In Haskell:** `SetRedeemAddress (·)`
    + **In Michelson:** `Right (Left (Right (Left (·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `admin`.



---

### `Pause`

Pause the contract, after the pause all end users actions are prohibited.

**Parameter:** [`()`](#types-lparenrparen)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `Pause` constructor.
    + **In Haskell:** `Pause (·)`
    + **In Michelson:** `Right (Left (Right (Right (Left (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `operator`.



---

### `Unpause`

Unpause the contract and resume end users actions.

This entry point pauses operations when the parameter is `True`,
and resumes them when the parameter is `False`. During the pause,
no contract can perform `transfer` or `approval` operations.

**Parameter:** [`()`](#types-lparenrparen)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `Unpause` constructor.
    + **In Haskell:** `Unpause (·)`
    + **In Michelson:** `Right (Left (Right (Right (Right (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>



**Authorization:** The sender has to be `administrator`.



---

### `TransferOwnership`

Start the transfer ownership to a new owner.

**Parameter:** ***newOwner*** : [`Address`](#types-Address)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `TransferOwnership` constructor.
    + **In Haskell:** `TransferOwnership (·)`
    + **In Michelson:** `Right (Right (Left (Left (·))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `admin`.



---

### `AcceptOwnership`

Accept the ownership of the contract.

**Parameter:** [`()`](#types-lparenrparen)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `AcceptOwnership` constructor.
    + **In Haskell:** `AcceptOwnership (·)`
    + **In Michelson:** `Right (Right (Left (Right (Left (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `new owner`.



---

### `StartMigrateTo`

Initialize process of migration from the old contract.

**Parameter:** ***migrationManager*** : [`ContractAddr`](#types-Contract) ([`Address`](#types-Address), [`Natural`](#types-Natural))

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `StartMigrateTo` constructor.
    + **In Haskell:** `StartMigrateTo (·)`
    + **In Michelson:** `Right (Right (Left (Right (Right (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `admin`.



---

### `StartMigrateFrom`

Initialize process of migration from the new contract.

**Parameter:** ***migrationManager*** : [`ContractAddr`](#types-Contract) ([`Address`](#types-Address), [`Natural`](#types-Natural))

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `StartMigrateFrom` constructor.
    + **In Haskell:** `StartMigrateFrom (·)`
    + **In Michelson:** `Right (Right (Right (Left (Left (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>



The sender has to be the `admin`.



---

### `MintForMigration`

Mint tokens to the given address from the agent.

**Parameter:** (***to*** : [`Address`](#types-Address), ***value*** : [`Natural`](#types-Natural))

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `MintForMigration` constructor.
    + **In Haskell:** `MintForMigration (·)`
    + **In Michelson:** `Right (Right (Right (Left (Right (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>





---

### `Migrate`

Migrate from one verstion of the contract to another. Thus tokens in the old version is burned and minted in the new.

**Parameter:** [`()`](#types-lparenrparen)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `Migrate` constructor.
    + **In Haskell:** `Migrate (·)`
    + **In Michelson:** `Right (Right (Right (Right (Left (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>





---

### `SetProxy`

Set address of the proxy contract.

**Parameter:** [`Address`](#types-Address)

<details>
  <summary>**How to call this entry point**</summary>

0. Construct parameter for the entry point.
1. Wrap into `SetProxy` constructor.
    + **In Haskell:** `SetProxy (·)`
    + **In Michelson:** `Right (Right (Right (Right (Right (·)))))`

Pass resulting value as parameter to the contract.

</details>
<p>









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



<a name="types-Address"></a>

---

### `Address`

Address primitive.

**Final Michelson representation:** `address`



<a name="types-Contract"></a>

---

### `Contract`

Contract primitive with given type of parameter.

**Final Michelson representation (example):** `ContractAddr Integer` = `contract int`



<a name="types-Integer"></a>

---

### `Integer`

Signed number.

**Final Michelson representation:** `int`



<a name="types-Natural"></a>

---

### `Natural`

Unsigned number.

**Final Michelson representation:** `nat`



<a name="types-View"></a>

---

### `View`

`View a r` accepts an argument of type `a` and callback contract which accepts `r` and returns result via calling that contract.
Read more in [A1 conventions document](https://gitlab.com/tzip/tzip/blob/master/A/A1.md#view-entry-points).

**Structure (example):** `View () Integer` = ([`()`](#types-lparenrparen), [`ContractAddr`](#types-Contract) [`Integer`](#types-Integer))

**Final Michelson representation (example):** `View () Integer` = `pair unit (contract int)`
