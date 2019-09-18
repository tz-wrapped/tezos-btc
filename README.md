<!--
 - SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -->

# ZBTC

Wrapped Bitcoin on Tezos Blockchain called ZBTC


## Build Instructions [↑](#ZBTC)

`stack build`

## Usage [↑](#ZBTC)

Run `stack test` and explore the tests.


### `tzbtc` executable

You can use `tzbtc` executable in order to get raw Michelson
parameters, that can be submitted to the chain via `tezos-client`.
Also, you can get contract code, initial contracts storage. This
stuff can be used for contract origination via `tezos-client`.
`parseContractParameter` subcommand can be used for debugging,
it parses raw Michelson value to the TZBTC contract parameter.

Use `tzbtc --help` to get a list of available commands.

### `tzbtc-client` executable

Also you can use `tzbtc-client` executable. Unlike the `tzbtc`
this executable performs transactions to the chain using remote
tezos-node. In order to setup `tzbtc-client` you should use
`tzbtc-client setupClient` command. Most of the commands (except
`printContract`, `printInitialStorage`, `printContractDoc`,
`printProxyContract`, `printAgentContract`, `parseContractParameter`,
`testScenario`, `injectOperation`, `setupClient`) will perform forge
and return an unsigned operation.

#### `tzbtc-client` prerequisites

In order to use `tzbtc-client` you will need to obtain `tezos-client`
executable. You can use one (built for alphanet) located in the
`bin/` folder. `tezos-client` is used for key storing, operation signing and
ledger interaction.

#### `tzbtc-client` usage

`setupClient` command is required for setting up `tzbtc-client`
environment. It takes information about node, user information
(its address and name alias in the `tezos-client`), contract address
and also path to the `tezos-client` executable, which is used for
transaction signing.

`tzbtc-client` interacts with the tezos node using [RPC API](https://tezos.gitlab.io/master/api/rpc.html).
Transaction forging takes place in several stages:

* Get latest block hash, in which our transaction is going to be injected.
* Get sender counter, so that we can construct correct transaction.
* Dry-run this transaction in order to get estimated consumed gas and storage size.
Also, on this stage transaction correctness is ensured.
* Forge transaction with estimated consumed gas, storage size and fee.
Hexadecimal representation of this transaction is returned to the stdout.
* Sign the transaction using `tezos-client`. If your secret key is stored on the
ledger, you will have to open `Tezos Wallet` app and confirm this signing on
your device.
* Inject signed operation using hexademical representation and signature obtained
on the previous steps.


So the workflow for interacting with the TZBTC contract on the chain is the following:
* Use `tzbtc-client setupClient` to set up the environment.
* Use `tzbtc-client <subcommand>` to submit desired operation.

## Contract documentation

Contract documentation is located in [ContractDoc.md](ContractDoc.md).

## Issue Tracker [↑](#ZBTC)

We use [YouTrack](https://issues.serokell.io/issues/TBTC)

## For Contributors [↑](#ZBTC)

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

## License

TODO
