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

You can use `tzbtc` executable in order to get raw Michelson
parameters, that can be submitted to the chain via `tezos-client`.
Also, you can get contract code, initial contracts storage. This
stuff can be used for contract origination via `tezos-client`.
`parseContractParameter` subcommand can be used for debugging,
it parses raw Michelson value to the TZBTC contract parameter.

Use `tzbtc --help` to get a list of available commands.

Also you can use `tzbtc-client` executable. Unlike the `tzbtc`
this executable performs transactions to the chain using remote
tezos-node. In order to setup `tzbtc-client` you should use
`tzbtc-client setupClient` command. Most of the commands (except
`printContract`, `printInitialStorage`, `parseContractParameter`,
`testScenario`) will perform a transaction and return operation hash
which can later be checked in the block explorer.

`tzbtc-client` interacts with the tezos node using [RPC API](https://tezos.gitlab.io/master/api/rpc.html).
Transaction execution takes place in several stages:

* Get latest block hash, in which our transaction is going to be injected.
* Get sender counter, so that we can construct correct transaction.
* Forge transaction to get it hexademical representation and sign it using user
secret key. This signature is required for preapplying and injection.
* Preapply this transaction in order to get estimated consumed gas and storage size.
Also, on this stage transaction correctness is ensured.
* Forge transaction once again, now with estimated consumed gas, storage size and fee.
Hexademical representation of this transaction is once again signed using user
secret key.
* Inject transaction using hexademical representation and signature obtained during
previous step.

As a result of injection we will have transaction hash, that can be checked in the
block explorer.

## Issue Tracker [↑](#ZBTC)

We use [YouTrack](https://issues.serokell.io/issues/TBTC)

## For Contributors [↑](#ZBTC)

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

## License

TODO
