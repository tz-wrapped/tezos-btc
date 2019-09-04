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
parameters, that can be sumbited to the chain via `tezos-client`.
Also, you can get contract code, initial contracts storage. This
stuff can be used for contract origination via `tezos-client`.
`parseContractParameter` subcommand can be used for debuging,
it parses raw Michelson value to the TZBTC contract parameter.

## Issue Tracker [↑](#ZBTC)

We use [YouTrack](https://issues.serokell.io/issues/TBTC)

## For Contributors [↑](#ZBTC)

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

## License

TODO
