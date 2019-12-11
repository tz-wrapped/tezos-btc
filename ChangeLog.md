<!--
 - SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -->

# Changelog for TZBTC

Changes that affect Michelson code are tagged with `[CONTRACT]`.

Unreleased
==========

* [#80](https://github.com/serokell/tezos-btc/pull/80)
  - Add `--token-name` and `--token-code` options to the deploy command.
  - [CONTRACT] Rename `tokenname` and `tokencode` to `tokenName` and `tokenCode` (affects migration to v1, changes storage representation).
  - [CONTRACT] Add `GetTokenName`, `GetTokenCode` and `GetRedeemAddress` view entrypoints.
  - Expose new entrypoints from CLI.

* [#77](https://github.com/serokell/tezos-btc/pull/77)
  Support tz2 and tz3 address in CLI.

* [CONTRACT] Use more efficient `duupX` implementation when its parameter is greater than 2.

* [#75](https://github.com/serokell/tezos-btc/pull/75)
  [CONTRACT] Rename administrator to owner.

0.1.1
=====

* [#74](https://github.com/serokell/tezos-btc/pull/74)
  Preserve annotations during automatic deploy.
* [#66](https://github.com/serokell/tezos-btc/pull/66)
  Add HTTPS support.
* [#65](https://github.com/serokell/tezos-btc/pull/65)
  Fix multisig call.
* [#64](https://github.com/serokell/tezos-btc/pull/64)
  Fix build on MacOS.
* [#68](https://github.com/serokell/tezos-btc/pull/68)
  Add git revision to automatically generated contract documentation.

0.1.0
=====

MVP release.
