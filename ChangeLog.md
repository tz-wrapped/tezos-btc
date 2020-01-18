<!--
 - SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -->

# Changelog for TZBTC

Changes that affect Michelson code are tagged with `[CONTRACT]`.

Unreleased
==========
* [#82](https://github.com/serokell/tezos-btc/pull/82)
  - Fix configuration conflict with `tezos-client` config by not having a separate config file.
  - Fix upgrade failure when contract storage is not immediately available after V0 deploy.
  - Add `tzbtc-client config` command that prints out active configuration.
  - Use default aliases for contract, multisig contract, user and read them from tezos-client config.
  - Mention how to get auto completion for `tzbtc-client` in readme.

* [#86](https://github.com/serokell/tezos-btc/pull/86)
  - Add `getOperators` command to `tzbtc-client`.

* [#83](https://github.com/serokell/tezos-btc/pull/83)
  - Resolve multisig package signing via ledger issue with temporary solution that
  suggests using patched `tezos-client` with `tzbtc-client`.

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
