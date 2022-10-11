<!--
 - SPDX-FileCopyrightText: 2019-2020 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -->

# Changelog for TZBTC

Changes that affect Michelson code are tagged with `[CONTRACT]`.

Unreleased
==========

* [180](https://github.com/tz-wrapped/tezos-btc/pull/180)
  Morley dependencies updated to provide changes for Kathmandu protocol support.
  + Note that due to changes upstream, CLI changed slightly. Previously,
    ambiguous tezos-client aliases would always resolve to a contract addresses,
    now those will fail unless explicitly disambiguated with `implicit:` or
    `contract:` prefix.

0.8.0
==========
* [#172](https://github.com/tz-wrapped/tezos-btc/pull/172)
  + Morley dependencies updated to provide changes that support Ithaca protocol.
* [#159](https://github.com/tz-wrapped/tezos-btc/pull/159)
  + Switch tests to nettest engine.

0.7.0
==========
* [#137](https://github.com/tz-wrapped/tezos-btc/pull/141)
  + `tzbtc-client` is reimplemented to use code from the [`morley-client`](https://gitlab.com/morley-framework/morley/-/tree/master/code/morley-client).


0.6.0
==========
* [#134](https://github.com/tz-wrapped/tezos-btc/pull/134)
  + `migrate` command of the executable now accepts version differently.
    Instead of
    `tzbtc migrate --version=1 ...`
    use
    `tzbtc migrate v1 ...`.

* [#133](https://github.com/tz-wrapped/tezos-btc/pull/133)
  + Morley dependency updated.
  + Types appearing in the parameter and storage of the contract now have field annotations.
    + Some existing type annotations are also replaced with field annotations.
  + Minor code optimizations applied.
  + Minor changes in error messages:
    + Occurrences of "EntryPoint" word are now replaced with "Entrypoint";
    + Some internal error messages changed.
  + Multisig contract affected in a similar way.
  + `testScenario` command of `tzbtc` executable now differentiates between verbosity modes (e.g. `-v`, `-vv`).

0.5.0
=====
* [#112](https://github.com/tz-wrapped/tezos-btc/pull/112)
  - Update to use new version of multisig contract that also include chain id
  in the signature payload. This is a breaking change and the `tzbtc-client`
  program in this release will not be compatible with previously deployed, older
  versions of the multisig contract.

0.4.1
=====
* [#122](https://github.com/tz-wrapped/tezos-btc/pull/122)
  - Fix loss of precision during the parsing of `burn-cap`, `baker-fee` values.

0.4.0
=====
* [#115](https://github.com/tz-wrapped/tezos-btc/pull/115)
  - [CONTRACT] Add FA2-style metadata
  - Move `tokenName`, `tokenCode` fields to `TokenMetadata`
  - `tokenCode`/`token-code` is now `tokenSymbol`/`token-symbol`
  - New `tzbtc-client` contract metadata option, provided along with `token-name`/`token-symbol`: `token-decimals`
* [#113](https://github.com/tz-wrapped/tezos-btc/pull/113)
  - Fix baker fee calculation for transactions by using `tezos-client` to compute fees.
  - Add global option "--fee" to explicitly specify baker fees for transactions.
  - Add global option "--verbose" to enable verbose output.
* [#108](https://github.com/tz-wrapped/tezos-btc/pull/108)
  - Add global option "--contract-addr" to override the default TZBTC contract address/alias.
  - Add option "--dry-run" to `testScenario` command to run the tests using integrational test
  framework.

0.3.1
=====
* [#105](https://github.com/tz-wrapped/tezos-btc/pull/105)
  - Add global option "--multisig-addr" to override the default multisig address/alias.
* [#104](https://github.com/tz-wrapped/tezos-btc/pull/104)
  - Updated patched `tezos-client` building.
* [#99](https://github.com/tz-wrapped/tezos-btc/pull/99)
  - "migrate" CLI command does not accept new owner anymore rather retains
    the owner from V0.
* [#100](https://github.com/tz-wrapped/tezos-btc/pull/100)
  - Add `--multisig` option for `acceptOwnership` tzbtc-client command.

0.3.0
=====
* [#98](https://github.com/tz-wrapped/tezos-btc/pull/98)
  - Further update automated documentation.
* [#96](https://github.com/tz-wrapped/tezos-btc/pull/96)
  - Use specialized multisig contract instead of generic multisig contract.
* [#94](https://github.com/tz-wrapped/tezos-btc/pull/94)
  - Add upgradeability description to automatically generated documentation.
  - [CONTRACT] Slightly update upgrade mechanism, now user have to provide not only
  new but also current version during the upgrade.
* [#93](https://github.com/tz-wrapped/tezos-btc/pull/93)
  - Add command for deploying generic multisig contract to `tzbtc-client`.
* [#81](https://github.com/tz-wrapped/tezos-btc/pull/81)
  - Changed internal versioning slightly, now contract version can be changed arbitrarily on owner's behalf.

0.2.0
=====
* [#85](https://github.com/tz-wrapped/tezos-btc/pull/87)
  - Add global option `--user` to override default `tzbtc-user` user alias

* [#82](https://github.com/tz-wrapped/tezos-btc/pull/82)
  - Fix configuration conflict with `tezos-client` config by not having a separate config file.
  - Fix upgrade failure when contract storage is not immediately available after V0 deploy.
  - Add `tzbtc-client config` command that prints out active configuration.
  - Use default aliases for contract, multisig contract, user and read them from tezos-client config.
  - Mention how to get auto completion for `tzbtc-client` in readme.

* [#86](https://github.com/tz-wrapped/tezos-btc/pull/86)
  - Add `getOperators` command to `tzbtc-client`.

* [#83](https://github.com/tz-wrapped/tezos-btc/pull/83)
  - Resolve multisig package signing via ledger issue with temporary solution that
  suggests using patched `tezos-client` with `tzbtc-client`.

* [#80](https://github.com/tz-wrapped/tezos-btc/pull/80)
  - Add `--token-name` and `--token-code` options to the deploy command.
  - [CONTRACT] Rename `tokenname` and `tokencode` to `tokenName` and `tokenCode` (affects migration to v1, changes storage representation).
  - [CONTRACT] Add `GetTokenName`, `GetTokenCode` and `GetRedeemAddress` view entrypoints.
  - Expose new entrypoints from CLI.

* [#77](https://github.com/tz-wrapped/tezos-btc/pull/77)
  Support tz2 and tz3 address in CLI.

* [CONTRACT] Use more efficient `duupX` implementation when its parameter is greater than 2.

* [#75](https://github.com/tz-wrapped/tezos-btc/pull/75)
  [CONTRACT] Rename administrator to owner.

0.1.1
=====

* [#74](https://github.com/tz-wrapped/tezos-btc/pull/74)
  Preserve annotations during automatic deploy.
* [#66](https://github.com/tz-wrapped/tezos-btc/pull/66)
  Add HTTPS support.
* [#65](https://github.com/tz-wrapped/tezos-btc/pull/65)
  Fix multisig call.
* [#64](https://github.com/tz-wrapped/tezos-btc/pull/64)
  Fix build on MacOS.
* [#68](https://github.com/tz-wrapped/tezos-btc/pull/68)
  Add git revision to automatically generated contract documentation.

0.1.0
=====

MVP release.
