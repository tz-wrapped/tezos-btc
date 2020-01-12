<!--
 - SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-Proprietary
 -->

# TZBTC

Wrapped Bitcoin on Tezos Blockchain called TZBTC

## `tzbtc-client` executable

This executable performs transactions injection to the chain using remote or local
tezos-node.

Use `tzbtc-client --help` to get a list of available commands.

### Prerequisites

In order to use `tzbtc-client` you will need to obtain `tezos-client`
executable. `tezos-client` is used for key storing, operation signing and ledger interaction.
For now, `tezos-client` doesn't support packed value signing via ledger,
so in order to fully use `tzbtc-client` (perform multisig package signing via ledger)
you should use patched `tezos-client` binary. These binaries are bundled along
with `tzbtc-client` in the [releases](https://github.com/serokell/tezos-btc/releases).

Alternatively, you can build patched binaries using nix.

In order to do that you should run one of the following commands:
```
nix-build release.nix -A tezos-client-mainnet -o tezos-client-mainnet

nix-build release.nix -A tezos-client-babylonnet -o tezos-client-babylonnet
```
Thus `tezos-client-{mainnet, babylonnet}` directory will have desired statically built
binary which can be used later.

For more information about this issue take a look at [MR in tezos repo](https://gitlab.com/tezos/tezos/merge_requests/1449).

If you are not going to use multisig package signing with ledger device,
you can obtain non-patched version of `tezos-client` in various form of distribution from
[tezos-packaging repo](https://github.com/serokell/tezos-packaging).

### `tzbtc-client` usage [↑](#tzbtc-client-executable)

#### Setup `tzbtc-client`

`tzbtc-client` program inherits configuration from the configuration of
`tezos-client`. So you should use it to configure the tezos node, port,
https/tls settings for `tzbtc-client` program as well.

The `tezos-client` program is expected to be in the path. You can
also use the `TZBTC_TEZOS_CLIENT` environment variable to set the
`tezos-client` program that `tzbtc-client` should use.

There are also a number of `tezos-client` aliases that `tzbtc-client`
program expects.

`tzbtc-user` is the alias that will be used to create transactions.
`tzbtc` is the alias of the TZBTC contract.
`tzbtc-multisig` is the alias of the multisig contract. This should
be originated separately and aliased as `tzbtc-multisig`.

If there is an existing address that you want to use for tzbtc operations,
then you probably have to use the force flag to add a duplicate alias as shown below.

```
tezos-client add address tzbtc-user tz1RyNvnKnkcD6m9E5VAMxVWVQM5fb9pQo4d --force
```

But unfortunately, right now there is [a bug](https://gitlab.com/tezos/tezos/issues/653)
in `tezos-client` that does not allow adding duplicate alias that point to one of the
existing aliased address. So instead, you will have to rename the existing alias as
`tzbtc-user` to use it with `tzbtc-client`. Soon, `tzbtc-client` will be amended to
include a flag [to override this default user alias.](https://issues.serokell.io/issue/TBTC-85)

#### Deploy TZBTC contract using `tzbtc-client`

Run `tzbtc-client deployTzbtcContract` command passing the desired owner and redeem address. For example,


`tzbtc-client deployTzbtcContract --owner tz1PPPYChg5TZBTCxXHpGzygnNkmzPd1hyVRMxvJf --redeem tz1PPPYChg5xXHpGzygnNkmzPd1hyVRMxvJf`

The `--owner` argument is optional. If left out `tzbtc-user` alias will be used instead.

After the contract deploy, it is possible to save the newly deployed contract with the alias
`tzbtc`, in the `tezos-client` configuration. You can also manually note the address and
use the `tezos-client` program to alias it as `tzbtc`, as expected by the `tzbtc-client` program.

#### Interact with TZBTC contract

Other commands will perform injection of desired transaction to the
TZBTC contract. E.g. `tzbtc-client mint --to tz1U1h1YzBJixXmaTgpwDpZnbrYHX3fMSpvby --value 100500`
will mint 100500 tokens to the `tz1U1h1YzBJixXmaTgpwDpZnbrYHX3fMSpvby` address.
This command will change actual contract storage in the chain.

`tzbtc-client` interacts with the tezos node using [RPC API](https://tezos.gitlab.io/api/rpc.html).
Transaction forging takes place in several stages:

* Get latest block hash, in which our transaction is going to be injected.
* Get sender counter, so that we can construct correct transaction.
* Dry-run this transaction in order to get estimated consumed gas and storage size.
Also, on this stage transaction correctness is ensured.
* Forge transaction with estimated consumed gas, storage size and fee.
* Sign the transaction using `tezos-client`. If your secret key is stored on the
ledger, you will have to open `Tezos Wallet` app and confirm this signing on
your device.
* Inject signed operation using hexademical representation and signature obtained
on the previous steps.


So the workflow for interacting with the TZBTC contract on the chain is the following:

* Setup `tezos-client` and add `tzbtc-user` alias and make sure it is available in PATH
(or use the environment variable `TZBTC_TEZOS_CLIENT` to set the path)
* Deploy contract using `deployTzbtcContract` command and alias it as `tzbtc` (either manually or
  by letting the `tzbtc-client` program create the alias when prompted) or alias an existing
contract as `tzbtc`.
* If required, deploy multisig contract, and alias it as `tzbtc-multisig`.
* Use `tzbtc-client <subcommand>` to submit desired operation.

All `tzbtc-client` commands can be performed with `--dry-run` flag, thus they won't
interact with the chain at all. This flag is basically used for testing purposes in
order to check that argument parser is sane.

Note that instead of plain addresses you can use `tezos-client` aliases as an arguments
in `tzbtc-client`. E.g. `tzbtc-client mint --to alice --value 500` (assuming that
`alice` is an alias for some address in `tezos-client`).

`tzbtc-client` also provides multisig support.

Multisig interaction based on [generic multisig contract](contracts/MultiSigGeneric.tz).
This contract has threshold (minimal required amount of signatures) and list of signers
public keys in its storage.

`tzbtc-client` supports multisig for administrative operations (such as `mint`, `burn`,
`add/removeOperator`, `pause`, `unpause`, `setRedeemAddress`, `transferOwnership`,
`startMigrateTo/From`)
In order to perform these actions make sure, that multisig contract's address is
an owner/operator of the TZBTC contract.

All administrative operations can be performed using multisig.
In order to create multisig package you should provide `--multisig` flag.
E.g. `tzbtc-client pause --multisig`. This command will return encoded multisig package.

You can get operation description from this package using `tzbtc-client getOpDescription` command.

There are two ways to sign multisig package:
* Sign package via `tzbtc-client signPackage --package <package filepath>` command.
Thus given package will be signed by the tzbtc user.
* Manually sign package. In order to extract bytes that needs to be signed you should use
`tzbtc-client getBytesToSign` command. After these bytes are signed, the signature can be
added using `tzbtc-client addSignature` command.

Once multisig operation initiator have obtained enough signed packages he can start this
operation using `tzbtc-client callMultisig` command.

## `tzbtc` executable

You can use `tzbtc` executable in order to get contract code converted
to Michelson, raw Michelson contract storage. This stuff can be used for
contract origination via `tezos-client`.
`parseContractParameter` subcommand can be used for debugging,
it parses raw Michelson value to the TZBTC contract parameter.

Use `tzbtc --help` to get a list of available commands.

## Build instructions [↑](#TZBTC)

You can build `tzbtc-client` and `tzbtc` from the sources.

There are two ways:
* Build stack project `stack build`, thus you'll be able to run executables using
`stack exec tzbtc` or `stack exec tzbtc-client`. Also you can use
`stack install tzbtc --local-bin-path ./bin`, thus `tzbtc-client` and `tzbtc` binaries
will be in `./bin` directory. Note that stack newer than 1.9 is
not supported due to this bug: [commercialhaskell/stack#4984](https://github.com/commercialhaskell/stack/issues/4984).
* Build static binaries from the stack project using nix. For this you will need to run:
``` bash
$(nix-build --no-link -A fullBuildScript) -o ./tzbtc-static
```
Static binaries will be located in `./tzbtc-static/bin` directory.

### Building packages

Once you've built static binary you can create `.deb` or `.rpm` package with
`tzbtc-client`. In order to do that run the one of the following commands:
```bash
nix-build release.nix --arg tzbtc-client-binary <path to tzbtc-client binary> -A packageIntoRpm -o tzbtc-client-package
nix-build release.nix --arg tzbtc-client-binary <path to tzbtc-client binary> -A packageIntoDeb -o tzbtc-client-package
```
After that the packages can be found in `./tzbtc-client-package` directory.

## Obtain static binary or package

You can either build them from the source code using nix or download the version from
[latest release](https://github.com/serokell/tezos-btc/releases/latest).

## Install packages

Installing `.deb` package:
```bash
sudo apt install <path to deb file>
#or
sudo dpkg -i <path to deb file>
```

Installing `.rpm` package:
```bash
sudo yum localinstall <path to the rpm file>
```

## Tests [↑](#TZBTC)

Run `stack test` and explore the tests.

## Contract documentation [↑](#TZBTC)

<!-- TODO TBTC-73 This link is a bit hacky, it relies on GitHub internals to some extent.
-->

Contract documentation is located at [TZBTC-contract.md](../autodoc/master/TZBTC-contract.md).

## Issue Tracker [↑](#TZBTC)

We use [YouTrack](https://issues.serokell.io/issues/TBTC)

## For Contributors [↑](#TZBTC)

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

## License

TODO
