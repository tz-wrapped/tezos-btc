<!--
 - SPDX-FileCopyrightText: 2019-2020 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -->

> :warning: **Note:** [Morley framework](https://gitlab.com/morley-framework/) and set of tools are planned for deprecation after protocol "N".
>
> This smart contract will no longer be actively developed then, but future bugs or security issues will be addressed.

# TZBTC

[![Build status](https://badge.buildkite.com/aecd40f89a7a0f1b78495ee2afc40ec575de85e18d42116186.svg?branch=master)](https://buildkite.com/serokell/tezos-btc)

Wrapped Bitcoin on Tezos Blockchain called TZBTC

## Prerequisites

### Build tools

If you want to build this software from sources you should install:
* [Nix package manager ](https://nixos.org/nix/manual/#ch-installing-binary)
* [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

### Operating system

Please note that only Linux is officially supported.
If you want to use `tzbtc-client` on MacOS, you can try [building it from sources](#build-instructions-) using `stack`.

### Executables

In order to use `tzbtc-client` you will need to obtain `tezos-client`
executable. `tezos-client` is used for key storing, operation signing and ledger interaction.

Tezos ledger application supports packed value signing since [v2.2.7](https://github.com/obsidiansystems/ledger-app-tezos/releases/tag/v2.2.7).
In order to use it install the newest version of this app using Ledger Live.
If you have this version installed, you can use non-patched `tezos-client` binary with `tzbtc-client`.
Various forms of distribution for `tezos-client` are presented in [tezos-packaging repo](https://github.com/serokell/tezos-packaging).

## `tzbtc-client` executable

This executable performs transactions injection to the chain using remote or local
tezos-node.

Use `tzbtc-client --help` to get a list of available commands.

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
`tzbtc-user` to use it with `tzbtc-client`. Or you can also override the default
alias `tzbtc-user` using the `--user` argument, in all the commands.

#### Deploy TZBTC contract using `tzbtc-client`

Run `tzbtc-client deployTzbtcContract` command passing the desired owner and redeem address. For example,


`tzbtc-client deployTzbtcContract --owner tz1PPPYChg5TZBTCxXHpGzygnNkmzPd1hyVRMxvJf --redeem tz1PPPYChg5xXHpGzygnNkmzPd1hyVRMxvJf`

The `--owner` argument is optional. If left out `tzbtc-user` alias will be used instead.

After the contract deploy, it is possible to save the newly deployed contract with the alias
`tzbtc`, in the `tezos-client` configuration. You can also manually note the address and
use the `tezos-client` program to alias it as `tzbtc`, as expected by the `tzbtc-client` program.

#### Deploy specialized multisig contract using `tzbtc-client`

It is possible to deploy specialized multisig contract using `tzbtc-client`.

Run `tzbtc-client deployMultisigContract` command passing the desired threshold (minimal amount of signatures
to perform multisig action), the list of signers public keys and optional flag to use custom human-readable errors
in the specialized multisig contract.

For example:
```sh
tzbtc-client deployMultisigContract --threshold 2 \
  --public-key edpkvD74whcSw91LUtJex5V16115w4BTwgu3dXHdtNDbbbBRiYEQqP \
  --public-key edpkvXeP2mxHEyF8GSakTy6Fg2G9EsN23eUbjBWtfgTez3b2ZQj3nL \
  --public-key edpkvJq6xRTcQyBpGVLPEcNqRijB2WKZ2M1TYvWhiKjQQPUzYgq9ec \
  --use-custom-errors
```

After the contract deploy, it is possible to save the newly deployed multisig contract with the alias
`tzbtc-multisig`, in the `tezos-client` configuration. You can also manually note the address and
use the `tezos-client` program to alias it as `tzbtc-multisig`, as expected by the `tzbtc-client` program.

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

Aliases in `tezos-client` can be ambiguous, so you may need to prefix the alias with `implicit:` for `tz1`/`tz2`/`tz3` addresses or `contract:` for `KT1` addresses. If an unprefixed alias could resolve to either, `tzbtc` will refuse to accept it. E.g. `tzbtc-client mint --to implicit:alice --value 500`

`tzbtc-client` also provides multisig support.

Multisig interaction based on specialized multisig contract.
This contract has threshold (minimal required amount of signatures) and list of signers
public keys in its storage.

`tzbtc-client` supports multisig for administrative operations (such as `mint`, `burn`,
`add/removeOperator`, `pause`, `unpause`, `setRedeemAddress`, `transferOwnership`,
`startMigrateTo/From`)
In order to perform these actions make sure, that multisig contract's address is
an owner/operator of the TZBTC contract.

All administrative operations can be performed using multisig.  In order to
create a multisig package (a file that holds the contract operation along with
the signatures) you should provide the path to the package file using the
`--multisig-package` option. By default this command will use the alias
`tzbtc-multisig` as the contract alias of the multisig that should be used. But
you can override this using the `--multisig-addr` option.

E.g. `tzbtc-client pause --multisig-package path-to-package-file`. This command
will create the encoded multisig package at the specified path.

You can get operation description from this package using `tzbtc-client getOpDescription` command.

There are two ways to sign multisig package:
* Sign package via `tzbtc-client signPackage --package <package filepath>` command.
Thus given package will be signed by the tzbtc user.
* Manually sign package. In order to extract bytes that needs to be signed you should use
`tzbtc-client getBytesToSign` command. After these bytes are signed, the signature can be
added using `tzbtc-client addSignature` command.

Once multisig operation initiator have obtained enough signed packages he can start this
operation using `tzbtc-client callMultisig` command.

##### Setting up command line auto completion

To setup autocompletion for `tzbtc-client` commands in terminal,
first ensure that `tzbtc-client` program is available in path.
Then, run the following command in a terminal.

```
source <(tzbtc-client --bash-completion-script `which tzbtc-client`)
```

Note that the setting does not persist, and if you open another terminal, you will have to
do this in the new terminal to get autocompletion there.

## `tzbtc` executable

You can use `tzbtc` executable in order to get contract code converted
to Michelson, raw Michelson contract storage. This stuff can be used for
contract origination via `tezos-client`.
`parseContractParameter` subcommand can be used for debugging,
it parses raw Michelson value to the TZBTC contract parameter.

Use `tzbtc --help` to get a list of available commands.

## Build instructions [↑](#tzbtc)

You can build `tzbtc-client` and `tzbtc` from the sources with stack.
Build the project using `stack build`, run executables using
`stack exec tzbtc` or `stack exec tzbtc-client`. Also you can use
`stack install tzbtc --local-bin-path ./bin`, thus `tzbtc-client` and `tzbtc` binaries
will be in `./bin` directory.

CI uses nix to build the project and to produce `.deb` and `.rpm` packages.
You can use nix locally as well, but it is not recommended, it
requires building GHC from scratch and takes a long time. Commands for
building the executables and packages using nix:

```bash
nix-build ci.nix -A tzbtc.components.exes.tzbtc -o tzbtc-exe
nix-build ci.nix -A tzbtc.components.exes.tzbtc-client -o tzbtc-client-exe
nix-build release.nix -A deb -o tzbtc-client-deb
nix-build release.nix -A rpm -o tzbtc-client-rpm
```

## Obtain static binary or package

You can either build them from the source code using nix or download the version from
[latest release](https://github.com/tz-wrapped/tezos-btc/releases/latest).

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

## Tests [↑](#tzbtc)

Run `stack test` and explore the tests.

## Contract documentation [↑](#tzbtc)

Contract documentation is located at [TZBTC-contract.md](https://github.com/tz-wrapped/tezos-btc/blob/autodoc/master/TZBTC-contract.md).

## Security considerations and trust model [↑](#tzbtc)

This contract has `approve` entrypoint, which should be used carefully, you can read more
about the potential unsafety of this entrypoint in the [corresponding section](https://github.com/tz-wrapped/tezos-btc/blob/autodoc/master/TZBTC-contract.md#approve)
in the contract documentation.

Any `operator` can pause the contract, thus all users will be unable to transfer their tokens.
Additionaly, `operator` is capable of minting and burning tokens.

`owner` of the contract can upgrade the contract and thus can potentially change any information stored in the
contract storage including the user's balances.

You can read more about `operator` and `owner` abilities in the [contract documentation](https://github.com/tz-wrapped/tezos-btc/blob/autodoc/master/TZBTC-contract.md#tzbtc).

## Issue Tracker [↑](#tzbtc)

If you want to create a new issue please do it on [GitHub](https://github.com/tz-wrapped/tezos-btc/issues/new).

## For Contributors [↑](#tzbtc)

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

## License

[MIT](/LICENSE)
