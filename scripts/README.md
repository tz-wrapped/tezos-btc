<!--
 - SPDX-FileCopyrightText: 2019 Tocqueville Group, 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: AGPL-3.0-or-later
 -->

# Scripts

Here we store scripts for various purposes.
Brief overview is provided below.

## Development

* `lint.hs` calls `hlint` for source files with proper arguments.

## CI

CI runs some executable to test them. It is done from
[bats/tzbtc.bats](../bats/tzbtc.bats) file.  Note that for now we do not use Tezos
network there at all and do not use `octez-client` or similar software.  It
runs the above scripts in `--dry-run` mode.

CI also have a script, [scripts/ci/upload-autodoc.sh](ci/upload-autodoc.sh)
that generates and push contract documentation to `autodoc/master` branch in
the repo (this script is only executed for the `master` branch).

### Running CI locally

You can run the tests using [bats](https://github.com/sstephenson/bats).
It should be possible to install it using your systems package manager. For Ubuntu you can install it using

```bash
apt-get install bats.
```

After that you can run the tests using command `bats bats/tzbtc-client.bats`.
Please ensure `tzbtc` and `tzbtc-client` binaries are in path.
