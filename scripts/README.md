<!--
 - SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>, 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MPL-2.0
 -->

# Scripts

Here we store scripts for various purposes.
Brief overview is provided below.

## Development

* `lint.hs` calls `hlint` for source files with proper arguments.

## Testing

* `test.py` can be used to test our contract on a real network.
* `test_client.py` can be used to test `tzbtc-client`.

Please refer to their CLI help and comments for more information, they are pretty simple.
Here is a common workflow:
1. Deploy TZBTC contract using `tzbtc-client deployTzbtcContract`
Find the address of the contract in the output.
To test the contract you need to provide at least two owned addresses.
2. Run `scripts/test.py --owner OWNER_ADDR --contract CONTRACT_ADDR --address OWNED_ADDR1 [OWNED_ADDR2 ...]`.
3. Run `scripts/test_client.py` --owner OWNER_ADDR --owner_secret_key SECRET_KEY --contract CONTRACT_ADDR --address OWNED_ADDR1 [OWNED_ADDR2 ...]

## CI

CI runs some executable to test them.
We put all these calls into `ci-test.sh`.
Note that for now we do not use Tezos network there at all and do not use `tezos-client` or similar software.
It runs the above scripts in `--dry-run` mode.
