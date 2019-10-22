#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>, 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-MPL-2.0

# This script calls `tzbtc-client with some commands`

import argparse
import os
import subprocess
import time

from common import (babylonnet_client, transfer)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Test our contract')
    # We can add more if necessary (e. g. for babylonnet_sh)
    parser.add_argument('--owner', required=True,
        metavar='ADDRESS', help="Owner address")
    parser.add_argument('--contract', required=True,
        metavar='ADDRESS', help="Contract address")
    parser.add_argument('--address', nargs='+',
        metavar='ADDRESS', help="Additional owned addresses")
    parser.add_argument('--owner-alias')
    parser.add_argument('--our-exe',
        default='stack exec -- tzbtc-client',
        help="How to launch our executable")
    parser.add_argument('--tezos-client',
        default='./tezos_client',
        help='How to run tezos_client')

    args = parser.parse_args()
    owner = args.owner
    owner_alias = args.owner_alias
    contract_addr = args.contract
    our_exe = args.our_exe.split()
    tezos_client = args.tezos_client.split()
    addresses = args.address

    test_scenario = [
        ["addOperator", "--operator", owner],
        ["pause"],
        ["unpause"],
        ["mint", "--value", "100500", "--to", addresses[0]],
        ["removeOperator", "--operator", owner]
    ]

    for command in test_scenario:
        print(' '.join(our_exe), ' '.join(command))
        subprocess.run(our_exe + command)
        # Sleep to wait for the next block, so we don't duplicate sender counter
        time.sleep(40)

    print("Finished running scenario")
