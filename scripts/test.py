#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>, 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-MPL-2.0

# This script calls some entrypoints of our contract using `alphanet.sh`.
# Unless `--dry-run` is passed it assumes that:
# • `alphanet.sh` can be executed.

import argparse
import os
import subprocess
import time

from common import (alphanet_client, transfer)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Test our contract')
    # We can add more if necessary (e. g. for alphanet_sh)
    parser.add_argument('--owner', required=True,
        metavar='ADDRESS', help="Owner address")
    parser.add_argument('--contract', required=True,
        metavar='ADDRESS', help="Contract address")
    parser.add_argument('--address', nargs='+',
        metavar='ADDRESS', help="Additional owned addresses")
    parser.add_argument('--dry-run', action='store_true',
        help="Do not submit transactions")
    parser.add_argument('--our-exe',
        default='stack exec -- tzbtc',
        help="How to launch our executable")

    args = parser.parse_args()
    owner = args.owner
    contract_addr = args.contract
    our_exe = args.our_exe.split()

    # Print scenario
    scenario = "scenario.txt"
    subprocess.run(our_exe +
        ["testScenario", "-o", scenario, owner] +
        [arg for addr in args.address for arg in ["--address", addr]]
        )

    with open(scenario, mode="r") as f:
        lines = list(f.readlines())
        for i in range(len(lines) // 3):
            comment = lines[i * 3].strip()
            addr = lines[i * 3 + 1].strip()
            param = lines[i * 3 + 2].strip()

            print(comment)
            print('Addr:', addr)
            print('Param:', param)

            transfer(
                source=addr, dest=contract_addr, param=param,
                burn_cap="45", dry_run=args.dry_run,
                )
            if not args.dry_run:
                time.sleep(20)

    print("Finished running scenario")
    os.remove(scenario)
