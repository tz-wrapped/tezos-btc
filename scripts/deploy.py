#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>, 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-MPL-2.0

# This script deploys our contract to a real network using `alphanet.sh`.
# Unless `--dry-run` is passed it assumes that:
# • `alphanet.sh` can be executed;
# • the secret key of the originator address is known and the address has
#   enough XTZ to perform all these operations.

import argparse
import os
import random
import subprocess
import time

from common import (alphanet_client, transfer)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Deploy our contract')
    # We can add more if necessary (e. g. for alphanet_sh)
    parser.add_argument('--owner', required=True,
        metavar='ADDRESS', help="Owner's address")
    parser.add_argument('--redeem', required=True,
        metavar='ADDRESS', help="Redeem address")
    parser.add_argument('--dry-run', action='store_true',
        help="Do not submit transactions/originations")
    parser.add_argument('--our-exe',
        default='stack exec -- tzbtc',
        help="How to launch our executable")

    args = parser.parse_args()
    owner = args.owner
    redeem = args.redeem
    our_exe = args.our_exe.split()

    # Print contract
    contract_tz = "tzbtc.tz"
    subprocess.run(our_exe + ["printContract", "-o", contract_tz])

    # Get initial storage
    initial_storage = subprocess.run(
        our_exe + ["printInitialStorage", owner, redeem],
        capture_output=True).stdout

    # Originate
    contract_alias = "er" + str(random.randrange(100500))
    originate_cmd = alphanet_client + [
        "originate", "contract", contract_alias, "for", owner, "transferring",
        "0", "from", owner, "running", "container:" + contract_tz,
        "--init", initial_storage.strip(),
        "--burn-cap", "10",
        ]
    if args.dry_run:
        print(originate_cmd)
    else:
        subprocess.run(originate_cmd, check=True)

    os.remove(contract_tz)
