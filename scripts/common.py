# SPDX-FileCopyrightText: 2019 Tocqueville Group, 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: AGPL-3.0-or-later

import subprocess

babylonnet_sh = "./babylonnet.sh"

babylonnet_client = [
    babylonnet_sh, "client", "-A", "jupiter.serokell.io", "-P", "8732",
    ]

def transfer(source, dest, param, burn_cap="20", amount="0", dry_run=False):
    cmd = babylonnet_client + [
        "transfer", amount, "from", source, "to", dest,
        "--burn-cap", burn_cap, "--arg", param,
        ]
    if dry_run:
        print(cmd)
    else:
        subprocess.run(cmd, check=True)
