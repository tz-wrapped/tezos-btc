# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>, 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-MPL-2.0

import subprocess

alphanet_sh = "./alphanet.sh"

alphanet_client = [
    alphanet_sh, "client", "-A", "jupiter.serokell.io", "-P", "8732",
    ]

def transfer(source, dest, param, burn_cap="20", amount="0", dry_run=False):
    cmd = alphanet_client + [
        "transfer", amount, "from", source, "to", dest,
        "--burn-cap", burn_cap, "--arg", param,
        ]
    if dry_run:
        print(cmd)
    else:
        subprocess.run(cmd, check=True)
