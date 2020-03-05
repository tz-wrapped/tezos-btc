#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2019 Tocqueville Group, 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: AGPL-3.0-or-later


# This script is used by CI to test command line things.
# Bash seems to be sufficient for now.
# We may use `bats` if it appears to be necessary.

set -e

our_exe="stack exec -- tzbtc"
# Sample addresses to demonstrate usage
# We pass `--dry-run`, so it doesn not matter which addreses we use
# as long as they are valid.
addr1="tz1TgK3oaBaqcCHankT97AUNMjcs87Tfj5vb"
addr2="tz1PPPYChg5xXHpGzygnNkmzPd1hyVRMxvJf"
addr3="tz1g3oS1UPgWFFpxrc2pEn4sgV3ky1Z6Qaz2"
addrKT="KT1L1Qa7PkDahr9X7K6sp8VdnbHzQxYqb3At"

stack exec -- tzbtc testScenario --dry-run > /dev/null
