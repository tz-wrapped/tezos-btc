#!/usr/bin/env bats
# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
#

@test "invoking tzbtc 'printContract' command" {
  stack exec -- tzbtc printContract
}

@test "invoking tzbtc 'printContract' command with --oneline flag" {
  stack exec -- tzbtc printContract --oneline
}

@test "invoking tzbtc 'printAgentContract' command" {
  stack exec -- tzbtc printAgentContract
}

@test "invoking tzbtc 'printAgentContract' command with --oneline flag" {
  stack exec -- tzbtc printAgentContract --oneline
}

@test "invoking tzbtc 'printProxyContract' command" {
  stack exec -- tzbtc printProxyContract
}

@test "invoking tzbtc 'printProxyContract' command with --oneline flag" {
  stack exec -- tzbtc printProxyContract --oneline
}

@test "invoking tzbtc 'printContractDoc' command" {
  stack exec -- tzbtc printContractDoc
}

@test "invoking tzbtc 'printInitialStorage' command" {
  result="$(stack exec -- tzbtc printInitialStorage --admin-address tz1f1S7V2hZJ3mhj47djb5j1saek8c2yB2Cx \
  \--redeem-address tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV)"
  [ "$result" == 'Pair { } (Pair (Pair (Pair "tz1f1S7V2hZJ3mhj47djb5j1saek8c2yB2Cx" (Pair False 0)) (Pair 0 (Pair 0 None))) (Pair (Pair { } (Pair "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" "ZBTC")) (Pair (Pair "TZBTC" None) (Pair None (Left "tz1f1S7V2hZJ3mhj47djb5j1saek8c2yB2Cx")))))' ]
}

@test "invoking 'parseContractParameter' command to parse burn parameter" {
  raw_parameter="Right (Left (Left (Left 100500)))"
  exec_command="stack exec -- tzbtc parseContractParameter '${raw_parameter}'"
  result=$(eval $exec_command)
  [ "$result" == 'Burn, value = 100500' ]
}