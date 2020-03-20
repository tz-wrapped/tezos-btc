#!/usr/bin/env bats
# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
#

setup () {
  tzbtc="tzbtc"
}

@test "invoking tzbtc 'printContract' command" {
  $tzbtc printContract
}

@test "invoking tzbtc 'printMultisigContract' command" {
  $tzbtc printMultisigContract
  $tzbtc printMultisigContract --use-custom-errors
}

@test "invoking tzbtc 'migrate' command" {
  $tzbtc migrate --version 1 --redeemAddress "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --tokenCode aa
}

@test "invoking tzbtc 'printContract' command with --oneline flag" {
  result="$($tzbtc printContract --oneline)"
  [[ "$result" == *"%transfer"* ]]
  [[ "$result" == *"%approve"* ]]
  [[ "$result" == *"%getBalance"* ]]
  [[ "$result" == *"%getAllowance"* ]]
  [[ "$result" == *"%getTotalSupply"* ]]
  [[ "$result" == *"%mint"* ]]
}

@test "invoking tzbtc 'printContractDoc' command" {
  $tzbtc printContractDoc
}

@test "invoking tzbtc 'printInitialStorage' command" {
  result="$($tzbtc printInitialStorage --owner-address tz1f1S7V2hZJ3mhj47djb5j1saek8c2yB2Cx)"
  [ "$result" == 'Pair { Elt 0x0501000000056f776e6572 0x050a000000160000d476acd953eb55d38c398c85c3f53e19b62b167a } (Pair { CDR; NIL operation; PAIR } (Pair 0 False))' ]
}

@test "invoking 'parseContractParameter' command to parse burn parameter" {
  raw_parameter="Right (Right (Right (Right (Right (Left (Left (Right 100500)))))))"
  exec_command="$tzbtc parseContractParameter '${raw_parameter}'"
  result=$(eval $exec_command)
  [ "$result" == 'Burn, value = 100500' ]
}

@test "invoking smoke tests" {
  $tzbtc testScenario --dry-run
}
