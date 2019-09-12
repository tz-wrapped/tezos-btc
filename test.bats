#!/usr/bin/env bats
# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
#

@test "invoking tzbtc 'approve' command" {
  result="$(stack exec -- tzbtc approve\
          --spender "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100)"
  [ "$result" == 'Left (Left (Left (Right (Right (Pair "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" 100)))))' ]

}

@test "invoking tzbtc 'mint' command" {
  result="$(stack exec -- tzbtc mint\
          --to "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100)"
  [ "$result" == 'Left (Right (Right (Right (Right (Pair "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" 100)))))' ]

}

@test "invoking tzbtc 'burn' command" {
  result="$(stack exec -- tzbtc burn --value 100)"
  [ "$result" == 'Right (Left (Left (Left 100)))' ]
}

@test "invoking tzbtc 'transfer' command" {
  result="$(stack exec -- tzbtc transfer\
    --to "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --from "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100)"
  [ "$result" == 'Left (Left (Left (Left (Pair "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" (Pair "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" 100)))))' ]
}

@test "invoking tzbtc 'getAllowance' command" {
  result="$(stack exec -- tzbtc getAllowance\
    --owner "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --spender "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx"\
    --callback "KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46")"
  [ "$result" == 'Left (Left (Right (Right (Left (Pair (Pair "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx") "KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46")))))' ]
}

@test "invoking tzbtc 'getBalance' command" {
  result="$(stack exec -- tzbtc getBalance\
    --address "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --callback "KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46")"
  [ "$result" == 'Left (Left (Right (Right (Right (Pair "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" "KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46")))))' ]
}

@test "invoking tzbtc 'addOperator' command" {
  result="$(stack exec -- tzbtc addOperator\
    --operator "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV")"
  [ "$result" == 'Right (Left (Left (Right (Left "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"))))' ]
}

@test "invoking tzbtc 'removeOperator' command" {
  result="$(stack exec -- tzbtc removeOperator\
    --operator "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV")"
  [ "$result" == 'Right (Left (Left (Right (Right "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"))))' ]
}

@test "invoking tzbtc 'pause' command" {
  result="$(stack exec -- tzbtc pause)"
  [ "$result" == 'Right (Left (Right (Right (Left Unit))))' ]
}

@test "invoking tzbtc 'unpause' command" {
  result="$(stack exec -- tzbtc unpause)"
  [ "$result" == 'Right (Left (Right (Right (Right Unit))))' ]
}

@test "invoking tzbtc 'setRedeemAddress' command" {
  result="$(stack exec -- tzbtc setRedeemAddress "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV")"
  [ "$result" == 'Right (Left (Right (Left "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV")))' ]
}

@test "invoking tzbtc 'startMigrateFrom' command" {
  result="$(stack exec -- tzbtc startMigrateFrom "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV")"
  [ "$result" == 'Right (Right (Right (Left (Left "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"))))' ]
}

@test "invoking tzbtc 'startMigrateTo' command" {
  result="$(stack exec -- tzbtc startMigrateTo "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV")"
  echo $result
  [ "$result" == 'Right (Right (Left (Right (Right "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"))))' ]
}

@test "invoking tzbtc 'migrate' command" {
  result="$(stack exec -- tzbtc migrate)"
  echo $result
  [ "$result" == 'Right (Right (Right (Right (Left Unit))))' ]
}

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
  result="$(stack exec -- tzbtc printInitialStorage tz1f1S7V2hZJ3mhj47djb5j1saek8c2yB2Cx tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV)"
  [ "$result" == 'Pair { } (Pair (Pair (Pair "tz1f1S7V2hZJ3mhj47djb5j1saek8c2yB2Cx" (Pair False 0)) (Pair 0 (Pair 0 None))) (Pair (Pair { } (Pair "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" "ZBTC")) (Pair (Pair "TZBTC" None) (Pair None (Left "tz1f1S7V2hZJ3mhj47djb5j1saek8c2yB2Cx")))))' ]
}

@test "invoking 'parseContractParameter' command to parse burn parameter" {
  raw_parameter="$(stack exec -- tzbtc burn --value 100500)"
  exec_command="stack exec -- tzbtc parseContractParameter '${raw_parameter}'"
  result=$(eval $exec_command)
  [ "$result" == 'Burn, value = 100500' ]
}