#!/usr/bin/env bats
# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
#

@test "invoking tzbtc-client 'approve' command" {
  stack exec -- tzbtc-client approve\
          --spender "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100 --dry-run

}

@test "invoking tzbtc-client 'mint' command" {
  stack exec -- tzbtc-client mint\
          --to "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100 --dry-run
}

@test "invoking tzbtc-client 'burn' command" {
  stack exec -- tzbtc-client burn --value 100 --dry-run
}

@test "invoking tzbtc-client 'transfer' command" {
  stack exec -- tzbtc-client transfer\
    --to "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --from "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100 --dry-run
}

@test "invoking tzbtc-client 'getAllowance' command" {
  stack exec -- tzbtc-client getAllowance\
    --owner "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --spender "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx"\
    --callback "KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46" --dry-run
}

@test "invoking tzbtc-client 'getAllowance' command without callback" {
  stack exec -- tzbtc-client getAllowance\
    --owner "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --spender "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --dry-run
}

@test "invoking tzbtc-client 'getBalance' command" {
  stack exec -- tzbtc-client getBalance\
    --address "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --callback "KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46" --dry-run
}

@test "invoking tzbtc-client 'getBalance' command without callback" {
  stack exec -- tzbtc-client getBalance\
    --address "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --dry-run
}

@test "invoking tzbtc-client 'getAdministrator' command" {
  stack exec -- tzbtc-client getAdministrator\
    --callback "KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46" --dry-run
}

@test "invoking tzbtc-client 'getAdministrator' command without callback" {
  stack exec -- tzbtc-client getAdministrator --dry-run
}

@test "invoking tzbtc-client 'getTotalSupply' command" {
  stack exec -- tzbtc-client getTotalSupply\
    --callback "KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46" --dry-run
}

@test "invoking tzbtc-client 'getTotalSupply' command without callback" {
  stack exec -- tzbtc-client getTotalSupply --dry-run
}

@test "invoking tzbtc-client 'getTotalMinted' command" {
  stack exec -- tzbtc-client getTotalMinted\
    --callback "KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46" --dry-run
}

@test "invoking tzbtc-client 'getTotalMinted' command without callback" {
  stack exec -- tzbtc-client getTotalMinted --dry-run
}

@test "invoking tzbtc-client 'getTotalBurned' command" {
  stack exec -- tzbtc-client getTotalBurned\
    --callback "KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46" --dry-run
}

@test "invoking tzbtc-client 'getTotalBurned' command without callback" {
  stack exec -- tzbtc-client getTotalBurned --dry-run
}

@test "invoking tzbtc-client 'addOperator' command" {
  stack exec -- tzbtc-client addOperator\
    --operator "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --dry-run
}

@test "invoking tzbtc-client 'removeOperator' command" {
  stack exec -- tzbtc-client removeOperator\
    --operator "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --dry-run
}

@test "invoking tzbtc-client 'pause' command" {
  stack exec -- tzbtc-client pause --dry-run
}

@test "invoking tzbtc-client 'unpause' command" {
  stack exec -- tzbtc-client unpause --dry-run
}

@test "invoking tzbtc-client 'setRedeemAddress' command" {
  stack exec -- tzbtc-client setRedeemAddress "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --dry-run
}

@test "invoking tzbtc-client 'startMigrateFrom' command" {
  stack exec -- tzbtc-client startMigrateFrom "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --dry-run
}

@test "invoking tzbtc-client 'startMigrateTo' command" {
  stack exec -- tzbtc-client startMigrateTo "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --dry-run
}

@test "invoking tzbtc-client 'migrate' command" {
  stack exec -- tzbtc-client migrate --dry-run
}

@test "invoking multisig package creation" {
  stack exec -- tzbtc-client addOperator --operator "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --multisig package.txt --dry-run
}

@test "invoking tzbtc 'printContract' command" {
  stack exec -- tzbtc printContract
}

@test "invoking tzbtc 'printContract' command with --oneline flag" {
  result="$(stack exec -- tzbtc printContract --oneline)"
  [[ "$result" == *"%transfer"* ]]
  [[ "$result" == *"%approve"* ]]
  [[ "$result" == *"%getBalance"* ]]
  [[ "$result" == *"%getAllowance"* ]]
  [[ "$result" == *"%getTotalSupply"* ]]
  [[ "$result" == *"%mint"* ]]
}

@test "invoking tzbtc 'printAgentContract' command" {
  stack exec -- tzbtc printAgentContract
}

@test "invoking tzbtc 'printAgentContract' command with --oneline flag" {
  stack exec -- tzbtc printAgentContract --oneline
}

@test "invoking tzbtc 'printContractDoc' command" {
  stack exec -- tzbtc printContractDoc
}

@test "invoking tzbtc 'printInitialStorage' command" {
  result="$(stack exec -- tzbtc printInitialStorage --admin-address tz1f1S7V2hZJ3mhj47djb5j1saek8c2yB2Cx \
  \--redeem-address tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV)"
  [ "$result" == 'Pair { } (Pair (Pair (Pair "tz1f1S7V2hZJ3mhj47djb5j1saek8c2yB2Cx" (Pair False 0)) (Pair 0 (Pair 0 None))) (Pair (Pair { } (Pair "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" "TZBTC")) (Pair "TZBTC" (Pair None None))))' ]
}

@test "invoking 'parseContractParameter' command to parse burn parameter" {
  raw_parameter="Right (Left (Right (Left (Left 100500)))))"
  exec_command="stack exec -- tzbtc parseContractParameter '${raw_parameter}'"
  result=$(eval $exec_command)
  [ "$result" == 'Burn, value = 100500' ]
}

@test "invoking tzbtc 'setupClient' command without arguments" {
  stack exec -- tzbtc-client setupClient
}

@test "invoking tzbtc 'config' command without arguments" {
  stack exec -- tzbtc-client config
}

@test "invoking tzbtc 'config --edit' command with available arguments" {
  stack exec -- tzbtc-client config --edit --node-url "localhost" --node-port "9900" --contract-address "KT1HmhmNcZKmm2NsuyahdXAaHQwYfWfdrBxi" --multisig-address "KT1HmhmNcZKmm2NsuyahdXAaHQwYfWfdrBxi" --alias alice --tezos-client /local/bin/tezos-client
}

@test "invoking tzbtc 'setupClient' command with arguments" {
  stack exec -- tzbtc-client setupClient --node-url "localhost" --node-port "9900" --contract-address "KT1HmhmNcZKmm2NsuyahdXAaHQwYfWfdrBxi" --multisig-address "KT1HmhmNcZKmm2NsuyahdXAaHQwYfWfdrBxi" --alias alice --tezos-client /local/bin/tezos-client
}
