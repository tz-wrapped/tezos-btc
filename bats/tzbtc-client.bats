#!/usr/bin/env bats
# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
#

setup () {
  callback="KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46"
}

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
    --spender "tz3VEZ4k6a4Wx42iyev6i2aVAptTRLEAivNN"\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getAllowance' command without callback" {
  stack exec -- tzbtc-client getAllowance\
    --owner "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --spender "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --dry-run
}

@test "invoking tzbtc-client 'getBalance' command" {
  stack exec -- tzbtc-client getBalance\
    --address "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getBalance' command without callback" {
  stack exec -- tzbtc-client getBalance\
    --address "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --dry-run
}

@test "invoking tzbtc-client 'getOwner' command" {
  stack exec -- tzbtc-client getOwner\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getOwner' command without callback" {
  stack exec -- tzbtc-client getOwner --dry-run
}

@test "invoking tzbtc-client 'getTokenName' command" {
  stack exec -- tzbtc-client getTokenName\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getTokenName' command without callback" {
  stack exec -- tzbtc-client getTokenName --dry-run
}

@test "invoking tzbtc-client 'getTokenCode' command" {
  stack exec -- tzbtc-client getTokenCode\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getTokenCode' command without callback" {
  stack exec -- tzbtc-client getTokenCode --dry-run
}

@test "invoking tzbtc-client 'getRedeemAddress' command" {
  stack exec -- tzbtc-client getRedeemAddress\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getRedeemAddress' command without callback" {
  stack exec -- tzbtc-client getRedeemAddress --dry-run
}

@test "invoking tzbtc-client 'getTotalSupply' command" {
  stack exec -- tzbtc-client getTotalSupply\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getTotalSupply' command without callback" {
  stack exec -- tzbtc-client getTotalSupply --dry-run
}

@test "invoking tzbtc-client 'getTotalMinted' command" {
  stack exec -- tzbtc-client getTotalMinted\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getTotalMinted' command without callback" {
  stack exec -- tzbtc-client getTotalMinted --dry-run
}

@test "invoking tzbtc-client 'getTotalBurned' command" {
  stack exec -- tzbtc-client getTotalBurned\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getTotalBurned' command without callback" {
  stack exec -- tzbtc-client getTotalBurned --dry-run
}

@test "invoking tzbtc-cliet 'getOperators' command" {
  stack exec -- tzbtc-client getOperators --dry-run
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

@test "invoking multisig package creation" {
  stack exec -- tzbtc-client addOperator --operator "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --multisig package.txt --dry-run
}

@test "deploy contract" {
  stack exec -- tzbtc-client deployTzbtcContract --owner boba --redeem boba --token-name Kukareq --token-code Cococoq --dry-run
}

@test "show config" {
  stack exec -- tzbtc-client config --dry-run
}
