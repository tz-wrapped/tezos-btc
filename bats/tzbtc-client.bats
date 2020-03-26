#!/usr/bin/env bats
# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
#

setup () {
  tzbtc_client="tzbtc-client"
  callback="KT1SyriCZ2kDyEMJ6BtQecGkFqVciQcfWj46"
}

@test "invoking tzbtc-client 'approve' command" {
  $tzbtc_client approve\
          --spender "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100 --dry-run

}

@test "invoking tzbtc-client 'approve' command with user" {
  $tzbtc_client approve\
          --spender "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100 --dry-run --user john

}

@test "invoking tzbtc-client 'mint' command" {
  $tzbtc_client mint\
          --to "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100 --dry-run
}

@test "invoking tzbtc-client 'mint' command with user" {
  $tzbtc_client mint\
          --to "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100 --dry-run --user john
}

@test "invoking tzbtc-client 'burn' command" {
  $tzbtc_client burn --value 100 --dry-run
}

@test "invoking tzbtc-client 'burn' command with user" {
  $tzbtc_client burn --value 100 --dry-run --user john
}

@test "invoking tzbtc-client 'transfer' command" {
  $tzbtc_client transfer\
    --to "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --from "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100 --dry-run
}

@test "invoking tzbtc-client 'transfer' command with user" {
  $tzbtc_client transfer\
    --to "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --from "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --value 100 --dry-run --user john
}

@test "invoking tzbtc-client 'getAllowance' command" {
  $tzbtc_client getAllowance\
    --owner "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --spender "tz3VEZ4k6a4Wx42iyev6i2aVAptTRLEAivNN"\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getAllowance' command without callback" {
  $tzbtc_client getAllowance\
    --owner "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --spender "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" --dry-run
}

@test "invoking tzbtc-client 'getBalance' command" {
  $tzbtc_client getBalance\
    --address "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV"\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getBalance' command without callback" {
  $tzbtc_client getBalance\
    --address "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --dry-run
}

@test "invoking tzbtc-client 'getOwner' command" {
  $tzbtc_client getOwner\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getOwner' command without callback" {
  $tzbtc_client getOwner --dry-run
}

@test "invoking tzbtc-client 'getTokenMetadata' command" {
  $tzbtc_client getTokenMetadata\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getTokenMetadata' command without callback" {
  $tzbtc_client getTokenMetadata --dry-run
}

@test "invoking tzbtc-client 'getRedeemAddress' command" {
  $tzbtc_client getRedeemAddress\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getRedeemAddress' command without callback" {
  $tzbtc_client getRedeemAddress --dry-run
}

@test "invoking tzbtc-client 'getTotalSupply' command" {
  $tzbtc_client getTotalSupply\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getTotalSupply' command without callback" {
  $tzbtc_client getTotalSupply --dry-run
}

@test "invoking tzbtc-client 'getTotalMinted' command" {
  $tzbtc_client getTotalMinted\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getTotalMinted' command without callback" {
  $tzbtc_client getTotalMinted --dry-run
}

@test "invoking tzbtc-client 'getTotalBurned' command" {
  $tzbtc_client getTotalBurned\
    --callback "$callback" --dry-run
}

@test "invoking tzbtc-client 'getTotalBurned' command without callback" {
  $tzbtc_client getTotalBurned --dry-run
}

@test "invoking tzbtc-cliet 'getOperators' command" {
  $tzbtc_client getOperators --dry-run
}

@test "invoking tzbtc-client 'addOperator' command" {
  $tzbtc_client addOperator\
    --operator "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --dry-run
}

@test "invoking tzbtc-client 'removeOperator' command" {
  $tzbtc_client removeOperator\
    --operator "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --dry-run
}

@test "invoking tzbtc-client 'pause' command" {
  $tzbtc_client pause --dry-run
}

@test "invoking tzbtc-client 'unpause' command" {
  $tzbtc_client unpause --dry-run
}

@test "invoking tzbtc-client 'setRedeemAddress' command" {
  $tzbtc_client setRedeemAddress "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --dry-run
}

@test "invoking multisig package creation" {
  $tzbtc_client addOperator --operator "tz1UMD9BcyJsiTrPLQSy1yoYzBhKUry66wRV" --multisig-package package.txt --dry-run
}

@test "invoking multisig package creation for acceptOwnership" {
  $tzbtc_client acceptOwnership --multisig-package package.txt --dry-run
}

@test "deploy contract" {
  $tzbtc_client deployTzbtcContract --owner boba --redeem boba --token-name Kukareq --token-symbol Cococoq --dry-run
}

@test "deploy multisig" {
  $tzbtc_client deployMultisigContract --threshold 2 \
    --public-key edpkvD74whcSw91LUtJex5V16115w4BTwgu3dXHdtNDbbbBRiYEQqP \
    --public-key edpkvXeP2mxHEyF8GSakTy6Fg2G9EsN23eUbjBWtfgTez3b2ZQj3nL \
    --public-key edpkvJq6xRTcQyBpGVLPEcNqRijB2WKZ2M1TYvWhiKjQQPUzYgq9ec \
    --use-custom-errors --dry-run
}

@test "show config" {
  $tzbtc_client config --dry-run
}
