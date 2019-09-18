#! /usr/bin/env bash

# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary

set -e

config_path="$HOME/.tzbtc/config.json"
node_url=$(cat $config_path | jq -r '.node_address')
node_port=$(cat $config_path | jq -r '.node_port')
address_alias=$(cat $config_path | jq -r '.user_alias')
our_exe=$(cat $config_path |jq -r '.tzbtc_executable')
tezos=$(cat $config_path | jq -r '.tezos_client_executable')

typeset -a args;

while true;
do
    arg="$1"
    if [[ -z "$arg" ]];
    then
        break
    fi
    case $arg in
        --help|--version|printContract|printAgentContract|printProxyContract|printInitialStorage|parseContractParameter|testScenario|setupClient )
            dry_run_flag=true
            ;;
        injectOperation )
            inject_flag=true
            ;;
    esac
    if [ "$arg" != "--dry-run" ]; then
       args+=("$arg")
    fi
    shift
done

if [ -n "$dry_run_flag" ] || [ -n "$inject_flag" ]
then
    $our_exe "${args[@]}"
else
    res=$($our_exe "${args[@]}")
    signature=$($tezos -A $node_url -P $node_port sign bytes "0x03$res" for $address_alias 2>/dev/null | cut -c 12- | tr -d '[:space:]')
    echo $signature
    op_hash=$($our_exe injectOperation $res $signature)
    echo $op_hash
fi
