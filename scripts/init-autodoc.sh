#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
# SPDX-License-Identifier: LicenseRef-MPL-2.0

# This script makes a stub gist for given user.
# Provide user credentials via 'AUTH_TOKEN' variable.
#
# Dependencies:
# * curl

set -e

echo "Starting script"

if [[ $AUTH_TOKEN == "" ]]; then
    echo "Pass user_id:oauth_token token via AUTH_TOKEN env variable"
    exit 1
fi

post_data=$(cat <<EOF
{
    "description": "Automatically generated documentation for  TZBTC contract",
    "files": {
        "Documentation.md": {
            "content": "Stub"
        }
    },
    "public": "false"
}
EOF
)

response=$(echo "$post_data" | curl -X POST https://api.github.com/gists -u "$AUTH_TOKEN" -d @-)

if [ -x "$(command -v jq)" ]; then
    echo "$response" | jq -r '.html_url'
else
    echo "$response"
fi
