#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
# SPDX-License-Identifier: LicenseRef-MPL-2.0

# This script generates and publishes contract autodoc to gist.
#
# How to use:
# 1. Manually create an empty gist or call `init-autodoc.sh`,
#    copy ID of created gist.
# 2. Call this script.
#
# Arguments:
# 1. Gist ID, it will be updated in-place.
# Env: AUTH_TOKEN variable should be <user_id>:<oauth_token> of gist author.
#      You can create oauth token in your github profile:
#      https://github.com/settings/tokens.
#
# Dependencies:
# * curl

set -e

if [[ $1 == "" ]]; then
    echo "Pass gist ID as the first argument"
    exit 1
fi
gist_id=$1

if [[ $AUTH_TOKEN == "" ]]; then
    echo "Pass user_id:oauth_token token via AUTH_TOKEN env variable"
    exit 1
fi

doc=$(stack exec -- tzbtc printContractDoc)
post_data=$(cat <<EOF
{
    "description": "Automatically generated documentation for TZBTC contract",
    "files": {
        "Documentation.md": {
            "content": "$(echo "$doc" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g'  -e 's/$/\\n/')"
        }
    }
}
EOF
)

echo "$post_data" | curl -X PATCH https://api.github.com/gists/$gist_id -u $AUTH_TOKEN -d @-
echo "Documentation updated: https://gist.github.com/$gist_id"
