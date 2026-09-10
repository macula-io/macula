#!/usr/bin/env bash
# Publish macula to hex.pm from exactly the pushed release tag.
#
# Tests gate the release commit on main, not the publish, so none run here.
#
# `rebar3 hex publish' packages the WORKING TREE, not a tag. This script
# refuses first, before anything slow, unless the checkout is the clean
# release tag v<vsn> and origin has that tag at the same commit
# (scripts/is_checkout_publishable.sh). After publishing it checks that hex
# serves the tagged code (scripts/is_hex_serving_what_git_says.sh).
#
# Usage: scripts/publish-to-hex.sh   (from a clean checkout of the release tag)
set -eo pipefail

cd "$(dirname "$0")/.."

bash scripts/is_checkout_publishable.sh

MACULA_VERSION="$(sed -n 's/.*{vsn, *"\([^"]*\)"}.*/\1/p' src/macula.app.src | head -n 1)"
CHECK_ATTEMPTS=5
CHECK_RETRY_SECONDS=30

# Source secrets
[ -f "$HOME/.config/zshrc/01-secrets" ] && source "$HOME/.config/zshrc/01-secrets"

echo "Publishing macula $MACULA_VERSION to hex.pm..."
echo ""

echo "Building hex package..."
rebar3 hex build
echo ""

echo "Publishing..."
rebar3 hex publish --yes
echo ""

echo "Checking that hex serves the code tagged v$MACULA_VERSION..."
for attempt in $(seq 1 "$CHECK_ATTEMPTS"); do
    if bash scripts/is_hex_serving_what_git_says.sh "$MACULA_VERSION"; then
        echo "Done! Published macula $MACULA_VERSION"
        exit 0
    fi
    echo "Not confirmed yet (attempt $attempt of $CHECK_ATTEMPTS), retrying in ${CHECK_RETRY_SECONDS}s"
    sleep "$CHECK_RETRY_SECONDS"
done
echo "hex.pm does not serve the code tagged v$MACULA_VERSION" >&2
exit 1
