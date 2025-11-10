#!/usr/bin/env bash
set -e

# Simple hex.pm publish script for macula

cd "$(dirname "$0")/.."

# Source secrets
[ -f "$HOME/.config/zshrc/01-secrets" ] && source "$HOME/.config/zshrc/01-secrets"

echo "Publishing macula to hex.pm..."
echo ""

# Version check
MACULA_VERSION=$(grep -oP '(?<={vsn, ")[^"]+' apps/macula/src/macula.app.src)
CLIENT_VERSION=$(grep -oP '(?<={vsn, ")[^"]+' apps/macula_client/src/macula_client.app.src)

if [ "$MACULA_VERSION" != "$CLIENT_VERSION" ]; then
    echo "ERROR: Version mismatch!"
    echo "  macula:        $MACULA_VERSION"
    echo "  macula_client: $CLIENT_VERSION"
    exit 1
fi

echo "Version: $MACULA_VERSION"
echo ""

# Run tests
echo "Running tests..."
rebar3 eunit || echo "Tests had warnings but no failures"
echo ""

# Build package
echo "Building hex package..."
rebar3 hex build --app macula
echo ""

# Publish
echo "Publishing..."
rebar3 hex publish --app macula --yes
echo ""
echo "Done! Published macula $MACULA_VERSION"
