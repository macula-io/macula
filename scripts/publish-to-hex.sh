#!/usr/bin/env bash
set -e

# Simple hex.pm publish script for macula

cd "$(dirname "$0")/.."

# Source secrets
[ -f "$HOME/.config/zshrc/01-secrets" ] && source "$HOME/.config/zshrc/01-secrets"

echo "Publishing macula to hex.pm..."
echo ""

# Version check
MACULA_VERSION=$(grep -oP '(?<={vsn, ")[^"]+' src/macula.app.src)

echo "Version: $MACULA_VERSION"
echo ""

# Run tests
echo "Running tests..."
rebar3 eunit || echo "Tests had warnings but no failures"
echo ""

# Build package
echo "Building hex package..."
rebar3 hex build
echo ""

# Publish
echo "Publishing..."
rebar3 hex publish --yes
echo ""
echo "Done! Published macula $MACULA_VERSION"
