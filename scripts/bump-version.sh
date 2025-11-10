#!/usr/bin/env bash

set -euo pipefail

# Bump version script for Macula project
# Updates version in both macula.app.src and macula_client.app.src

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Files to update
MACULA_APP_SRC="$PROJECT_ROOT/apps/macula/src/macula.app.src"
MACULA_CLIENT_APP_SRC="$PROJECT_ROOT/apps/macula_client/src/macula_client.app.src"

# Check if version argument is provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 <new-version>"
    echo ""
    echo "Examples:"
    echo "  $0 0.4.2"
    echo "  $0 0.5.0"
    echo "  $0 1.0.0"
    exit 1
fi

NEW_VERSION="$1"

# Validate version format (basic semver check)
if ! [[ "$NEW_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo "Error: Version must be in semver format (e.g., 0.4.2)"
    exit 1
fi

# Get current versions
CURRENT_MACULA_VERSION=$(grep -oP '(?<={vsn, ")[^"]+' "$MACULA_APP_SRC" || echo "unknown")
CURRENT_CLIENT_VERSION=$(grep -oP '(?<={vsn, ")[^"]+' "$MACULA_CLIENT_APP_SRC" || echo "unknown")

echo "Current versions:"
echo "  macula:        $CURRENT_MACULA_VERSION"
echo "  macula_client: $CURRENT_CLIENT_VERSION"
echo ""
echo "Bumping to: $NEW_VERSION"
echo ""

# Update macula.app.src
sed -i "s/{vsn, \".*\"}/{vsn, \"$NEW_VERSION\"}/" "$MACULA_APP_SRC"
echo "✓ Updated $MACULA_APP_SRC"

# Update macula_client.app.src
sed -i "s/{vsn, \".*\"}/{vsn, \"$NEW_VERSION\"}/" "$MACULA_CLIENT_APP_SRC"
echo "✓ Updated $MACULA_CLIENT_APP_SRC"

echo ""
echo "Version bump complete!"
echo ""
echo "Next steps:"
echo "  1. Review changes: git diff"
echo "  2. Commit: git add apps/*/src/*.app.src && git commit -m 'chore: Bump version to $NEW_VERSION'"
echo "  3. Build hex package: rebar3 hex build --app macula"
echo "  4. Publish: rebar3 hex publish --app macula"
