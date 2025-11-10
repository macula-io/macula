#!/bin/bash
# Refactor macula_sdk to macula_client
# This script automates the entire refactoring process

set -e  # Exit on error
set -u  # Exit on undefined variable

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MACULA_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$MACULA_ROOT"

echo "=== Macula SDK → Client Refactoring Script ==="
echo "Working directory: $MACULA_ROOT"
echo

# Step 1: Rename directory
echo "[1/9] Renaming directory: apps/macula_sdk → apps/macula_client"
git mv apps/macula_sdk apps/macula_client
echo "✓ Directory renamed"
echo

# Step 2: Rename source files
echo "[2/9] Renaming source files"
cd apps/macula_client/src
for file in macula_sdk*.erl; do
    if [ -f "$file" ]; then
        newfile="${file/macula_sdk/macula_client}"
        git mv "$file" "$newfile"
        echo "  Renamed: $file → $newfile"
    fi
done
git mv macula_sdk.app.src macula_client.app.src
echo "  Renamed: macula_sdk.app.src → macula_client.app.src"
cd "$MACULA_ROOT"
echo "✓ Source files renamed"
echo

# Step 3: Rename test files
echo "[3/9] Renaming test files"
cd apps/macula_client/test
for file in macula_sdk*.erl; do
    if [ -f "$file" ]; then
        newfile="${file/macula_sdk/macula_client}"
        git mv "$file" "$newfile"
        echo "  Renamed: $file → $newfile"
    fi
done
cd "$MACULA_ROOT"
echo "✓ Test files renamed"
echo

# Step 4: Update file contents in macula_client
echo "[4/9] Updating file contents in macula_client"
find apps/macula_client -name "*.erl" -type f -exec sed -i 's/macula_sdk/macula_client/g' {} +
find apps/macula_client -name "*.hrl" -type f -exec sed -i 's/macula_sdk/macula_client/g' {} + 2>/dev/null || true
sed -i 's/{application, macula_sdk,/{application, macula_client,/g' apps/macula_client/src/macula_client.app.src
sed -i 's/"Macula HTTP\/3 Mesh SDK"/"Macula HTTP\/3 Mesh Client"/g' apps/macula_client/src/macula_client.app.src
echo "✓ File contents updated"
echo

# Step 5: Update version to 0.4.0
echo "[5/9] Updating version to 0.4.0"
sed -i 's/{vsn, "0\.3\.4"}/{vsn, "0.4.0"}/g' apps/macula_client/src/macula_client.app.src
echo "✓ Version bumped to 0.4.0"
echo

# Step 6: Update umbrella app dependencies and root rebar.config
echo "[6/9] Updating umbrella app dependencies"
sed -i 's/macula_sdk/macula_client/g' apps/macula/src/macula.app.src
sed -i 's/macula_sdk/macula_client/g' rebar.config
echo "✓ Umbrella app and root config updated"
echo

# Step 7: Update documentation
echo "[7/9] Updating documentation"
find . -maxdepth 1 -name "*.md" -type f -exec sed -i 's/macula_sdk/macula_client/g' {} +
find architecture -name "*.md" -type f -exec sed -i 's/macula_sdk/macula_client/g' {} + 2>/dev/null || true
find docs -name "*.md" -type f -exec sed -i 's/macula_sdk/macula_client/g' {} + 2>/dev/null || true
echo "✓ Documentation updated"
echo

# Step 8: Update examples
echo "[8/9] Updating examples"
find examples -name "*.erl" -type f -exec sed -i 's/macula_sdk/macula_client/g' {} + 2>/dev/null || true
find examples -name "*.md" -type f -exec sed -i 's/macula_sdk/macula_client/g' {} + 2>/dev/null || true
echo "✓ Examples updated"
echo

# Step 9: Check for any remaining references
echo "[9/9] Checking for remaining references to macula_sdk"
REMAINING=$(grep -r "macula_sdk" apps/*/src apps/*/include apps/*/test --include="*.erl" --include="*.hrl" 2>/dev/null || true)
if [ -n "$REMAINING" ]; then
    echo "⚠ Warning: Found remaining references to macula_sdk:"
    echo "$REMAINING"
    echo
    echo "These may need manual review."
else
    echo "✓ No remaining references found"
fi
echo

# Summary
echo "=== Refactoring Complete ==="
echo
echo "Changes made:"
echo "  • Directory renamed: apps/macula_sdk → apps/macula_client"
echo "  • All modules renamed: macula_sdk_* → macula_client_*"
echo "  • Application name updated in .app.src"
echo "  • Version bumped: 0.3.4 → 0.4.0"
echo "  • Documentation and examples updated"
echo
echo "Next steps:"
echo "  1. Review changes: git diff --staged"
echo "  2. Test compilation: rebar3 compile"
echo "  3. Run tests: rebar3 eunit"
echo "  4. Commit: git commit -m 'Refactor: Rename macula_sdk to macula_client'"
echo
