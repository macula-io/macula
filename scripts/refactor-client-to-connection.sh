#!/usr/bin/env bash
set -e

cd "$(dirname "$0")/.."

echo "Refactoring macula_client_client → macula_connection"
echo ""

# Step 1: Rename files
echo "1. Renaming files..."
mv src/macula_client_client.erl src/macula_connection.erl
mv test/macula_client_client_tests.erl test/macula_connection_tests.erl
echo "   ✓ Files renamed"

# Step 2: Update module declarations and all references
echo "2. Updating module names in source files..."

# Update the main module file
sed -i 's/-module(macula_client_client)/-module(macula_connection)/g' src/macula_connection.erl
sed -i 's/macula_client_client/macula_connection/g' src/macula_connection.erl

# Update test module file
sed -i 's/-module(macula_client_client_tests)/-module(macula_connection_tests)/g' test/macula_connection_tests.erl
sed -i 's/macula_client_client/macula_connection/g' test/macula_connection_tests.erl

# Update references in macula_client.erl
sed -i 's/macula_client_client/macula_connection/g' src/macula_client.erl

# Update references in other test files
sed -i 's/macula_client_client/macula_connection/g' test/macula_client_rpc_tests.erl

echo "   ✓ Module names updated"

# Step 3: Update documentation comments
echo "3. Updating documentation..."
sed -i 's/Macula SDK client connection/Macula SDK connection/g' src/macula_connection.erl
echo "   ✓ Documentation updated"

echo ""
echo "✓ Refactoring complete!"
echo ""
echo "Changed:"
echo "  - src/macula_client_client.erl → src/macula_connection.erl"
echo "  - test/macula_client_client_tests.erl → test/macula_connection_tests.erl"
echo "  - Updated all references in 4 files"
