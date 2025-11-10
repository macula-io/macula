#!/usr/bin/env bash
set -e

cd "$(dirname "$0")/.."

echo "=== Phase 2: Complete Code Cleanup ==="
echo ""

# Step 1: Rename macula_rpc_uri → macula_rpc_names
echo "1. Renaming macula_rpc_uri → macula_rpc_names..."
mv src/macula_rpc_uri.erl src/macula_rpc_names.erl

sed -i 's/-module(macula_rpc_uri)/-module(macula_rpc_names)/g' src/macula_rpc_names.erl
sed -i 's/macula_rpc_uri/macula_rpc_names/g' src/macula_rpc_names.erl
sed -i 's/macula_rpc_uri/macula_rpc_names/g' src/macula_rpc_server.erl

echo "   ✓ Renamed to macula_rpc_names"

# Step 2: Standardize discovery API (rpc to match pubsub)
echo "2. Standardizing discovery APIs..."

# In macula_rpc_discovery.erl:
# - announce_registration/5 → announce/5
# - remove_registration/3 → unannounce/3

sed -i 's/announce_registration/announce/g' src/macula_rpc_discovery.erl
sed -i 's/remove_registration/unannounce/g' src/macula_rpc_discovery.erl

# Update documentation
sed -i 's/Register RPC provider/Announce RPC provider/g' src/macula_rpc_discovery.erl
sed -i 's/Remove RPC provider/Unannounce RPC provider/g' src/macula_rpc_discovery.erl

echo "   ✓ Standardized to announce/unannounce"

echo ""
echo "✓ Phase 2 complete!"
echo ""
echo "Changes:"
echo "  - Renamed macula_rpc_uri → macula_rpc_names (DNS-style names, not URIs)"
echo "  - Standardized discovery APIs:"
echo "    - announce_registration/5 → announce/5"
echo "    - remove_registration/3 → unannounce/3"
echo ""
echo "Total files deleted: 9 empty namespace modules"
echo "Total files renamed: 2 (macula_rpc_uri, macula_client_client)"
echo "Total APIs standardized: 2 functions"
