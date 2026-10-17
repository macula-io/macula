#!/usr/bin/env bash
# Group distributor scale bench (DESIGN_E2E_SEALED_PUBSUB.md §13 #7): rotation
# CPU and reply bytes at n members, both crypto profiles, with the real
# default membership path (one realm-signed endorsement per member, verified
# on every pull), the link-layer costs §6.3 prices a sealed pull at (signed
# RESULT, KEM decapsulation, UCAN authorize), and publish/open per event.
#
# N1/N2 env override the member counts (defaults 1000 and 10000).
set -euo pipefail

REPO_ROOT="${REPO_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
N1="${N1:-1000}"
N2="${N2:-10000}"
BUILD="${BUILD:-$REPO_ROOT/_build/test}"

[ -d "$BUILD/lib/macula/ebin" ] || {
    echo "no test build at $BUILD; run: rebar3 as test compile" >&2
    exit 1
}

mkdir -p "$BUILD/lib/macula/test"
erlc -o "$BUILD/lib/macula/test" "$REPO_ROOT/test/bench_group_distributor.erl"

echo "commit: $(git -C "$REPO_ROOT" rev-parse --short HEAD 2>/dev/null || echo unknown)"
echo "cpu: $(lscpu | awk -F: '/Model name/ {gsub(/^ +/, "", $2); print $2; exit}')"
erl -noshell -pa "$BUILD/lib/macula/ebin" -pa "$BUILD/lib/macula/test" -pa "$BUILD"/lib/*/ebin \
    -eval "bench_group_distributor:main(${N1}, ${N2}), halt()."
