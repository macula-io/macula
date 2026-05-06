#!/usr/bin/env bash
# Run the macula_e2e_SUITE Common Test suite against a real Macula fleet.
#
# Defaults to https://boot.macula.io:4433. Override with one or more
# comma-separated bootstrap URLs:
#
#   ./scripts/e2e.sh
#   ./scripts/e2e.sh https://station-be-kortrijk.macula.io:4433
#   ./scripts/e2e.sh "https://station-be-brussels.macula.io:4433,https://station-be-ghent.macula.io:4433"
#
# This dev box typically can't reach QUIC/4433 (home ISP blocks
# outbound UDP/4433). To verify against the live fleet, run from a
# beam lab node or a VPS:
#
#   ssh rl@beam00.lab 'cd ~/work/macula && ./scripts/e2e.sh'
#
# When the bootstrap is unreachable, init_per_suite returns
# {skip, fleet_not_reachable, _}; every test case is marked SKIPPED
# (not FAIL) so offline runs do not break CI. Only set
# MACULA_E2E_FAIL_ON_SKIP=1 to make a skip count as a build failure.

set -euo pipefail

cd "$(dirname "$0")/.."

if [ "$#" -gt 0 ]; then
    export MACULA_E2E_BOOTSTRAP="$1"
fi

echo "=== macula E2E ==="
echo "  bootstrap: ${MACULA_E2E_BOOTSTRAP:-https://boot.macula.io:4433 (default)}"
echo

rebar3 ct --suite test/macula_e2e_SUITE
