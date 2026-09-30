#!/usr/bin/env bash
# Regenerate test/vectors/station_endpoint_v1.json with the compiled tree (rebar3 compile first). Signing is
# randomized, so a run signs new records with the same readings; the committed file is the vector.
set -euo pipefail
cd "$(dirname "$0")/.."
exec erl -noshell -pa _build/default/lib/*/ebin -eval "$(cat scripts/generate_station_endpoint_vectors.erl)"
