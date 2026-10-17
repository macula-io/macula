#!/usr/bin/env bash
# Checks the committed wire 1.0 vectors against two fresh generations of them (scripts/wire_1_0_vectors_drift.erl).
# Regeneration is randomized, so the two runs are what tells the checker which parts are per-run material; the
# committed files are never written to. Needs the tree compiled (rebar3 compile first), because the generator
# reads priv/vectors' own modules to build what it writes.
set -euo pipefail
cd "$(dirname "$0")/.."
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
first="$work/first"
second="$work/second"
mkdir -p "$first" "$second"
WIRE_1_0_VECTOR_DIR="$first" bash scripts/generate-wire-1-0-vectors.sh
WIRE_1_0_VECTOR_DIR="$second" bash scripts/generate-wire-1-0-vectors.sh
erlc -o "$work" scripts/wire_1_0_vectors_drift.erl
exec erl -noshell -pa _build/default/lib/*/ebin -pa "$work" \
     -eval "wire_1_0_vectors_drift:main([\"priv/vectors/wire_1_0\", \"$first\", \"$second\"])."
