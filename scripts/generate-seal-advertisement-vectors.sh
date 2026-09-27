#!/usr/bin/env bash
# Regenerate test/vectors/e2e_seal_v1_advertisements.json with the compiled tree (rebar3 compile first). Signing is
# randomized, so a run signs new records with the same verdicts; the committed file is the vector.
set -euo pipefail
cd "$(dirname "$0")/.."
exec erl -noshell -pa _build/default/lib/*/ebin -eval "$(cat scripts/generate_seal_advertisement_vectors.erl)"
