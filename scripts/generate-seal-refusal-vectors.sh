#!/usr/bin/env bash
# Regenerate test/vectors/e2e_seal_v1_refusals.json with the compiled tree (rebar3 compile first). Signing is
# randomized, so a run signs new frames with the same shape; the committed file is the vector.
set -euo pipefail
cd "$(dirname "$0")/.."
exec erl -noshell -pa _build/default/lib/*/ebin -eval "$(cat scripts/generate_seal_refusal_vectors.erl)"
