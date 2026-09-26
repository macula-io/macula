#!/usr/bin/env bash
# Regenerate test/vectors/ucan_v1.json with the compiled tree (rebar3 compile first). ML-DSA-87 and RSA-PSS signing
# are randomized, so a run mints new tokens with the same verdicts; the committed file is the vector.
set -euo pipefail
cd "$(dirname "$0")/.."
exec erl -noshell -pa _build/default/lib/*/ebin -eval "$(cat scripts/generate_ucan_vectors.erl)"
