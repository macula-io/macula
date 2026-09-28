#!/usr/bin/env bash
# Regenerate test/vectors/e2e_seal_v1_group_keys.json with the compiled tree (rebar3 compile first). The payloads are
# deterministic; the UCANs are signed anew each run with the same verdicts, so the committed file is the vector.
set -euo pipefail
cd "$(dirname "$0")/.."
exec erl -noshell -pa _build/default/lib/*/ebin -eval "$(cat scripts/generate_seal_group_keys_vectors.erl)"
