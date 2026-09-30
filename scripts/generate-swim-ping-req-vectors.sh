#!/usr/bin/env bash
# Regenerate test/vectors/swim_ping_req_v1.json with the compiled tree (rebar3 compile first). frame_id and sent_at_ms
# differ per run, so a run writes other bytes with the same readings; the committed file is the vector.
set -euo pipefail
cd "$(dirname "$0")/.."
exec erl -noshell -pa _build/default/lib/*/ebin -eval "$(cat scripts/generate_swim_ping_req_vectors.erl)"
