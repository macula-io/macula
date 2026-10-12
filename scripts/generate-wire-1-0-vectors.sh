#!/usr/bin/env bash
# Regenerate priv/vectors/wire_1_0/*.json with the compiled tree (rebar3 compile first). Signing is randomized, so a
# run signs new bytes with the same keys and expectations; the committed files are the vectors.
set -euo pipefail
cd "$(dirname "$0")/.."
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
erlc -o "$out" scripts/wire_1_0_vectors.erl scripts/wire_1_0_frame_specs.erl
exec erl -noshell -pa _build/default/lib/*/ebin -pa "$out" -eval 'wire_1_0_vectors:main().'
