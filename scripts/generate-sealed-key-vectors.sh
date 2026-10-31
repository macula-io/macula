#!/usr/bin/env bash
# Regenerates test/vectors/sealed_key_v1.json, the sealed key file vector macula-go and macula-rust run too
# (docs/design/DESIGN_SEALED_KEY_FILE.md, section 7). Needs the tree compiled (rebar3 compile first), because the
# generator uses macula_node_keys and macula_seal from _build. Key generation is random, so a run makes a new key; the
# committed file is the vector.
set -euo pipefail
cd "$(dirname "$0")/.."
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
erlc -o "$work" scripts/generate_sealed_key_vectors.erl
exec erl -noshell -pa _build/default/lib/*/ebin -pa "$work" \
     -eval "generate_sealed_key_vectors:main()."
