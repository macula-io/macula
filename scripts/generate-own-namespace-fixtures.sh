#!/usr/bin/env bash
# Regenerate test/fixtures/own_namespace/ with the compiled tree (rebar3 compile first).
set -euo pipefail
cd "$(dirname "$0")/.."
exec erl -noshell -pa _build/default/lib/*/ebin -eval "$(cat scripts/generate_own_namespace_fixtures.erl)"
