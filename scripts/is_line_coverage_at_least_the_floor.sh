#!/usr/bin/env bash
# Fails when the merged line coverage of the eunit and ct suites is below the floor (macula#153).
#
# The two suites run in separate CI jobs, so they arrive as two cover export files and neither on its own is the
# suite's coverage: a module only the fleet suites reach reads as dead in the eunit file. scripts/line_coverage.erl
# merges them and applies the floor.
#
# Usage: bash scripts/is_line_coverage_at_least_the_floor.sh <eunit.coverdata> <ct.coverdata> [...]
#
# THE FLOOR ONLY GOES UP. It is main's measured line coverage, rounded down to a whole percent: 82.08% over
# 125 modules, measured in the pinned image on 1de6a799 (2026-10-28), so 82. A change that raises coverage raises
# this number in the same commit. Lowering it is a reviewed decision about which lines stop being tested, stated
# in the commit that does it, and never something done to turn a branch green. CONTRIBUTING, Testing, says the
# same where someone writing a test will read it.
set -euo pipefail
cd "$(dirname "$0")/.."

FLOOR=82

if [ "$#" -eq 0 ]; then
    echo "usage: bash scripts/is_line_coverage_at_least_the_floor.sh <export.coverdata> [...]" >&2
    echo "  no export file given, so there is nothing to measure" >&2
    exit 2
fi

for export_file in "$@"; do
    if [ ! -f "$export_file" ]; then
        echo "no cover export file at ${export_file}: did that suite run with --cover?" >&2
        exit 2
    fi
done

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# One path per line, so no path is ever spliced into an -eval string.
printf '%s\n' "$@" > "${work}/exports.txt"

erlc -o "$work" scripts/line_coverage.erl
erl -noshell -pa "$work" \
    -eval "line_coverage:main([\"${FLOOR}\", \"${work}/exports.txt\"])."
