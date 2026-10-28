#!/usr/bin/env bash
# Fails when the merged line coverage of the eunit and ct suites is below the floor (macula#153).
#
# Each suite is run by its own step in the `coverage' matrix entry, so they arrive as two cover export files and
# neither on its own is the suite's coverage: a module only the fleet suites reach reads as dead in the eunit file.
# scripts/line_coverage.erl merges them and applies the floor.
#
# Usage: bash scripts/is_line_coverage_at_least_the_floor.sh <eunit.coverdata> <ct.coverdata> [...]
#
# THE FLOOR ONLY GOES UP. It is main's measured line coverage, rounded down to a whole percent: on 2026-10-28 that
# was just under 83%, so 82. Neither count is a property of the tree - the covered count moved by up to three lines
# across consecutive runs of one commit, and a rebase onto a new base moves both - so this records the level and the
# job's own output is the authority for the counts. A whole percent here is about 131 lines, so a whole-percent floor
# cannot sit closer than that, and 82 leaves about a hundred covered lines of headroom. A
# change that adds more than that many untested lines is red here. A change that raises coverage raises this number in
# the same commit. Lowering it is a reviewed decision about which lines stop being tested, stated in the commit that
# does it, and never something done to turn a branch green. CONTRIBUTING, Testing, says the same where someone
# writing a test will read it.
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
