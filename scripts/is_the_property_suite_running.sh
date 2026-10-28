#!/usr/bin/env bash
# Do the wire property tests run, and do they run every property this tree defines?
#
# macula#151. `rebar3 proper` finds its modules by name — a file under test/ whose module starts with prop_, holding
# functions whose names start with prop_ — and exits 0 when it finds none. So a renamed file, a module proper does not
# look in, or a property renamed out of its prefix turns this check green while testing nothing. That is the failure
# this script exists for, and it is why the count is DERIVED from the source rather than restated here: the expected
# number is the number of `prop_*()` definitions under test/*.erl, and every one of them must have run.
#
# Usage:
#   scripts/is_the_property_suite_running.sh [DIR]
#     DIR  where the property modules are, default test
set -euo pipefail

DIR="${1:-test}"

# The properties this tree defines: one `prop_name() ->` per property, at the start of a line, in ANY module under the
# directory — not only the prop_*.erl ones. A property in a module proper does not look in is a test nobody runs, and
# counting only the files proper already agrees to look at would let a rename out of its prefix pass this check.
#
# Counted per FILE:NAME pair, not deduplicated by name (`grep -h`): two modules may each define a property of the same
# name, both of them run, and a name-only count would read that as one property too many having run.
#
# `|| true' because grep exits 1 on no match, and under `set -e' that would end the script at the assignment instead of
# at the check below, which is where the reason belongs.
expected=$( { grep -oE '^prop_[a-z0-9_]+\(' "$DIR"/*.erl || true; } | wc -l)

if [ "$expected" -eq 0 ]; then
    echo "no prop_*() definitions under $DIR/*.erl: there is no property suite here to run"
    exit 1
fi

log=$(mktemp)
trap 'rm -f "$log"' EXIT

set +e
rebar3 proper >"$log" 2>&1
status=$?
set -e
cat "$log"

# The summary line: "19/19 properties passed", or "16/19 properties passed, 3 failed".
summary=$(grep -E '^[0-9]+/[0-9]+ properties passed' "$log" | tail -1 || true)
if [ -z "$summary" ]; then
    echo "FAIL: rebar3 proper printed no property summary, so nothing can be said about what ran"
    exit 1
fi

ran=$(printf '%s' "$summary" | sed -E 's#^([0-9]+)/.*#\1#')

if [ "$status" -ne 0 ]; then
    echo "FAIL: the property suite is red: $summary"
    exit "$status"
fi

if [ "$ran" -ne "$expected" ]; then
    echo "FAIL: $expected properties are defined under $DIR/*.erl but $ran ran: a property or a module is not being discovered"
    exit 1
fi

echo "OK: all $expected properties ran, and the suite is green"
