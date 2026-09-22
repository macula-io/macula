#!/usr/bin/env bash
# Does test-parallel.sh's warm-up actually leave the shards nothing to compile?
#
# WHY THIS EXISTS. test-parallel.sh's safety argument is that it compiles once
# before fanning out, because the shards share one _build and compiling into it
# concurrently is a race. That argument was FALSE for its first day: the warm-up
# ran `rebar3 as test compile' while the shards run `rebar3 eunit', and eunit
# rebuilds what `as test compile' produces, so on a fresh checkout all three
# shards rebuilt the same files at once. It failed as erlc losing a rename
# (`failed to rename X.bea# to X.beam') for one person and as `Module not found
# in project' for another, and it was invisible on a warm machine.
#
# So if you change the warm-up, check it here rather than reasoning about it.
#
# ⚠ WHAT NOT TO MEASURE. rebar3 prints "===> Compiling macula" whether or not it
# compiles anything. Counting that line says the shards are working when they
# are not, and it reads identically for a correct and a broken warm-up. That is
# how the original claim passed its own check. What distinguishes them is
# whether beams are WRITTEN, so this compares their modification times.
#
# ⚠ DESTRUCTIVE, on purpose. A warm tree cannot answer the question: everything
# is already built, so any warm-up looks sufficient. It deletes the test
# profile's beams to recreate the only state where the fault appears, a tree
# eunit has not built. They are rebuilt by the run.
#
#   bash scripts/is-the-build-warm.sh

set -uo pipefail
cd "$(dirname "$0")/.."

RUNNER=scripts/test-parallel.sh

# One source of truth for the warm-up: read it out of the runner rather than
# keeping a copy here that drifts from it. Refuse rather than guess.
# Comment lines are stripped FIRST. Without that this read the phrase
# `rebar3 as test compile' out of the comment explaining the old bug, reported
# on a command the runner does not run, and answered NOT WARM for the broken and
# the fixed warm-up alike: a check that cannot tell them apart, which is the
# fault it exists to catch.
WARMUP=$(grep -vE '^[[:space:]]*#' "$RUNNER" \
         | grep -oE 'rebar3 eunit --module=[a-z_0-9]+|rebar3 as test compile' \
         | head -1)
if [ -z "$WARMUP" ]; then
    echo "could not find the warm-up command in $RUNNER; it has changed shape." >&2
    echo "Update the pattern here rather than letting this check quietly pass." >&2
    exit 1
fi
echo "==> warm-up under test, read from $RUNNER:"
echo "      $WARMUP"

BEAMS=_build/test/lib/macula
snapshot() { find "$BEAMS" -name '*.beam' -printf '%T@ %p\n' 2>/dev/null | sort | md5sum | awk '{print $1}'; }

echo "==> emptying the test profile's beams (the state where the fault appears)"
find "$BEAMS/test" -name '*.beam' -delete 2>/dev/null
find "$BEAMS/ebin" -name '*.beam' -delete 2>/dev/null

echo "==> running the warm-up"
if ! $WARMUP > /tmp/is-the-build-warm.$$.log 2>&1; then
    echo "the warm-up itself failed:" >&2; tail -20 /tmp/is-the-build-warm.$$.log >&2
    rm -f /tmp/is-the-build-warm.$$.log; exit 1
fi
before=$(snapshot)

echo "==> running one test module, which is what a shard does"
rebar3 eunit --module=macula_mri_tests > /tmp/is-the-build-warm.$$.log 2>&1
after=$(snapshot)
rm -f /tmp/is-the-build-warm.$$.log

echo
if [ "$before" = "$after" ]; then
    echo "WARM: the run rewrote no beams, so the shards compile nothing and cannot race."
    exit 0
fi
echo "NOT WARM: the run rewrote beams, so every shard would redo this work at once," >&2
echo "into the one shared _build. That is the race. The warm-up must use the same" >&2
echo "command the shards use." >&2
exit 1
