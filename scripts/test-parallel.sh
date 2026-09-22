#!/usr/bin/env bash
# Run the eunit suite in parallel shards on one machine.
#
# WHY: `rebar3 eunit` is serial and takes about 6 minutes here. CI can be made
# parallel by giving each check its own runner, but that does nothing for the
# run you do before pushing, which is where the waiting actually happens.
#
# IT IS ALSO AN INSTRUMENT, NOT ONLY A SPEED TOOL. A test can be green for a
# reason that has nothing to do with the code under test, and no amount of
# reading it will show that. Two things expose the family: a MUTATION catches a
# test whose expected value happens to equal what a bug produces; a DIFFERENT
# SCHEDULE catches tests that depend on the order or timing the suite happens to
# give them. This is the second of those. On its first honest run it found a
# registry left registered, four assertions that needed a module loaded, and
# three tests that were running twice. To judge whether a test is honest, run it
# alone (`rebar3 eunit --module=X') and run it under these shards.
#
# WHAT IT DOES NOT DO: change which tests run. The module list is taken from
# the compiled beams, not from a filename pattern, and the script refuses to
# run unless every module it found lands in exactly one shard. See "The list"
# below for why that matters.
#
#   scripts/test-parallel.sh            # shards = 3
#   SHARDS=6 scripts/test-parallel.sh   # more shards
#
# ⚠ STALE BEAMS, inherited from `rebar3 eunit' and NOT made worse here. rebar3
# recompiles a source only when it is STRICTLY newer than its beam, at one-second
# granularity, so a source edited after its beam was written inside the same
# second is not rebuilt and the run silently tests the old code. This compiles
# once, exactly as `rebar3 eunit' does, and the shards then find a warm tree and
# compile nothing, so sharing one build directory adds no path to it.
#
# There is deliberately no check for it. A tie between source and beam mtimes is
# what an ORDINARY correct edit-and-compile produces, so refusing on a tie would
# fire mostly on correct trees, and a guard that cries wolf on the tool people run
# before pushing is one they learn to switch off. What separates the two cases is
# content, not time, and a hash manifest beside the beams would make this a build
# system. If you are mutating code and restoring it, `rm' the beam; that belongs
# in the mutation procedure, where it is a step, not here, where it is a guess.
#
# Exit status is 0 only if every shard passed.

set -uo pipefail
cd "$(dirname "$0")/.."

SHARDS="${SHARDS:-3}"
OUT="${TMPDIR:-/tmp}/macula-test-parallel.$$"
mkdir -p "$OUT"
# A failing run keeps its shard logs: the summary names the module but the
# reason is in the log, and deleting it means running the whole thing again to
# read what it already knew.
cleanup_out() { if [ "${KEEP:-0}" = 1 ]; then echo "shard logs kept in $OUT"; else rm -rf "$OUT"; fi; }
trap cleanup_out EXIT

# ---------------------------------------------------------------------------
# Compile ONCE, serially, before any shard starts.
#
# Shards share one _build. That is safe when there is nothing left to compile,
# and a race when there is: several rebar3 processes would write the same beams
# at the same time. Doing it here means each shard finds the tree warm and
# compiles nothing.
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# The OTP on PATH, checked before anything else.
#
# This script inherits whatever `erl' is first on PATH. On a box whose default
# is a different major, the run dies inside a DEPENDENCY with no mention of
# versions: under OTP 29 it is meck, on `'catch ...' is deprecated', and the
# only thing this script would otherwise print is "compile failed". That is two
# confusing runs for the next person, so it is named here instead.
#
# The wanted major comes from .tool-versions, not a constant, so this does not
# need editing when the team moves major.
# ---------------------------------------------------------------------------
WANT_OTP=$(awk '/^erlang /{split($2, v, "."); print v[1]}' .tool-versions 2>/dev/null)
HAVE_OTP=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null)
if [ -n "$WANT_OTP" ] && [ -n "$HAVE_OTP" ] && [ "$WANT_OTP" != "$HAVE_OTP" ]; then
    echo "OTP $HAVE_OTP is on PATH; .tool-versions asks for $WANT_OTP." >&2
    echo "A run under another major is drift, not a test result. Put the right erl first:" >&2
    echo "  PATH=<otp-$WANT_OTP>/bin:\$PATH $0" >&2
    exit 1
fi

echo "==> compiling once (shards share this build)"
if ! rebar3 as test compile > "$OUT/compile.log" 2>&1; then
    echo "compile failed:"; tail -30 "$OUT/compile.log"; exit 1
fi

# ---------------------------------------------------------------------------
# The list.
#
# Taken from the beams, by asking each module whether it exports `test/0`,
# which is what including eunit.hrl generates and therefore exactly what eunit
# would run. NOT from `ls test/*.erl`: 15 modules under src/ carry their tests
# inline behind -ifdef(TEST), and a dozen modules under test/ are helpers with
# no tests at all. A filename-based list would quietly run FEWER tests than
# `rebar3 eunit` does, which is the one outcome this script must not have.
# ---------------------------------------------------------------------------
# Both directories: rebar3's test profile puts application modules in ebin/ and
# the test modules in test/. Looking only in ebin/ finds the 15 src modules
# with inline tests and none of the 168 test modules, which is how this first
# refused to run at all.
mapfile -t MODULES < <(
    erl -noshell -pa _build/test/lib/*/ebin -pa _build/test/lib/*/test -eval '
        Beams = filelib:wildcard("_build/test/lib/*/ebin/*.beam")
                ++ filelib:wildcard("_build/test/lib/*/test/*.beam"),
        Mods = [list_to_atom(filename:basename(B, ".beam")) || B <- Beams],
        Testable = [M || M <- Mods,
                         code:ensure_loaded(M) =:= {module, M},
                         erlang:function_exported(M, test, 0)],
        [io:format("~s~n", [M]) || M <- lists:usort(Testable)],
        halt().' 2>/dev/null
)

if [ "${#MODULES[@]}" -eq 0 ]; then
    echo "found no modules exporting test/0; refusing to report a green run" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# A bad LIST is the failure the shard check cannot see.
#
# The split check below proves every module we found lands in exactly one
# shard. It cannot prove we found every module, and that is where coverage is
# actually lost: a module eunit would run, missing from the enumeration, is
# simply never run and every shard still goes green.
#
# `test/0' is exported by including eunit.hrl. A module that defines `*_test'
# or `*_test_' functions WITHOUT that include has tests eunit finds and this
# enumeration would miss, so it is named here and the run refuses.
#
# Deliberately not a comparison against `rebar3 eunit''s own total: that total
# counts three tests twice (eunit pairs a module with its `<module>_tests'
# sibling, and test/ held both), so such a check could never pass and would be
# switched off, leaving nothing checked at all.
mapfile -t UNREACHED < <(
    erl -noshell -pa _build/test/lib/*/ebin -pa _build/test/lib/*/test -eval '
        Beams = filelib:wildcard("_build/test/lib/*/ebin/*.beam")
                ++ filelib:wildcard("_build/test/lib/*/test/*.beam"),
        Mods = lists:usort([list_to_atom(filename:basename(B, ".beam")) || B <- Beams]),
        Has = fun(M) ->
                  lists:any(fun({F, 0}) ->
                                    L = lists:reverse(atom_to_list(F)),
                                    lists:prefix("tset_", L) orelse lists:prefix("_tset_", L);
                               (_) -> false
                            end, M:module_info(exports))
              end,
        Miss = [M || M <- Mods,
                     code:ensure_loaded(M) =:= {module, M},
                     not erlang:function_exported(M, test, 0),
                     Has(M)],
        [io:format("~s~n", [M]) || M <- Miss],
        halt().' 2>/dev/null
)
if [ "${#UNREACHED[@]}" -gt 0 ]; then
    echo "these modules define tests but export no test/0, so this runner would skip them:" >&2
    printf '  %s\n' "${UNREACHED[@]}" >&2
    echo "add -include_lib(\"eunit/include/eunit.hrl\") to them, or teach this script to find them" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Deal modules round-robin. Not balanced by timing on purpose: a timing table
# checked in here would go stale the moment a test is added, and an unbalanced
# shard is a slower run, while a missing module is a false green.
# ---------------------------------------------------------------------------
declare -a SHARD_LIST
for i in $(seq 0 $((SHARDS - 1))); do SHARD_LIST[$i]=""; done
idx=0
for m in "${MODULES[@]}"; do
    s=$((idx % SHARDS))
    if [ -z "${SHARD_LIST[$s]}" ]; then SHARD_LIST[$s]="$m"; else SHARD_LIST[$s]="${SHARD_LIST[$s]},$m"; fi
    idx=$((idx + 1))
done

# Every module in exactly one shard, or we do not run. This is the check that
# makes a green here mean the same as a green from `rebar3 eunit`.
assigned=0
for i in $(seq 0 $((SHARDS - 1))); do
    [ -z "${SHARD_LIST[$i]}" ] && continue
    n=$(printf '%s' "${SHARD_LIST[$i]}" | tr ',' '\n' | grep -c .)
    assigned=$((assigned + n))
done
if [ "$assigned" -ne "${#MODULES[@]}" ]; then
    echo "shard split lost modules: ${#MODULES[@]} found, $assigned assigned" >&2
    exit 1
fi
echo "==> ${#MODULES[@]} test modules across $SHARDS shards"

# ---------------------------------------------------------------------------
# Run them.
# ---------------------------------------------------------------------------
start=$SECONDS
pids=()
for i in $(seq 0 $((SHARDS - 1))); do
    [ -z "${SHARD_LIST[$i]}" ] && continue
    ( s=$SECONDS
      rebar3 eunit --module="${SHARD_LIST[$i]}" > "$OUT/shard$i.log" 2>&1
      echo $? > "$OUT/shard$i.rc"
      echo $((SECONDS - s)) > "$OUT/shard$i.time" ) &
    pids+=($!)
done
for p in "${pids[@]}"; do wait "$p"; done
elapsed=$((SECONDS - start))

# ---------------------------------------------------------------------------
# One result.
# ---------------------------------------------------------------------------
total=0; failed=0
echo
for i in $(seq 0 $((SHARDS - 1))); do
    [ -f "$OUT/shard$i.rc" ] || continue
    rc=$(cat "$OUT/shard$i.rc"); t=$(cat "$OUT/shard$i.time")
    n=$(grep -oE 'All ([0-9]+) tests passed|([0-9]+) tests, ' "$OUT/shard$i.log" | grep -oE '[0-9]+' | head -1)
    n=${n:-0}
    total=$((total + n))
    if [ "$rc" -eq 0 ]; then
        printf '  shard %d: %4d tests  %3ds  ok\n' "$i" "$n" "$t"
    else
        failed=1
        printf '  shard %d: %4d tests  %3ds  FAILED\n' "$i" "$n" "$t"
        grep -E '\*\*[^*]|Failed:|error|\.erl:[0-9]+' "$OUT/shard$i.log" | head -15 | sed 's/^/      /'
    fi
done

echo
if [ "$failed" -eq 0 ]; then
    echo "==> $total tests passed in ${elapsed}s across $SHARDS shards"
else
    KEEP=1
    echo "==> FAILED after ${elapsed}s. $total tests counted; see the shard output above."
fi
exit "$failed"
