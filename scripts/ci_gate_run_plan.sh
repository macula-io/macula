#!/usr/bin/env bash
# Run the plan ci_gate_plan.sh derived, in the workflows' own order, stopping at the first step that refuses
# (macula#163).
#
#   scripts/ci_gate_run_plan.sh <plan dir>
#
# Each step is the workflow's own `run:` text with the YAML block indentation removed, so what runs here is what CI
# runs. The first refusal ends the gate and is the step's own exit status, named, because "the gate is red" without
# saying where sends a member back to CI to find out.
set -uo pipefail

WORK="${1:?usage: ci_gate_run_plan.sh <plan dir>}"
PLAN="$WORK/plan.tsv"
[ -f "$PLAN" ] || { echo "REFUSED: no plan at $PLAN" >&2; exit 2; }

# The reusable doc-snippets workflow writes its check into $RUNNER_TEMP and runs it from there, the same way it does in
# CI, so the directory is made here rather than assumed.
export RUNNER_TEMP="$WORK/runner-temp"
mkdir -p "$RUNNER_TEMP"

ran=0
while IFS=$'\t' read -r order job name check status reason script; do
    [ "$status" = "run" ] || continue
    ran=$((ran + 1))
    printf '\n== step %s: %s [%s / %s]\n' "$order" "$name" "$job" "$check"
    bash "$script"
    rc=$?
    if [ "$rc" -ne 0 ]; then
        printf '\nREFUSED at step %s: %s [%s / %s], exit %s\n' "$order" "$name" "$job" "$check" "$rc"
        exit "$rc"
    fi
done < "$PLAN"

# The compile step tees its output to build.log, which is not ignored by git: leaving it would make the tree dirty and
# refuse the gate's own next run, on a check that is about what is pushed and not about what this run wrote.
rm -f build.log

printf '\nOK: %s step(s) derived from .github/workflows passed.\n' "$ran"
