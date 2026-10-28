#!/usr/bin/env bash
# Fail when the local gate and the gating workflows have drifted apart (macula#163).
#
#   scripts/is_the_local_gate_in_step_with_ci.sh
#
# CI runs this, so drift is red where it can be seen by everyone rather than discovered by a member whose green local
# gate let a red step through. It reads .github/workflows/test.yml and docs.yml, follows the reusable doc-snippet
# workflow to the sha docs.yml pins, and refuses when:
#
#   unknown-job    a job nobody has ruled on: a new job is unknown until a commit says whether it can run locally
#   unruled-workflow    a workflow file that gates a push and is in neither the plan nor the ruled list
#   no-step        a matrix check whose steps all skipped, which would be green having run nothing
#   no-matrix-checks    a matrix check list the parser could not read, so that check could not be seen to have run
#   shared-step-script  two steps written to one script, so one runs the other's text and both may pass for it
#   unmodelled-shell    a step using a shell other than the default bash, which this gate does not model
#   unresolved-expression   a step using ${{ ... }} the gate cannot resolve: an input with no default, a matrix entry,
#                           an env value or the ref
#   multiple-uses  a workflow calling more than one reusable workflow, of which the plan would follow one
#   unapplied-with a caller passing inputs the gate would not apply, so the callee's defaults would run instead
#   stale-ruling   a ruling for a job the workflows no longer contain
#   stale-workflow-ruling   a workflow ruling for a file .github/workflows no longer has
#   unpinned       a reusable workflow called by anything but a full commit sha
#
# It prints the jobs the gate does NOT run, with the reason, every time: the drift that matters most is the one a member
# does not know about.
set -uo pipefail
cd "$(dirname "$0")/.."

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

bash scripts/ci_gate_plan.sh --work "$WORK"
rc=$?
if [ "$rc" -ne 0 ]; then
    echo "DRIFT: the local gate is not in step with CI (exit $rc). Fix scripts/ci_gate_plan.sh, not this check."
    exit "$rc"
fi
echo "OK: the local gate is in step with CI."
