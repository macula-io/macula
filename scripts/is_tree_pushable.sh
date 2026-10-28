#!/usr/bin/env bash
# Is this tree fit to push to macula's main branch? One gate, equal to CI (macula#163).
#
# WHAT IT IS. It runs the steps of the gating workflows themselves - .github/workflows/test.yml and docs.yml, including
# the doc-snippet check whose escript lives in macula-ci-images at the sha docs.yml pins - inside the image those
# workflows pin, as root, derived at run time rather than copied here. A gate that copies CI cannot be in step with it;
# one that is derived cannot fall out of step without scripts/ci_gate_plan.sh refusing.
#
# WHAT RUNS IS THE COMMIT. HEAD is exported with `git archive` into a throwaway directory and that is what the container
# sees, never the working directory. Mounting the live tree let a file git ignores decide the result - a
# `_checkouts/<dep>` shadowing the locked one, an `_build` compiled on another branch - so the gate could pass a tree CI
# rejects, which is the failure this script exists to prevent. It also kept the container's root-owned files out of the
# member's checkout, where a red run left a build.log behind and the next run refused the tree as dirty.
#
# Refuses, printing the step that refused and that step's own exit status, unless, in order:
#   1. the working tree is clean (no modified, staged or untracked files), so the commit exported is the commit that is
#      pushed and nothing unpublished is lost. This is checked HERE and not in the container: git inside the container
#      cannot see a worktree's gitdir, so the check would pass having examined nothing;
#   2. the gating workflow's container is pinned by digest, so the toolchain is the one CI uses;
#   3. every derived step passes.
#
# Prints what it does NOT cover before it runs, so a green gate is not read as more than it is: a job that cannot run
# inside the build image (cargo clippy, the advisory scans) and a workflow that is not in the plan (the PDF build, the
# release) are named with the reason, and CI still runs them.
#
# Usage:
#   scripts/is_tree_pushable.sh              # this repository
#   scripts/is_tree_pushable.sh <repo-dir>   # another checkout
#
# Exits 3, never 0, when no container runtime is installed: a gate that cannot run is not a gate that passed.
set -uo pipefail

REPO_DIR="${1:-$(cd "$(dirname "$0")/.." && pwd)}"
cd "$REPO_DIR" || { echo "REFUSED: no such directory: $REPO_DIR"; exit 1; }

DIRTY="$(git status --porcelain --untracked-files=normal)"
[ -z "$DIRTY" ] || {
    echo "REFUSED: working tree is not clean: $(printf '%s' "$DIRTY" | head -n 5 | tr '\n' ';')"
    exit 1
}

WORKFLOW=".github/workflows/test.yml"
IMAGE="$(awk '/^[[:space:]]*container:[[:space:]]*/{ sub(/^[[:space:]]*container:[[:space:]]*/, ""); print; exit }' "$WORKFLOW")"
[ -n "$IMAGE" ] || { echo "REFUSED: no container: in $WORKFLOW, so there is no image to run CI's steps in"; exit 1; }
case "$IMAGE" in
    *@sha256:*) ;;
    *) echo "REFUSED: $WORKFLOW's container is not pinned by digest: $IMAGE"; exit 1 ;;
esac

RUNTIME="$(command -v podman || command -v docker || true)"
[ -n "$RUNTIME" ] || {
    echo "CANNOT RUN: neither podman nor docker is installed, so the gate cannot run CI's steps in CI's image."
    echo "NOT PASSED: this is exit 3 for that reason; do not read it as a green tree."
    exit 3
}

# WHAT RUNS IS THE COMMIT, NOT THE WORKING DIRECTORY.
EXPORT="$(mktemp -d "${TMPDIR:-/tmp}/macula-gate.XXXXXX")" || {
    echo "CANNOT RUN: no writable temporary directory to export HEAD into."
    exit 3
}
cleanup() {
    rm -rf "$EXPORT" 2>/dev/null && return
    # A rootless container writes as a subuid, which the invoking user cannot unlink; inside the container's own user
    # namespace it can, and `podman unshare` is how to enter it. Docker without root has no equivalent, so the directory
    # is named rather than left silently behind.
    if [ "${RUNTIME##*/}" = "podman" ]; then podman unshare rm -rf "$EXPORT" 2>/dev/null && return; fi
    rm -rf "$EXPORT" 2>/dev/null || echo "NOTE: could not remove $EXPORT; it holds files this user cannot unlink."
}
trap cleanup EXIT

git -C "$REPO_DIR" archive HEAD | tar -x -m -C "$EXPORT" || {
    echo "REFUSED: could not export HEAD into $EXPORT."
    exit 1
}

# The plan and the run both happen inside the image: the derivation reads the workflows of the exported commit, and the
# steps must run on the toolchain CI uses rather than the host's.
"$RUNTIME" run --rm \
    --cgroup-parent=ci-runners.slice \
    -v "$EXPORT":/work:Z \
    -w /work \
    -e GATE_ROOT=/work \
    "$IMAGE" \
    bash -c 'bash scripts/ci_gate_plan.sh --work _build/local-gate && bash scripts/ci_gate_run_plan.sh _build/local-gate'
rc=$?
if [ "$rc" -ne 0 ]; then
    echo "REFUSED: the gate did not pass (exit $rc). The step above is the one CI would have refused on."
    exit "$rc"
fi

echo "OK: $(git -C "$REPO_DIR" rev-parse HEAD) is clean, and every step .github/workflows gates on passed on that commit."
