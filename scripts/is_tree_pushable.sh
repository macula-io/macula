#!/usr/bin/env bash
# Is this tree fit to push to macula's main branch?
#
# rebar.config's xref_checks name what xref treats as a defect: a call to a
# function that does not exist, a function that does not exist, a local
# function nothing calls, and a call to a deprecated function. Nothing else
# runs rebar3 xref, so without this a push that calls a removed or deprecated
# function passes whenever no test walks that path. Run this before every push
# to main, locally and in CI, and push only when it exits 0.
#
# Refuses, printing the step that failed, unless, in order:
#   1. the working tree is clean (no modified, staged or untracked files), so
#      what is checked is the commit that is pushed;
#   2. the Erlang/OTP on PATH is the one .tool-versions pins;
#   3. rebar3 xref passes;
#   4. rebar3 eunit passes, over every test module.
#
# Usage:
#   scripts/is_tree_pushable.sh              # this repository
#   scripts/is_tree_pushable.sh <repo-dir>   # another checkout
set -uo pipefail

REPO_DIR="${1:-$(cd "$(dirname "$0")/.." && pwd)}"

cd "$REPO_DIR" || { echo "REFUSED: no such directory: $REPO_DIR"; exit 1; }

DIRTY="$(git status --porcelain --untracked-files=normal)"
[ -z "$DIRTY" ] || { echo "REFUSED: working tree is not clean: $(printf '%s' "$DIRTY" | head -n 5 | tr '\n' ';')"; exit 1; }

"$REPO_DIR/scripts/is_erlang_the_pinned_version.sh" "$REPO_DIR" || { echo "REFUSED: not the pinned Erlang/OTP"; exit 1; }

rebar3 xref || { echo "REFUSED: rebar3 xref failed"; exit 1; }

rebar3 eunit || { echo "REFUSED: rebar3 eunit failed"; exit 1; }

echo "OK: $REPO_DIR at $(git rev-parse --short HEAD) is clean, on the pinned Erlang/OTP, and passes xref and every eunit test."
