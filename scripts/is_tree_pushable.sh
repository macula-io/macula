#!/usr/bin/env bash
# Is this tree fit to push to macula's main branch?
#
# rebar.config's xref_checks name what xref treats as a defect: a call to a
# function that does not exist, a function that does not exist, a local
# function nothing calls, and a call to a deprecated function. Nothing else
# runs rebar3 xref, so without this a push that calls a removed or deprecated
# function passes whenever no test walks that path. Run this before every push
# to main, and push only when it exits 0. No CI workflow runs it yet.
#
# Refuses, printing the step that failed, unless, in order:
#   1. the directory is a git checkout whose working tree is clean (no
#      modified, staged or untracked files), so what is checked is the commit
#      that is pushed;
#   2. there is no _checkouts directory: rebar3 builds the dependencies in it
#      from local copies, while the pushed tree resolves them from rebar.config;
#   3. the Erlang/OTP on PATH is the one .tool-versions pins;
#   4. rebar3 xref passes;
#   5. rebar3 eunit passes, over every test module.
#
# Usage:
#   scripts/is_tree_pushable.sh              # this repository
#   scripts/is_tree_pushable.sh <repo-dir>   # another checkout, absolute or relative
set -uo pipefail

REPO_ARG="${1:-$(dirname "$0")/..}"
REPO_DIR="$(cd "$REPO_ARG" 2>/dev/null && pwd)" || { echo "REFUSED: no such directory: $REPO_ARG"; exit 1; }

cd "$REPO_DIR" || { echo "REFUSED: no such directory: $REPO_DIR"; exit 1; }

DIRTY="$(git status --porcelain --untracked-files=normal 2>/dev/null)" || { echo "REFUSED: not a git repository: $REPO_DIR"; exit 1; }
[ -z "$DIRTY" ] || { echo "REFUSED: working tree is not clean: $(printf '%s' "$DIRTY" | head -n 5 | tr '\n' ';')"; exit 1; }

[ ! -e "$REPO_DIR/_checkouts" ] || { echo "REFUSED: $REPO_DIR/_checkouts exists; dependencies would build from local copies, not from rebar.config"; exit 1; }

"$REPO_DIR/scripts/is_erlang_the_pinned_version.sh" "$REPO_DIR" || { echo "REFUSED: not the pinned Erlang/OTP"; exit 1; }

rebar3 xref || { echo "REFUSED: rebar3 xref failed"; exit 1; }

rebar3 eunit || { echo "REFUSED: rebar3 eunit failed"; exit 1; }

echo "OK: $REPO_DIR at $(git rev-parse --short HEAD) is clean, on the pinned Erlang/OTP, and passes xref and every eunit test."
