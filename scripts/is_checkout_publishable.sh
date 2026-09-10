#!/usr/bin/env bash
# Is this checkout exactly the release that the version in src/macula.app.src
# names, so that publishing it to hex ships the tagged code?
#
# `rebar3 hex publish' packages the WORKING TREE, not a tag. On 2026-09-10,
# 10.23.0 was first published from a checkout one local commit past the tag,
# and hex served that commit until the package was replaced. Run this before
# anything slow, locally and in CI, and publish only when it exits 0.
#
# Refuses, printing every reason, unless:
#   1. the working tree is clean (no modified, staged or untracked files);
#   2. the tag v<vsn> exists and HEAD is exactly that tag's commit;
#   3. the remote has the tag at the same commit;
#   4. CHANGELOG.md has a section for <vsn>.
#
# Usage:
#   scripts/is_checkout_publishable.sh              # this repository
#   scripts/is_checkout_publishable.sh <repo-dir>   # another checkout
#   REMOTE=upstream scripts/is_checkout_publishable.sh
set -uo pipefail

REPO_DIR="${1:-$(cd "$(dirname "$0")/.." && pwd)}"
REMOTE="${REMOTE:-origin}"
APP_SRC="src/macula.app.src"
CHANGELOG="CHANGELOG.md"

cd "$REPO_DIR" || { echo "REFUSED: no such directory: $REPO_DIR"; exit 1; }

REASONS=()
refuse() { REASONS+=("$1"); }

VSN="$(sed -n 's/.*{vsn, *"\([^"]*\)"}.*/\1/p' "$APP_SRC" 2>/dev/null | head -n 1)"
[ -n "$VSN" ] || { echo "REFUSED: no {vsn, \"...\"} found in $APP_SRC"; exit 1; }
TAG="v$VSN"

# 1. Clean tree.
DIRTY="$(git status --porcelain --untracked-files=normal)"
[ -z "$DIRTY" ] || refuse "working tree is not clean: $(printf '%s' "$DIRTY" | head -n 5 | tr '\n' ';')"

# 2. HEAD is exactly the tag.
HEAD_COMMIT="$(git rev-parse HEAD)"
TAG_COMMIT="$(git rev-parse -q --verify "refs/tags/$TAG^{commit}" 2>/dev/null || true)"
if [ -z "$TAG_COMMIT" ]; then
    refuse "tag $TAG does not exist locally"
elif [ "$HEAD_COMMIT" != "$TAG_COMMIT" ]; then
    refuse "HEAD $(git rev-parse --short HEAD) is not tag $TAG ($(git rev-parse --short "$TAG_COMMIT"))"
fi

# 3. The remote has the tag at the same commit. An annotated tag is listed
# twice; the peeled line (^{}) names the commit.
REMOTE_LINES="$(git ls-remote --tags "$REMOTE" "refs/tags/$TAG" "refs/tags/$TAG^{}" 2>/dev/null)"
LS_REMOTE_RC=$?
REMOTE_COMMIT="$(printf '%s\n' "$REMOTE_LINES" | awk '/\^\{\}$/ {print $1; found=1} END {}' | head -n 1)"
[ -n "$REMOTE_COMMIT" ] || REMOTE_COMMIT="$(printf '%s\n' "$REMOTE_LINES" | awk 'NF {print $1}' | head -n 1)"
if [ "$LS_REMOTE_RC" -ne 0 ]; then
    refuse "could not list tags on remote $REMOTE"
elif [ -z "$REMOTE_COMMIT" ]; then
    refuse "tag $TAG is not on remote $REMOTE"
elif [ -n "$TAG_COMMIT" ] && [ "$REMOTE_COMMIT" != "$TAG_COMMIT" ]; then
    refuse "tag $TAG on $REMOTE is ${REMOTE_COMMIT:0:7}, locally ${TAG_COMMIT:0:7}"
fi

# 4. A CHANGELOG section for this version.
VSN_PATTERN="$(printf '%s' "$VSN" | sed 's/\./\\./g')"
grep -qE "^## \[$VSN_PATTERN\]" "$CHANGELOG" 2>/dev/null || refuse "$CHANGELOG has no section for $VSN"

if [ "${#REASONS[@]}" -gt 0 ]; then
    echo "REFUSED to publish macula $VSN from $REPO_DIR:"
    printf '  - %s\n' "${REASONS[@]}"
    exit 1
fi

echo "OK: $REPO_DIR is exactly $TAG ($(git rev-parse --short HEAD)), clean, on $REMOTE, with a CHANGELOG section."
