#!/usr/bin/env bash
# Set the version this repository declares, in every place it declares it.
#
# THIS EXISTS BECAUSE THE VERSION LIVES IN TWO FILES AND MISSING ONE IS SILENT.
# src/macula.app.src is what rebar3 packages and what hex serves. CLAUDE.md's
# header is what a reader (and an agent) is told the current version is. A bump
# that updates only the first leaves the second claiming the previous release
# indefinitely, and nothing complains.
#
# It does NOT tag and does NOT publish. A tag is a push, a push goes through
# the gate, and the tag is what arms .github/workflows/publish-hex.yml, which
# then publishes with no further gate, by design: pushing the tag IS the
# release. Any other path to hex.pm would be a second, unchecked one.
#
# Usage:
#   scripts/bump-version.sh 11.6.0
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
APP_SRC="${REPO_ROOT}/src/macula.app.src"
CLAUDE_MD="${REPO_ROOT}/CLAUDE.md"

NEW_VERSION="${1:-}"
if [ -z "${NEW_VERSION}" ]; then
    echo "usage: $0 <version>      e.g. $0 11.6.0"
    exit 1
fi

case "${NEW_VERSION}" in
    [0-9]*.[0-9]*.[0-9]*) ;;
    *) echo "REFUSED: '${NEW_VERSION}' is not MAJOR.MINOR.PATCH"; exit 1 ;;
esac

for f in "${APP_SRC}" "${CLAUDE_MD}"; do
    [ -f "$f" ] || { echo "REFUSED: no such file: $f"; exit 1; }
done

CURRENT="$(sed -n 's/.*{vsn, "\([^"]*\)".*/\1/p' "${APP_SRC}")"
[ -n "${CURRENT}" ] || { echo "REFUSED: no {vsn, ...} found in ${APP_SRC}"; exit 1; }

echo "  ${CURRENT} -> ${NEW_VERSION}"

sed -i "s/{vsn, \"${CURRENT}\"}/{vsn, \"${NEW_VERSION}\"}/" "${APP_SRC}"
# The version pattern takes a whole semver string, pre-release suffix
# included (12.0.0-alpha.1). One that stopped at "-" left the old suffix
# behind and read back a truncated version, so a bump from 12.0.0-alpha.2 to
# 12.0.0 left CLAUDE.md saying alpha.2 and still reported success.
sed -i "s/^\*\*Current Version\*\*: v[0-9][0-9A-Za-z.+-]*/**Current Version**: v${NEW_VERSION}/" "${CLAUDE_MD}"

# Confirm rather than assume: a sed that matched nothing exits 0.
WROTE_APP="$(sed -n 's/.*{vsn, "\([^"]*\)".*/\1/p' "${APP_SRC}")"
WROTE_MD="$(sed -n 's/^\*\*Current Version\*\*: v\([0-9][0-9A-Za-z.+-]*\).*/\1/p' "${CLAUDE_MD}")"

FAILED=0
[ "${WROTE_APP}" = "${NEW_VERSION}" ] || { echo "  FAILED to set ${APP_SRC} (reads ${WROTE_APP:-nothing})"; FAILED=1; }
[ "${WROTE_MD}" = "${NEW_VERSION}" ] || { echo "  FAILED to set ${CLAUDE_MD} (reads ${WROTE_MD:-nothing})"; FAILED=1; }
[ "${FAILED}" -eq 0 ] || exit 1

echo "  set in $(basename "${APP_SRC}") and $(basename "${CLAUDE_MD}")"
echo
echo "Next: CHANGELOG entry, then the release checklist in CLAUDE.md."
echo "The tag and the publish go through the gate, not from here."
