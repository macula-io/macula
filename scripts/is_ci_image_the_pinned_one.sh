#!/usr/bin/env bash
# Is this job running on the CI image we pinned, with the tools we pinned?
#
# The build image (ghcr.io/macula-io/macula-ci-otp) supplies rebar3 and Rust.
# While a workflow referenced it as :latest, any rebuild of the image could
# change the compiler this repository builds with, with no commit here. We
# already lost a hex publish to a tool that moved overnight (rebar3_hex 7.3.0).
#
# Refuses, naming what it found, unless:
#   1. every reference to macula-ci-otp in the workflow file is
#      :YYYYMMDD-HHmm@sha256:<64 hex>, a dated tag AND a digest;
#   2. rebar3, rustc and cargo on PATH report exactly the versions the
#      workflow declares in CI_REBAR3_VERSION and CI_RUST_VERSION.
#
# Usage (in the job, after checkout; the env vars come from the workflow):
#   scripts/is_ci_image_the_pinned_one.sh .github/workflows/<file>.yml
set -uo pipefail

WORKFLOW="${1:?usage: $0 <workflow-file>}"
REASONS=()
refuse() { REASONS+=("$1"); }

[ -f "$WORKFLOW" ] || { echo "REFUSED: no workflow file $WORKFLOW"; exit 1; }
: "${CI_REBAR3_VERSION:?CI_REBAR3_VERSION is not set by the workflow}"
: "${CI_RUST_VERSION:?CI_RUST_VERSION is not set by the workflow}"

# 1. How the workflow names the image.
# Full image references only, so a comment that merely names the image is
# not mistaken for a use of it.
REFS="$(grep -oE 'ghcr\.io/macula-io/macula-ci-otp[^[:space:]"'"'"']*' "$WORKFLOW" \
        | sed 's#^ghcr\.io/macula-io/##' | sort -u)"
[ -n "$REFS" ] || refuse "$WORKFLOW never references macula-ci-otp"
while IFS= read -r ref; do
    [ -n "$ref" ] || continue
    printf '%s\n' "$ref" | grep -qE '^macula-ci-otp:[0-9]{8}-[0-9]{4}@sha256:[0-9a-f]{64}$' \
        || refuse "$WORKFLOW references '$ref', not :YYYYMMDD-HHmm@sha256:<digest>"
done <<< "$REFS"

# 2. What the image actually carries. The version is the word after the
# tool's own name on the line that starts with it: "rebar 3.27.0 on ...",
# "rustc 1.98.1 (...)". Not simply the first line: inside a project,
# `rebar3 version' first fetches the project's plugins and prints
# "===> Fetching ..." lines ahead of its own.
tool_version() {
    local word="$1"; shift
    "$@" 2>/dev/null | awk -v w="$word" '$1 == w {print $2; exit}'
}
check() {
    local name="$1" word="$2" want="$3"; shift 3
    local got
    got="$(tool_version "$word" "$@")"
    [ "$got" = "$want" ] || refuse "$name is '${got:-missing}', the workflow pins $want"
}
check rebar3 rebar "$CI_REBAR3_VERSION" rebar3 version
check rustc rustc "$CI_RUST_VERSION" rustc --version
check cargo cargo "$CI_RUST_VERSION" cargo --version

if [ "${#REASONS[@]}" -gt 0 ]; then
    echo "REFUSED: the CI image is not the pinned one:"
    printf '  - %s\n' "${REASONS[@]}"
    exit 1
fi
echo "OK: macula-ci-otp pinned by digest; rebar3 $CI_REBAR3_VERSION, rustc and cargo $CI_RUST_VERSION."
