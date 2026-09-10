#!/usr/bin/env bash
# Is the Erlang/OTP on PATH exactly the one .tool-versions pins?
#
# rebar3 builds with the erl it finds on PATH. On 2026-09-11, 10.24.0 was
# published from OTP 29.0.6 while .tool-versions pins erlang 28.4.2, and
# OTP 29's edoc chunks hold only exported functions, so the published docs
# differed from a 28.4.2 build. Run this before building anything to publish.
#
# Refuses, printing both versions, unless erlang:system_info(otp_release) is
# the pinned major version and, where releases/<release>/OTP_VERSION can be
# read, the full version equals the pin.
#
# Usage:
#   scripts/is_erlang_the_pinned_version.sh              # this repository
#   scripts/is_erlang_the_pinned_version.sh <repo-dir>   # another checkout
set -uo pipefail

REPO_DIR="${1:-$(cd "$(dirname "$0")/.." && pwd)}"
TOOL_VERSIONS="$REPO_DIR/.tool-versions"

PINNED="$(awk '$1 == "erlang" {print $2; exit}' "$TOOL_VERSIONS" 2>/dev/null)"
[ -n "$PINNED" ] || { echo "REFUSED: no erlang pin found in $TOOL_VERSIONS"; exit 1; }
PINNED_MAJOR="${PINNED%%.*}"

ERL="$(command -v erl)" || { echo "REFUSED: no erl on PATH; .tool-versions pins erlang $PINNED"; exit 1; }

# Prints "<otp_release> <OTP_VERSION>", the second part empty when unreadable.
RUNNING="$("$ERL" -noshell -eval '
    Release = erlang:system_info(otp_release),
    File = filename:join([code:root_dir(), "releases", Release, "OTP_VERSION"]),
    Full = case file:read_file(File) of {ok, Bin} -> string:trim(Bin); _ -> <<>> end,
    io:format("~s ~s~n", [Release, Full]),
    halt().' 2>/dev/null)"
read -r RUNNING_MAJOR RUNNING_FULL <<< "$RUNNING"

[ -n "${RUNNING_MAJOR:-}" ] || { echo "REFUSED: could not run $ERL; .tool-versions pins erlang $PINNED"; exit 1; }

if [ "$RUNNING_MAJOR" != "$PINNED_MAJOR" ] || { [ -n "${RUNNING_FULL:-}" ] && [ "$RUNNING_FULL" != "$PINNED" ]; }; then
    echo "REFUSED: erl on PATH is OTP ${RUNNING_FULL:-$RUNNING_MAJOR} ($ERL),"
    echo "  but .tool-versions pins erlang $PINNED."
    echo "  Put the pinned Erlang first on PATH (for example through mise or asdf) and run again."
    exit 1
fi

if [ -z "${RUNNING_FULL:-}" ]; then
    echo "OK: OTP $RUNNING_MAJOR matches the pinned major of erlang $PINNED ($ERL); the full version could not be read."
else
    echo "OK: OTP $RUNNING_FULL is the pinned erlang $PINNED ($ERL)."
fi
