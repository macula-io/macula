#!/usr/bin/env bash
# Does the QUIC NIF in a built macula belong to this source tree?
#
# THIS EXISTS BECAUSE a build once installed the precompiled QUIC NIF released
# for the version in src/macula.app.src while native/macula_quic had changed
# since that release. The build reported success, and macula_quic then failed
# to load with {bad_lib, "Function not found macula_quic:nif_connect/8"}.
# macula now builds the NIF from its own native/ sources; this check stops a
# release, or a local build, whose library still does not load.
#
# It passes only when macula_quic loads from the built tree. Given a build log,
# it also fails when that log shows a precompiled NIF being fetched.
#
# Usage:
#   scripts/is_quic_nif_built_from_this_tree.sh [BUILD_LIB_DIR [BUILD_LOG]]
#     BUILD_LIB_DIR  the rebar3 lib directory, default _build/default/lib
#     BUILD_LOG      optional output of the build that made it
set -euo pipefail

# erl may be reachable only through mise, as in is_hex_serving_what_git_says.sh.
ERL=(erl)
command -v erl >/dev/null 2>&1 || ERL=(mise exec -- erl)

LIB_DIR="${1:-_build/default/lib}"
BUILD_LOG="${2:-}"

if [ ! -d "$LIB_DIR/macula/ebin" ]; then
    echo "no macula build under $LIB_DIR"
    exit 2
fi

if [ -n "$BUILD_LOG" ] && grep -q 'Fetching precompiled' "$BUILD_LOG"; then
    echo "the build fetched a precompiled NIF instead of building it from this tree:"
    grep 'Fetching precompiled' "$BUILD_LOG"
    exit 1
fi

"${ERL[@]}" -noshell -pa "$LIB_DIR"/*/ebin -eval '
    case code:ensure_loaded(macula_quic) of
        {module, macula_quic} ->
            io:format("macula_quic loads from ~s~n", [code:priv_dir(macula)]),
            halt(0);
        Other ->
            io:format("macula_quic does not load: ~p~n", [Other]),
            halt(1)
    end.'
