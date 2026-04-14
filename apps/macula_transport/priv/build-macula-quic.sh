#!/bin/sh
# Build the Quinn QUIC NIF for macula SDK v2.
#
# Called from rebar.config pre_hooks on every compile. Idempotent:
# skips rebuild when priv/libmacula_quic.so already exists and is newer
# than the Rust source tree.
set -eu

BASEDIR="${1:-.}"
PRIV_DIR="${BASEDIR}/priv"
NATIVE_DIR="${BASEDIR}/native"
CRATE_DIR="${NATIVE_DIR}/macula_quic"
OUT_SO="${PRIV_DIR}/libmacula_quic.so"

# When compiling inside _build/, native/ isn't symlinked but src/ is.
# Follow the src symlink to locate the source root.
if [ ! -d "${CRATE_DIR}" ] && [ -L "${BASEDIR}/src" ]; then
    SRC_TARGET=$(readlink -f "${BASEDIR}/src")
    SOURCE_ROOT=$(dirname "${SRC_TARGET}")
    if [ -d "${SOURCE_ROOT}/native/macula_quic" ]; then
        CRATE_DIR="${SOURCE_ROOT}/native/macula_quic"
    fi
fi

mkdir -p "${PRIV_DIR}"

if [ ! -d "${CRATE_DIR}" ]; then
    echo "[macula_quic] source not found at ${CRATE_DIR}; skipping NIF build."
    exit 0
fi

if ! command -v cargo >/dev/null 2>&1; then
    echo "[macula_quic] Rust toolchain not found. Install: https://rustup.rs"
    echo "[macula_quic] Continuing without NIF — macula_transport tests will fail."
    exit 0
fi

# Only rebuild when the source tree is newer than the artifact.
if [ -f "${OUT_SO}" ]; then
    NEWER=$(find "${CRATE_DIR}/src" "${CRATE_DIR}/Cargo.toml" -newer "${OUT_SO}" -print -quit 2>/dev/null || true)
    if [ -z "${NEWER}" ]; then
        exit 0
    fi
fi

echo "[macula_quic] building NIF (release)…"
( cd "${CRATE_DIR}" && cargo build --release )

SRC_SO="${CRATE_DIR}/target/release/libmacula_quic.so"
if [ ! -f "${SRC_SO}" ]; then
    echo "[macula_quic] build produced no artifact at ${SRC_SO}" >&2
    exit 1
fi

cp -f "${SRC_SO}" "${OUT_SO}"
echo "[macula_quic] installed ${OUT_SO}"
