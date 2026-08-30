#!/bin/bash
# Build all Rust NIFs for the macula package.
# Handles both the Quinn QUIC NIF (with precompiled download fallback)
# and the crypto/identity/MRI NIFs (always built from source).
#
# Usage: priv/build-nifs.sh [BASEDIR]
# Called by rebar.config pre_hooks during compilation.
set -eu

BASEDIR="${1:-.}"
PRIV_DIR="${BASEDIR}/priv"
NATIVE_DIR="${BASEDIR}/native"

# When compiling inside _build/, native/ isn't symlinked but src/ is.
# Follow the src symlink to find the source root and its native/ dir.
if [ ! -d "${NATIVE_DIR}" ] && [ -L "${BASEDIR}/src" ]; then
    SRC_TARGET=$(readlink -f "${BASEDIR}/src")
    SOURCE_ROOT=$(dirname "${SRC_TARGET}")
    if [ -d "${SOURCE_ROOT}/native" ]; then
        NATIVE_DIR="${SOURCE_ROOT}/native"
    fi
fi

mkdir -p "${PRIV_DIR}"

# ============================================================
# Helper: build a Rust NIF crate from source
#
# REQUIRED="true" (macula_cbor_nif only, see below) makes a missing
# cargo or a failed build a hard error (exit 1) instead of a warning.
# Every OTHER caller here has a real Erlang fallback (macula_crypto_nif,
# macula_ucan_nif, macula_did_nif, macula_mri_nif all document one in
# their own moduledoc) and stays soft-skip on purpose: a consumer
# without a Rust toolchain still gets a working, if slower, build.
# ============================================================
build_nif() {
    local CRATE_NAME="$1"
    local REQUIRED="${2:-false}"
    local NIF_FILE="${PRIV_DIR}/${CRATE_NAME}.so"
    local CRATE_DIR="${NATIVE_DIR}/${CRATE_NAME}"

    # Skip if already built
    if [ -f "${NIF_FILE}" ]; then
        return 0
    fi

    if [ ! -d "${CRATE_DIR}" ]; then
        echo "[${CRATE_NAME}] WARNING: No source at ${CRATE_DIR}, skipping."
        return 0
    fi

    if ! command -v cargo >/dev/null 2>&1; then
        if [ "${REQUIRED}" = "true" ]; then
            echo "[${CRATE_NAME}] ERROR: Rust toolchain not found. This NIF has no Erlang" >&2
            echo "[${CRATE_NAME}] fallback (see its own moduledoc) -- a build that silently" >&2
            echo "[${CRATE_NAME}] skipped it would produce a package that compiles clean and" >&2
            echo "[${CRATE_NAME}] fails every caller at runtime with nif_not_loaded instead." >&2
            echo "[${CRATE_NAME}] Install Rust: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh" >&2
            exit 1
        fi
        echo "[${CRATE_NAME}] WARNING: Rust toolchain not found, skipping NIF build."
        echo "[${CRATE_NAME}] Install Rust: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
        return 0
    fi

    echo "[${CRATE_NAME}] Building NIF from source..."
    cargo build --release --manifest-path "${CRATE_DIR}/Cargo.toml"

    # Copy .so (Linux) or .dylib (macOS) to priv/
    if cp "${CRATE_DIR}/target/release/lib${CRATE_NAME}.so" "${NIF_FILE}" 2>/dev/null || \
       cp "${CRATE_DIR}/target/release/lib${CRATE_NAME}.dylib" "${NIF_FILE}" 2>/dev/null; then
        return 0
    fi
    echo "[${CRATE_NAME}] WARNING: Could not find compiled NIF."
    if [ "${REQUIRED}" = "true" ]; then
        echo "[${CRATE_NAME}] ERROR: build reported success but produced no .so/.dylib -- this" >&2
        echo "[${CRATE_NAME}] NIF has no Erlang fallback, so a silently missing binary is worse" >&2
        echo "[${CRATE_NAME}] than a failed build. See build output above for the real cause." >&2
        exit 1
    fi
}

# ============================================================
# 1. Quinn QUIC NIF (with precompiled download)
# ============================================================
# Delegate to existing fetch-nif.sh which handles download + fallback
sh "${BASEDIR}/priv/fetch-nif.sh" "${BASEDIR}"

# ============================================================
# 2. Crypto, Identity, and MRI NIFs (build from source, soft-skip --
#    all have a real Erlang fallback)
# ============================================================
build_nif "macula_crypto_nif"
build_nif "macula_ucan_nif"
build_nif "macula_did_nif"
build_nif "macula_mri_nif"

# ============================================================
# 3. CBOR NIF (build from source, REQUIRED -- macula_cbor_nif.erl's
#    own moduledoc: "There is NO Erlang fallback ... Failing fast at
#    NIF-load time is the right behavior." A soft-skip here produced
#    exactly the opposite: a clean build that fails every caller at
#    test/runtime with an opaque nif_not_loaded, found live in
#    hecate-om's CI (erlang:28 container, no Rust toolchain installed).
# ============================================================
build_nif "macula_cbor_nif" "true"

echo "[macula] All NIFs ready."
