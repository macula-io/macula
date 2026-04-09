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
# ============================================================
build_nif() {
    local CRATE_NAME="$1"
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
        echo "[${CRATE_NAME}] WARNING: Rust toolchain not found, skipping NIF build."
        echo "[${CRATE_NAME}] Install Rust: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
        return 0
    fi

    echo "[${CRATE_NAME}] Building NIF from source..."
    cargo build --release --manifest-path "${CRATE_DIR}/Cargo.toml"

    # Copy .so (Linux) or .dylib (macOS) to priv/
    cp "${CRATE_DIR}/target/release/lib${CRATE_NAME}.so" "${NIF_FILE}" 2>/dev/null || \
    cp "${CRATE_DIR}/target/release/lib${CRATE_NAME}.dylib" "${NIF_FILE}" 2>/dev/null || \
    echo "[${CRATE_NAME}] WARNING: Could not find compiled NIF."
}

# ============================================================
# 1. Quinn QUIC NIF (with precompiled download)
# ============================================================
# Delegate to existing fetch-nif.sh which handles download + fallback
sh "${BASEDIR}/priv/fetch-nif.sh" "${BASEDIR}"

# ============================================================
# 2. Crypto, Identity, and MRI NIFs (build from source)
# ============================================================
build_nif "macula_crypto_nif"
build_nif "macula_ucan_nif"
build_nif "macula_did_nif"
build_nif "macula_mri_nif"

echo "[macula] All NIFs ready."
