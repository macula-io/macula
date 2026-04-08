#!/bin/bash
# Fetch precompiled Quinn NIF from GitHub releases.
# Falls back to building from source if download fails.
#
# Usage: scripts/fetch-nif.sh [BASEDIR]
# Called by rebar.config pre_hooks during compilation.
set -eu

BASEDIR="${1:-.}"
PRIV_DIR="${BASEDIR}/priv"
NIF_FILE="${PRIV_DIR}/libmacula_quic.so"
NATIVE_DIR="${BASEDIR}/native/macula_quic"

# Skip if NIF already exists
if [ -f "${NIF_FILE}" ]; then
    exit 0
fi

mkdir -p "${PRIV_DIR}"

build_from_source() {
    if [ ! -d "${NATIVE_DIR}" ]; then
        echo "[macula_quic] ERROR: No Rust source and download failed."
        echo "[macula_quic] Either install from a tagged release or install Rust:"
        echo "[macula_quic]   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
        exit 1
    fi

    if ! command -v cargo >/dev/null 2>&1; then
        echo "[macula_quic] ERROR: Rust toolchain not found."
        echo "[macula_quic]   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
        exit 1
    fi

    echo "[macula_quic] Building NIF from source..."
    cargo build --release --manifest-path "${NATIVE_DIR}/Cargo.toml"
    cp "${NATIVE_DIR}/target/release/libmacula_quic.so" "${NIF_FILE}" 2>/dev/null || \
    cp "${NATIVE_DIR}/target/release/libmacula_quic.dylib" "${NIF_FILE}" 2>/dev/null || true
    echo "[macula_quic] NIF built successfully."
}

# Detect platform
ARCH=$(uname -m)
OS=$(uname -s | tr '[:upper:]' '[:lower:]')

case "${ARCH}" in
    x86_64|amd64) ARCH="x86_64" ;;
    aarch64|arm64) ARCH="aarch64" ;;
    *) echo "[macula_quic] Unsupported arch: ${ARCH}" && build_from_source && exit 0 ;;
esac

if [ "${OS}" != "linux" ]; then
    echo "[macula_quic] Precompiled NIF only for Linux"
    build_from_source
    exit 0
fi

# Detect musl (Alpine) vs glibc — precompiled NIF is glibc, musl must build from source
if ldd --version 2>&1 | grep -qi musl; then
    echo "[macula_quic] musl libc detected (Alpine) — building from source"
    build_from_source
    exit 0
fi

ARTIFACT="libmacula_quic-${OS}-${ARCH}.so"

# Read version from app.src
VERSION=$(grep '{vsn' "${BASEDIR}/src/macula.app.src" 2>/dev/null | sed 's/.*"\(.*\)".*/\1/' || echo "")
if [ -z "${VERSION}" ]; then
    echo "[macula_quic] Cannot determine version"
    build_from_source
    exit 0
fi

RELEASE_URL="https://github.com/macula-io/macula/releases/download/v${VERSION}/${ARTIFACT}"
CHECKSUM_URL="${RELEASE_URL}.sha256"

echo "[macula_quic] Fetching precompiled NIF v${VERSION} for ${OS}-${ARCH}..."

# Try downloading precompiled NIF
if curl -fsSL --retry 2 --max-time 30 -o "${NIF_FILE}" "${RELEASE_URL}" 2>/dev/null; then
    # Verify checksum
    if curl -fsSL --retry 2 --max-time 10 -o "${NIF_FILE}.sha256" "${CHECKSUM_URL}" 2>/dev/null; then
        EXPECTED=$(awk '{print $1}' "${NIF_FILE}.sha256")
        ACTUAL=$(sha256sum "${NIF_FILE}" | awk '{print $1}')
        rm -f "${NIF_FILE}.sha256"
        if [ "${EXPECTED}" = "${ACTUAL}" ]; then
            echo "[macula_quic] Checksum OK. Precompiled NIF ready."
            exit 0
        fi
        echo "[macula_quic] Checksum mismatch!"
        rm -f "${NIF_FILE}"
    else
        # No checksum available but download succeeded — use it
        echo "[macula_quic] No checksum, using downloaded NIF."
        exit 0
    fi
else
    echo "[macula_quic] Download failed."
fi

# Fallback: build from source
build_from_source
