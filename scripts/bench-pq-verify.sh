#!/usr/bin/env bash
# Measures signing and verification cost per crypto profile, ML-DSA-87
# (pq_pure) and the LAMPS composite id-MLDSA87-RSA4096-PSS-SHA512 (pq_hybrid), through
# macula_node_keys:sign/2 and verify/4 on one core, for a set of message
# sizes. The verification budget per connection cites these numbers.
set -euo pipefail

REPO_ROOT="${REPO_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
BUILD="${BUILD:-$REPO_ROOT/_build/bench_pq_verify}"
VERIFY_ITERATIONS="${VERIFY_ITERATIONS:-2000}"
SIGN_ITERATIONS="${SIGN_ITERATIONS:-200}"
MESSAGE_SIZES="${MESSAGE_SIZES:-512,8192,262144}"
MODULES="${MODULES:-src/identity/macula_node_keys.erl src/crypto_profile/macula_crypto_profile.erl scripts/bench_pq_verify.erl}"

mkdir -p "$BUILD"
for module in $MODULES; do
  erlc -o "$BUILD" "$REPO_ROOT/$module"
done

echo "commit: $(git -C "$REPO_ROOT" rev-parse --short HEAD 2>/dev/null || echo unknown)"
echo "cpu: $(lscpu | awk -F: '/Model name/ {gsub(/^ +/, "", $2); print $2; exit}')"
erl -noshell -pa "$BUILD" \
  -eval "bench_pq_verify:main(${VERIFY_ITERATIONS}, ${SIGN_ITERATIONS}, [${MESSAGE_SIZES}]), halt()."
