#!/usr/bin/env bash
##
## Build professional PDFs from macula documentation.
## Uses pandoc + xelatex for scientific-style output.
##
## Usage: bash scripts/build-docs-pdf.sh
## Output: dist/*.pdf
##
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DIST_DIR="${REPO_ROOT}/dist"

mkdir -p "${DIST_DIR}"

# Pandoc flags for professional scientific-style PDF
PANDOC_COMMON=(
  --pdf-engine=xelatex
  -V geometry:margin=2.5cm
  -V "mainfont=DejaVu Sans"
  -V "monofont=DejaVu Sans Mono"
  -V fontsize=11pt
  -V colorlinks=true
  -V linkcolor=blue
  -V urlcolor=blue
  -V "header-includes=\usepackage{fancyhdr}\pagestyle{fancy}\fancyhead[L]{Macula}\fancyhead[R]{\leftmark}\fancyfoot[C]{\thepage}"
  --highlight-style=tango
  -V papersize=a4
  "--resource-path=${REPO_ROOT}:${REPO_ROOT}/assets"
)

build_pdf() {
  local output_name="$1"
  shift
  local files=()
  for f in "$@"; do
    [ -f "$f" ] && files+=("$f") || echo "  [SKIP] $f"
  done
  [ ${#files[@]} -eq 0 ] && { echo "  [SKIP] No files for ${output_name}"; return; }
  echo "  Building ${output_name}.pdf (${#files[@]} files)..."
  pandoc "${PANDOC_COMMON[@]}" -o "${DIST_DIR}/${output_name}.pdf" "${files[@]}"
}

build_pdf_with_toc() {
  local output_name="$1"
  shift
  local files=()
  for f in "$@"; do
    [ -f "$f" ] && files+=("$f") || echo "  [SKIP] $f"
  done
  [ ${#files[@]} -eq 0 ] && { echo "  [SKIP] No files for ${output_name}"; return; }
  echo "  Building ${output_name}.pdf with TOC (${#files[@]} files)..."
  pandoc "${PANDOC_COMMON[@]}" --toc --toc-depth=2 \
    -V toc-title="Table of Contents" \
    -o "${DIST_DIR}/${output_name}.pdf" "${files[@]}"
}

echo "=== Macula Documentation PDF Builder ==="
echo ""

# --- Individual Guide PDFs ---

echo "[1/8] Distribution Over Relay Mesh"
build_pdf "macula-dist-over-mesh" \
  "${REPO_ROOT}/docs/guides/DIST_OVER_MESH_GUIDE.md"

echo "[2/8] PubSub Guide"
build_pdf "macula-pubsub" \
  "${REPO_ROOT}/docs/guides/PUBSUB_GUIDE.md"

echo "[3/8] RPC Guide"
build_pdf "macula-rpc" \
  "${REPO_ROOT}/docs/guides/RPC_GUIDE.md"

echo "[4/8] Clustering Guide"
build_pdf "macula-clustering" \
  "${REPO_ROOT}/docs/guides/CLUSTERING_GUIDE.md"

echo "[5/8] Authorization Guide"
build_pdf "macula-authorization" \
  "${REPO_ROOT}/docs/guides/AUTHORIZATION_GUIDE.md"

echo "[6/8] DHT Architecture"
build_pdf "macula-dht" \
  "${REPO_ROOT}/docs/guides/DHT_GUIDE.md"

echo "[7/8] Operator Guides"
build_pdf "macula-operator" \
  "${REPO_ROOT}/docs/operator/TLS_GUIDE.md" \
  "${REPO_ROOT}/docs/operator/MONITORING_GUIDE.md" \
  "${REPO_ROOT}/docs/operator/PERFORMANCE_GUIDE.md" \
  "${REPO_ROOT}/docs/operator/TROUBLESHOOTING_GUIDE.md"

echo "[8/8] Supervision Tree"
build_pdf "macula-supervision-tree" \
  "${REPO_ROOT}/docs/guides/FULL_SUPERVISION_TREE.md"

# --- Combined Technical Report ---

echo ""
echo "[COMBINED] Macula Technical Report..."

build_pdf_with_toc "macula-technical-report" \
  "${REPO_ROOT}/README.md" \
  "${REPO_ROOT}/docs/GLOSSARY.md" \
  "${REPO_ROOT}/docs/guides/DIST_OVER_MESH_GUIDE.md" \
  "${REPO_ROOT}/docs/guides/PUBSUB_GUIDE.md" \
  "${REPO_ROOT}/docs/guides/RPC_GUIDE.md" \
  "${REPO_ROOT}/docs/guides/CLUSTERING_GUIDE.md" \
  "${REPO_ROOT}/docs/guides/DHT_GUIDE.md" \
  "${REPO_ROOT}/docs/guides/AUTHORIZATION_GUIDE.md" \
  "${REPO_ROOT}/docs/guides/CONTENT_TRANSFER_GUIDE.md" \
  "${REPO_ROOT}/docs/guides/PROTOCOL_GATEKEEPER_GUIDE.md" \
  "${REPO_ROOT}/docs/guides/MRI_GUIDE.md" \
  "${REPO_ROOT}/docs/guides/FULL_SUPERVISION_TREE.md" \
  "${REPO_ROOT}/docs/operator/TLS_GUIDE.md" \
  "${REPO_ROOT}/docs/operator/MONITORING_GUIDE.md" \
  "${REPO_ROOT}/docs/operator/PERFORMANCE_GUIDE.md" \
  "${REPO_ROOT}/docs/operator/TROUBLESHOOTING_GUIDE.md" \
  "${REPO_ROOT}/docs/ROADMAP.md"

echo ""
echo "=== Build complete ==="
ls -lh "${DIST_DIR}"/*.pdf 2>/dev/null || echo "  (no PDFs generated)"
