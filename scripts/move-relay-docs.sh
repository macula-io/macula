#!/bin/bash
# Move relay-specific docs from macula SDK to macula-relay.
set -eu

SDK="/home/rl/work/github.com/macula-io/macula"
RELAY="/home/rl/work/github.com/macula-io/macula-relay"

# Create target directories in relay
mkdir -p "${RELAY}/docs/operator"
mkdir -p "${RELAY}/docs/guides"

# Operator guides — all relay concerns
for f in TLS_GUIDE.md MONITORING_GUIDE.md PERFORMANCE_GUIDE.md TROUBLESHOOTING_GUIDE.md MDNS_SETUP.md; do
    if [ -f "${SDK}/docs/operator/${f}" ]; then
        cp "${SDK}/docs/operator/${f}" "${RELAY}/docs/operator/${f}"
        rm "${SDK}/docs/operator/${f}"
        echo "Moved operator/${f}"
    fi
done

# Guides that belong in relay
RELAY_GUIDES=(
    DHT_GUIDE.md
    CONTENT_TRANSFER_GUIDE.md
    FULL_SUPERVISION_TREE.md
    mri_resource_lifecycle.md
)

for f in "${RELAY_GUIDES[@]}"; do
    if [ -f "${SDK}/docs/guides/${f}" ]; then
        cp "${SDK}/docs/guides/${f}" "${RELAY}/docs/guides/${f}"
        rm "${SDK}/docs/guides/${f}"
        echo "Moved guides/${f}"
    fi
done

# Remove empty operator dir if all files moved
rmdir "${SDK}/docs/operator" 2>/dev/null && echo "Removed empty docs/operator/" || true

# Remove duplicate mri.md (MRI_GUIDE.md is the canonical one)
if [ -f "${SDK}/docs/guides/mri.md" ]; then
    rm "${SDK}/docs/guides/mri.md"
    echo "Removed duplicate mri.md (MRI_GUIDE.md is canonical)"
fi

echo ""
echo "SDK docs remaining:"
find "${SDK}/docs" -name "*.md" | sort
echo ""
echo "Relay docs added:"
find "${RELAY}/docs" -name "*.md" | sort
