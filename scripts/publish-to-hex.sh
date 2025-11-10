#!/usr/bin/env bash

set -euo pipefail

# Publish macula package to hex.pm
# This script handles the complete publishing workflow with safety checks

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Source secrets if available
if [ -f "$HOME/.config/zshrc/01-secrets" ]; then
    source "$HOME/.config/zshrc/01-secrets"
fi

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}✓${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}⚠${NC} $1"
}

log_error() {
    echo -e "${RED}✗${NC} $1"
}

# Change to project root
cd "$PROJECT_ROOT"

# Get current version
MACULA_VERSION=$(grep -oP '(?<={vsn, ")[^"]+' apps/macula/src/macula.app.src || echo "unknown")
CLIENT_VERSION=$(grep -oP '(?<={vsn, ")[^"]+' apps/macula_client/src/macula_client.app.src || echo "unknown")

echo ""
echo "═══════════════════════════════════════════════"
echo "  Publishing Macula to Hex.pm"
echo "═══════════════════════════════════════════════"
echo ""
echo "Version: $MACULA_VERSION"
echo ""

# Validate versions match
if [ "$MACULA_VERSION" != "$CLIENT_VERSION" ]; then
    log_error "Version mismatch!"
    echo "  macula:        $MACULA_VERSION"
    echo "  macula_client: $CLIENT_VERSION"
    echo ""
    echo "Run: ./scripts/bump-version.sh <version> to sync versions"
    exit 1
fi

log_info "Version check passed (macula and macula_client both at $MACULA_VERSION)"

# Check for uncommitted changes
if ! git diff-index --quiet HEAD --; then
    log_error "Uncommitted changes detected"
    echo ""
    git status --short
    echo ""
    echo "Commit or stash changes before publishing"
    exit 1
fi

log_info "Git working tree is clean"

# Check we're authenticated with hex
log_warn "Checking hex.pm authentication..."

# Check if HEX_API_KEY is set
if [ -n "${HEX_API_KEY:-}" ]; then
    log_info "Authenticated via HEX_API_KEY environment variable"
    HEXPM_USER="API Key"
else
    # Check interactive authentication
    if ! rebar3 hex user whoami > /dev/null 2>&1; then
        log_error "Not authenticated with hex.pm"
        echo ""
        echo "Run: rebar3 hex user auth"
        echo "Or set HEX_API_KEY environment variable"
        exit 1
    fi
    HEXPM_USER=$(rebar3 hex user whoami 2>/dev/null | grep -oP '(?<=Username: ).*' || echo "authenticated user")
    log_info "Authenticated as: $HEXPM_USER"
fi

# Run tests
log_warn "Running tests..."
rebar3 eunit > /tmp/macula-test-output.log 2>&1

# Extract test results (rebar3 may return non-zero even for passing tests with expected crashes)
TEST_COUNT=$(grep -oP '\d+(?= tests)' /tmp/macula-test-output.log | head -1 || echo "0")
FAIL_COUNT=$(grep -oP '\d+(?= failures)' /tmp/macula-test-output.log | head -1 || echo "0")
CANCEL_COUNT=$(grep -oP '\d+(?= cancelled)' /tmp/macula-test-output.log | head -1 || echo "0")

# Check if we got test results at all
if [ "$TEST_COUNT" -eq 0 ]; then
    log_error "Failed to run tests (no test output detected)"
    echo ""
    tail -50 /tmp/macula-test-output.log
    exit 1
fi

# Check for actual test failures (not expected crashes from error handling tests)
if [ "$FAIL_COUNT" -gt 0 ]; then
    log_error "$FAIL_COUNT test(s) failed"
    echo ""
    tail -50 /tmp/macula-test-output.log
    exit 1
fi

log_info "$TEST_COUNT tests passed ($CANCEL_COUNT cancelled - expected)"

# Build hex package
log_warn "Building hex package..."
if ! rebar3 hex build --app macula > /tmp/macula-hex-build.log 2>&1; then
    log_error "Hex build failed!"
    echo ""
    tail -50 /tmp/macula-hex-build.log
    exit 1
fi

PACKAGE_PATH="_build/default/lib/macula/hex/macula-${MACULA_VERSION}.tar"
DOCS_PATH="_build/default/lib/macula/hex/macula-${MACULA_VERSION}-docs.tar"

if [ ! -f "$PACKAGE_PATH" ]; then
    log_error "Package tarball not found: $PACKAGE_PATH"
    exit 1
fi

PACKAGE_SIZE=$(du -h "$PACKAGE_PATH" | cut -f1)
log_info "Package built: $PACKAGE_SIZE"

if [ -f "$DOCS_PATH" ]; then
    DOCS_SIZE=$(du -h "$DOCS_PATH" | cut -f1)
    log_info "Docs built: $DOCS_SIZE"
fi

# Final confirmation
echo ""
echo "═══════════════════════════════════════════════"
echo "  Ready to Publish"
echo "═══════════════════════════════════════════════"
echo ""
echo "Package:    macula"
echo "Version:    $MACULA_VERSION"
echo "Size:       $PACKAGE_SIZE (package) + $DOCS_SIZE (docs)"
echo "User:       $HEXPM_USER"
echo ""
echo "This will publish to hex.pm (PUBLIC repository)"
echo ""

read -p "Continue with publish? (yes/no): " -r CONFIRM
echo ""

if [ "$CONFIRM" != "yes" ]; then
    log_warn "Publish cancelled"
    exit 0
fi

# Publish to hex.pm
log_warn "Publishing to hex.pm..."
echo ""

if rebar3 hex publish --app macula --yes; then
    echo ""
    log_info "Successfully published macula $MACULA_VERSION to hex.pm"
    echo ""
    echo "Package URL: https://hex.pm/packages/macula/$MACULA_VERSION"
    echo "Docs URL:    https://hexdocs.pm/macula/$MACULA_VERSION"
    echo ""
    echo "Next steps:"
    echo "  1. Verify package on hex.pm"
    echo "  2. Tag release: git tag -a v$MACULA_VERSION -m 'Release $MACULA_VERSION'"
    echo "  3. Push tags: git push origin v$MACULA_VERSION"
    echo "  4. Update CHANGELOG.md with release notes"
    echo ""
else
    log_error "Publish failed!"
    echo ""
    echo "Check the output above for errors"
    echo "Common issues:"
    echo "  - Version already published (increment version)"
    echo "  - Authentication expired (run: rebar3 hex user auth)"
    echo "  - Network connectivity issues"
    exit 1
fi
