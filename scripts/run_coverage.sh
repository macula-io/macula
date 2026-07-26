#!/bin/bash
# Test Coverage Report Generator for Macula
# Runs tests with coverage analysis and generates HTML reports

set -e

PROJECT_ROOT="/home/rl/work/github.com/macula-io/macula"
COVERAGE_DIR="${PROJECT_ROOT}/_build/test/cover"

cd "${PROJECT_ROOT}"

echo "==> Cleaning previous build artifacts..."
rebar3 clean

echo ""
echo "==> Compiling project..."
rebar3 compile

echo ""
echo "==> Running EUnit tests with coverage..."
rebar3 eunit --cover

echo ""
echo "==> Generating coverage report..."
rebar3 cover --verbose

echo ""
echo "==> Coverage Summary:"
echo "--------------------"

# Parse coverage data from .coverdata files
if [ -d "${COVERAGE_DIR}" ]; then
    echo "Coverage reports generated in: ${COVERAGE_DIR}"
    echo ""

    # Try to extract summary from verbose output
    if [ -f "${COVERAGE_DIR}/index.html" ]; then
        echo "HTML report available at: ${COVERAGE_DIR}/index.html"
        echo "Open with: firefox ${COVERAGE_DIR}/index.html"
    fi
else
    echo "Warning: Coverage directory not found at ${COVERAGE_DIR}"
fi

echo ""
echo "==> Coverage analysis complete!"
echo ""
echo "To view detailed coverage:"
echo "  1. Open ${COVERAGE_DIR}/index.html in a browser"
echo "  2. Or run: firefox ${COVERAGE_DIR}/index.html"
echo ""
