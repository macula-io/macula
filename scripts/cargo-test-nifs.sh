#!/usr/bin/env bash
#
# Run the Rust tests of the NIF crates.
#
# ⚠ WHY THIS IS A SCRIPT AND NOT `.cargo/config.toml`.
#
# A NIF crate is a `cdylib` whose `enif_*` symbols the BEAM supplies when it
# loads the library. A `cargo test` harness is an ordinary executable with no
# BEAM behind it, and the dynamic loader binds some of those symbols AT LOAD
# rather than lazily, so the test binary dies with `symbol lookup error`
# before a single test runs. The link argument below is what lets it start;
# the stubs in `src/lib.rs` (all of which abort) are what let it stay up.
#
# The obvious home for that argument is `[build] rustflags` in
# `.cargo/config.toml`. IT IS THE WRONG HOME. An environment `RUSTFLAGS`
# REPLACES config rustflags entirely rather than adding to them, and the
# container image builds already export
# `RUSTFLAGS="-C target-feature=-crt-static"`. The config value would be
# silently dropped in exactly the environment where it matters, with nothing
# to say so. A script that composes the whole flag string cannot be shadowed
# that way.
#
# So: if you are about to "simplify" this into `.cargo/config.toml`, the
# answer is no, and this paragraph is why.
set -euo pipefail

cd "$(dirname "$0")/.."

# Compose rather than replace, so an environment that already sets RUSTFLAGS
# (an image build, a cross compile) keeps what it set.
export RUSTFLAGS="${RUSTFLAGS:-} -C link-arg=-Wl,--unresolved-symbols=ignore-all"

# ⚠ ONLY THE CRATES THAT HAVE BOTH TESTS AND THE STUBS TO RUN THEM.
#
# Every NIF crate here has the same structural barrier, and as of this
# writing only macula_quic has the stubs that clear it. The others carry NO
# RUST TESTS AT ALL -- not because anyone decided against them, but because
# `cargo test` has never been able to start in this repository, so nobody
# ever saw the option. `grep -c "cfg(test)" native/*/src/*.rs` is zero
# everywhere else.
#
# Add a crate to this list when it grows tests, and give it the stub module
# at the same time. Do not widen this to `native/*/`: a crate with no tests
# and no stubs fails at load, and a loop that fails on an empty crate teaches
# a reader to ignore the script.
CRATES="native/macula_quic"

for crate in $CRATES; do
    echo "=== cargo test: ${crate} ==="
    ( cd "$crate" && cargo test --lib "$@" )
done
