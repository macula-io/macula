#!/bin/bash
# Measure macula's handshake under deliberate datagram loss, and its size on the wire.
#
# WHY IT EXISTS: msquic HARDCODES classical key exchange groups, and its own source comment says why — ML-KEM key
# shares make the Client/Server hello span multiple UDP datagrams, and their tests "hit a buffer space assertion
# failure" or "time out on loss recovery". Our stations offer ML-KEM-1024 hybrids and nothing else, so ours is the
# large handshake, and the question is whether OURS completes when a datagram carrying it is dropped.
#
# WHAT IT MEASURES: the size of every datagram in each direction, and whether the handshake completes under a
# deterministic drop policy. Results and caveats are recorded as V21 in
# plans/PLAN_POST_QUANTUM_SECURITY_PART1.md; the headline is that our client hello spans FIVE datagrams.
#
# THE INSTRUMENT: a user-space UDP relay between the client and the listener.
#   - No root, unlike netem, so it runs anywhere the suite runs.
#   - DETERMINISTIC: it drops the Nth datagram rather than a percentage, which is what "the second datagram of a
#     hello that spans five" requires and what a percentage cannot express.
#   - It sees every datagram, so the sizes and the loss results come from ONE run rather than two.
#
# ⚠ WHAT IT CANNOT TELL YOU, and do not let a green run say otherwise:
#   - Loopback is NOT a lossy link. No latency, no jitter, no reordering, no congestion, no MTU variation.
#   - A deterministic every-Nth drop is NOT bursty real-world loss, which is correlated and would likely be
#     worse for a multi-datagram flight.
#   - It exercises the QUIC/TLS handshake ONLY, not the macula CONNECT and HELLO that follow inside the
#     connection.
#   - One machine, one build. It reports counts, not a distribution.
set -eu

HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
ROOT="$(dirname "$HERE")"
OTP_BIN="${OTP_BIN:-/home/rl/.asdf/installs/erlang/28.4.2/bin}"
OUT="${OUT:-$(mktemp -d)}"

export PATH="$OTP_BIN:$PATH"
cd "$ROOT"

# The probe lives beside this script so the measurement and its instrument stay together.
erlc -o "$OUT" -pa _build/test/lib/macula/ebin "$HERE/lossy_handshake.erl"

erl -noshell \
    -config "$ROOT/config/test.sys.config" \
    -pa "$OUT" \
    -pa _build/test/lib/*/ebin \
    -eval 'lossy_handshake:run(), init:stop().'
