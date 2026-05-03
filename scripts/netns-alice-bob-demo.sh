#!/usr/bin/env bash
#
# Phase 3 §8 acceptance #4 — full Alice ↔ Bob daemon-to-daemon over
# four netns + a mock DHT in the root namespace. Same shape as
# netns-daemons-demo.sh but with two host stations and two daemons:
#
#   bridge mb0 (10.99.0.254/24, root ns) + mock DHT on :5555
#       │
#       ├── netns helsinki  (10.99.0.2)  — host station,  seed bb
#       ├── netns nuremberg (10.99.0.3)  — host station,  seed cc
#       ├── netns alice     (10.99.0.4)  — daemon, attached to helsinki
#       └── netns bob       (10.99.0.5)  — daemon, attached to nuremberg
#
# Both daemons send a CBOR data envelope to the other on startup:
#
#   alice  ─ attach stream ─▶ helsinki ─ Phase 2 ─▶ nuremberg ─ attach ─▶ bob
#   bob    ─ attach stream ─▶ nuremberg ─ Phase 2 ─▶ helsinki ─ attach ─▶ alice
#
# Acceptance: both alice.log and bob.log contain a "[daemon] received"
# line — that is, the headline §8 row #4 acceptance ("Two-daemon
# traffic ... exchange CBOR envelopes both directions") is met
# end-to-end with real QUIC, real netns, real kernel forwarding.

set -euo pipefail

BR=mb0
BR_IP=10.99.0.254
NETNS_H=maculaHelsinki
NETNS_N=maculaNuremberg
NETNS_A=maculaAlice
NETNS_B=maculaBob
DHT_PORT=5555

SRC_DIR=${SRC_DIR:-${HOME}/macula-src}
ERL_BIN=${ERL_BIN:-${HOME}/.asdf/installs/erlang/26.0/bin/erl}
ERLC_BIN=${ERLC_BIN:-${HOME}/.asdf/installs/erlang/26.0/bin/erlc}

PIDFILE_DHT=/tmp/macula-alicebob-dht.pid
PIDFILE_H=/tmp/macula-alicebob-helsinki.pid
PIDFILE_N=/tmp/macula-alicebob-nuremberg.pid
PIDFILE_A=/tmp/macula-alicebob-alice.pid
PIDFILE_B=/tmp/macula-alicebob-bob.pid

LOG_DIR=/tmp/macula-alicebob
mkdir -p "${LOG_DIR}"

erl_pa_args() {
    local out=()
    for d in "${SRC_DIR}"/_build/default/lib/*/ebin; do
        out+=(-pa "${d}")
    done
    printf '%s\n' "${out[@]}"
}

print_pubkey_hex_for_seed_hex() {
    local hex="$1"
    mapfile -t pa < <(erl_pa_args)
    "${ERL_BIN}" -noshell "${pa[@]}" -eval "
        Seed = <<16#${hex}:256>>,
        {Pub, _Priv} = crypto:generate_key(eddsa, ed25519, Seed),
        Pk = iolist_to_binary(Pub),
        io:format(\"~s\", [binary:encode_hex(Pk, lowercase)]),
        init:stop()
    "
}

# Seeds (must match lan_demo_host_node ?IDENT_HOST and the daemon
# module's MACULA_DAEMON_SEED_HEX wiring).
HELSINKI_SEED=bb
NUREMBERG_SEED=cc
ALICE_SEED=dd
BOB_SEED=ee

ensure_demo_compiled() {
    "${ERLC_BIN}" -pa "${SRC_DIR}/_build/default/lib/macula/ebin" \
        -o "${SRC_DIR}/_build/default/lib/macula/ebin" \
        "${SRC_DIR}/scripts/lan_demo_node3.erl" \
        "${SRC_DIR}/scripts/lan_demo_dht.erl" \
        "${SRC_DIR}/scripts/lan_demo_host_node.erl" \
        "${SRC_DIR}/scripts/lan_demo_daemon_node.erl"
}

setup() {
    sudo modprobe veth 2>/dev/null || true
    teardown_quietly

    sudo ip link add name "${BR}" type bridge
    sudo ip addr add "${BR_IP}/24" dev "${BR}"
    sudo ip link set "${BR}" up

    create_netns "${NETNS_H}" vh0 vh1 10.99.0.2
    create_netns "${NETNS_N}" vn0 vn1 10.99.0.3
    create_netns "${NETNS_A}" va0 va1 10.99.0.4
    create_netns "${NETNS_B}" vb0 vb1 10.99.0.5
    echo ">>> bridge + 4 netns ready"
}

create_netns() {
    local ns="$1" v0="$2" v1="$3" ip="$4"
    sudo ip netns add "${ns}"
    sudo ip link add "${v0}" type veth peer name "${v1}"
    sudo ip link set "${v0}" netns "${ns}"
    sudo ip link set "${v1}" master "${BR}"
    sudo ip link set "${v1}" up
    sudo ip -n "${ns}" addr add "${ip}/24" dev "${v0}"
    sudo ip -n "${ns}" link set lo up
    sudo ip -n "${ns}" link set "${v0}" up
    sudo ip -n "${ns}" route add default via "${BR_IP}" 2>/dev/null || true
}

start_dht() {
    : > "${LOG_DIR}/dht.log"
    mapfile -t pa < <(erl_pa_args)
    "${ERL_BIN}" -noshell "${pa[@]}" -eval "
        application:ensure_all_started(crypto),
        {ok, _} = lan_demo_dht:start(${DHT_PORT}),
        receive _ -> ok end
    " > "${LOG_DIR}/dht.log" 2>&1 &
    echo $! > "${PIDFILE_DHT}"
    sleep 0.3
}

start_host() {
    local role="$1" ns="$2" pidfile="$3" log="$4" seed="$5" ip="$6" \
          metrics_port="$7"
    : > "${log}"
    mapfile -t pa < <(erl_pa_args)
    sudo ip netns exec "${ns}" \
        env MACULA_DHT_HOST="${BR_IP}" MACULA_DHT_PORT="${DHT_PORT}" \
            MACULA_HOST_VETH_IP="${ip}" \
            MACULA_HOST_SEED_HEX="${seed}" \
            MACULA_METRICS_PORT="${metrics_port}" \
            MACULA_TUN_MTU="${MACULA_TUN_MTU:-1280}" \
        "${ERL_BIN}" -noshell "${pa[@]}" -s lan_demo_host_node start \
        > "${log}" 2>&1 &
    echo $! > "${pidfile}"
    : "${role}"
}

start_helsinki() {
    start_host helsinki "${NETNS_H}" "${PIDFILE_H}" \
               "${LOG_DIR}/helsinki.log" "${HELSINKI_SEED}" 10.99.0.2 9145
}

start_nuremberg() {
    start_host nuremberg "${NETNS_N}" "${PIDFILE_N}" \
               "${LOG_DIR}/nuremberg.log" "${NUREMBERG_SEED}" 10.99.0.3 9146
}

start_daemon() {
    local role="$1" ns="$2" pidfile="$3" log="$4" seed="$5" \
          host_seed="$6" host_ip="$7" peer_seed="$8" send_delay="$9"
    local host_pk_hex
    host_pk_hex="$(print_pubkey_hex_for_seed_hex "${host_seed}")"
    : > "${log}"
    mapfile -t pa < <(erl_pa_args)
    sudo ip netns exec "${ns}" \
        env MACULA_HOST_HOST="${host_ip}" MACULA_HOST_PORT=4400 \
            MACULA_HOST_PUBKEY_HEX="${host_pk_hex}" \
            MACULA_DAEMON_SEED_HEX="${seed}" \
            MACULA_DAEMON_PEER_SEED_HEX="${peer_seed}" \
            MACULA_DAEMON_SEND_DELAY_MS="${send_delay}" \
            MACULA_DAEMON_PAYLOAD_BYTES="${MACULA_DAEMON_PAYLOAD_BYTES:-22}" \
        "${ERL_BIN}" -noshell "${pa[@]}" -s lan_demo_daemon_node start \
        > "${log}" 2>&1 &
    echo $! > "${pidfile}"
    : "${role}"
}

start_alice() {
    # Alice attaches to Helsinki, sends to Bob after a 2s delay so
    # both host stations have advertised their endpoints first.
    start_daemon alice "${NETNS_A}" "${PIDFILE_A}" \
                  "${LOG_DIR}/alice.log" \
                  "${ALICE_SEED}" "${HELSINKI_SEED}" 10.99.0.2 \
                  "${BOB_SEED}" 2000
}

start_bob() {
    start_daemon bob "${NETNS_B}" "${PIDFILE_B}" \
                  "${LOG_DIR}/bob.log" \
                  "${BOB_SEED}" "${NUREMBERG_SEED}" 10.99.0.3 \
                  "${ALICE_SEED}" 2000
}

wait_for_up() {
    wait_for_pattern "${LOG_DIR}/helsinki.log"  "up; idling"
    wait_for_pattern "${LOG_DIR}/nuremberg.log" "up; idling"
    wait_for_pattern "${LOG_DIR}/alice.log"     "up; idling"
    wait_for_pattern "${LOG_DIR}/bob.log"       "up; idling"
    echo ">>> helsinki + nuremberg + alice + bob all up"
}

wait_for_pattern() {
    local log="$1" pat="$2"
    for _ in $(seq 1 80); do
        if grep -q "${pat}" "${log}" 2>/dev/null; then return 0; fi
        sleep 0.25
    done
    echo "node failed; tail of ${log}:"
    tail -30 "${log}"
    return 1
}

verify() {
    # Each daemon scheduled a send 2s after startup. Give the round-
    # trip (alice -> helsinki -> nuremberg -> bob and reverse) a few
    # seconds to land. The acceptance signal is "[daemon] received"
    # in BOTH alice.log and bob.log.
    #
    # Larger payloads (MACULA_DAEMON_PAYLOAD_BYTES > 22) take longer.
    # Scale the wait by ~1s per 256 KiB of payload.
    local extra_wait=0
    if [ "${MACULA_DAEMON_PAYLOAD_BYTES:-22}" -gt 22 ]; then
        extra_wait=$(( (MACULA_DAEMON_PAYLOAD_BYTES + 262143) / 262144 ))
    fi
    sleep $((4 + extra_wait))
    local fail=0
    if grep -q "\[daemon\] received" "${LOG_DIR}/alice.log"; then
        echo ">>> alice received:"
        grep "\[daemon\] received" "${LOG_DIR}/alice.log"
    else
        echo "FAIL: no [daemon] received line in alice.log"
        fail=1
    fi
    if grep -q "\[daemon\] received" "${LOG_DIR}/bob.log"; then
        echo ">>> bob received:"
        grep "\[daemon\] received" "${LOG_DIR}/bob.log"
    else
        echo "FAIL: no [daemon] received line in bob.log"
        fail=1
    fi
    if [ ${fail} -ne 0 ]; then
        echo "--- alice.log tail ---";     tail -30 "${LOG_DIR}/alice.log"
        echo "--- bob.log tail ---";       tail -30 "${LOG_DIR}/bob.log"
        echo "--- helsinki.log tail ---";  tail -30 "${LOG_DIR}/helsinki.log"
        echo "--- nuremberg.log tail ---"; tail -30 "${LOG_DIR}/nuremberg.log"
        return 1
    fi
    echo ">>> Phase 3 §8 #4 acceptance: Alice ↔ Bob both directions"
    verify_payload_integrity
    verify_metrics
    return 0
}

verify_payload_integrity() {
    # Phase 4.2 §5 #6 — for any non-default payload size, sender and
    # receiver compute sha8 of the payload. Sender prints "sent ...
    # sha8=X"; receiver prints "received ... sha8=Y". For a successful
    # MTU run, the sha8 sent by alice MUST equal the sha8 received by
    # bob (and vice-versa).
    local size="${MACULA_DAEMON_PAYLOAD_BYTES:-22}"
    [ "${size}" -le 22 ] && return 0
    set +e
    local alice_sent bob_recv bob_sent alice_recv
    alice_sent="$(grep -oE 'sha8=[0-9a-f]+' "${LOG_DIR}/alice.log" | grep -m1 sha8)"
    bob_recv="$(grep -oE 'sha8=[0-9a-f]+'   "${LOG_DIR}/bob.log"   | grep -m1 sha8)"
    bob_sent="$(grep -oE 'sha8=[0-9a-f]+'   "${LOG_DIR}/bob.log"   | sed -n 2p)"
    alice_recv="$(grep -oE 'sha8=[0-9a-f]+' "${LOG_DIR}/alice.log" | sed -n 2p)"
    set -e
    echo ">>> alice sent  ${alice_sent}, bob received ${bob_recv}"
    echo ">>> bob sent    ${bob_sent}, alice received ${alice_recv}"
    if [ "${alice_sent}" = "${bob_recv}" ] && [ "${bob_sent}" = "${alice_recv}" ]; then
        echo ">>> Phase 4.2 §5 #6 acceptance: ${size}-byte payload sha8 round-trip OK"
        return 0
    fi
    echo "FAIL: sha8 mismatch in payload integrity check"
    return 1
}

verify_metrics() {
    # Phase 4.1 §8 #6 — scrape Prometheus endpoints exposed by both
    # host stations. Helsinki on 10.99.0.2:9145, Nuremberg on
    # 10.99.0.3:9146. Both bind in their own netns; root ns reaches
    # them via the bridge-routed veths.
    #
    # Locally relax `set -e` because grep returning 1 (no-match) inside
    # command substitution would otherwise drag the function down.
    set +e
    echo ">>> verifying metrics endpoints"
    local fail=0
    local label name port ip body egress ingress
    for label in helsinki:9145:10.99.0.2 nuremberg:9146:10.99.0.3; do
        name="${label%%:*}"
        port="$(printf '%s' "${label}" | cut -d: -f2)"
        ip="${label##*:}"
        body="$(curl -s --max-time 3 "http://${ip}:${port}/metrics")"
        if [ -z "${body}" ]; then
            echo "FAIL: ${name} /metrics empty or unreachable (${ip}:${port})"
            fail=1
            continue
        fi
        egress="$(printf '%s\n' "${body}" \
                  | grep -E 'macula_net_envelopes_forwarded_total.*direction="egress"' \
                  | head -1)"
        ingress="$(printf '%s\n' "${body}" \
                   | grep -E 'macula_net_envelopes_forwarded_total.*direction="ingress"' \
                   | head -1)"
        relay="$(printf '%s\n' "${body}" \
                 | grep -E 'macula_net_envelopes_forwarded_total.*direction="relay"' \
                 | head -1)"
        echo ">>> ${name} envelopes_forwarded_total samples:"
        [ -n "${egress}" ]  && printf '  %s\n' "${egress}"
        [ -n "${ingress}" ] && printf '  %s\n' "${ingress}"
        [ -n "${relay}" ]   && printf '  %s\n' "${relay}"
        if [ -z "${relay}" ]; then
            echo "FAIL: ${name} no relay sample (expected for daemon-via-host)"
            fail=1
        fi
        path_mtu="$(printf '%s\n' "${body}" \
                    | grep -E '^macula_net_transport_path_mtu_bytes' \
                    | head -1)"
        echo ">>> ${name} transport_path_mtu_bytes:"
        [ -n "${path_mtu}" ] && printf '  %s\n' "${path_mtu}" \
            || echo "  <none yet — needs >5s uptime>"
    done
    set -e
    if [ ${fail} -ne 0 ]; then
        echo "FAIL: Phase 4.1 §8 #6 metrics verification failed"
        return 1
    fi
    echo ">>> Phase 4.1 §8 #6 acceptance: metrics scraped from both hosts"
}

stop_all() {
    for pf in "${PIDFILE_DHT}" "${PIDFILE_H}" "${PIDFILE_N}" "${PIDFILE_A}" "${PIDFILE_B}"; do
        if [ -f "${pf}" ]; then
            local pid; pid="$(cat "${pf}")"
            sudo kill "${pid}" 2>/dev/null || true
            rm -f "${pf}"
        fi
    done
}

teardown_quietly() {
    stop_all 2>/dev/null || true
    for ns in "${NETNS_H}" "${NETNS_N}" "${NETNS_A}" "${NETNS_B}"; do
        sudo ip netns pids "${ns}" 2>/dev/null | xargs -r sudo kill -9 2>/dev/null || true
        sudo ip netns del "${ns}" 2>/dev/null || true
    done
    for v in vh0 vh1 vn0 vn1 va0 va1 vb0 vb1; do
        sudo ip link del "${v}" 2>/dev/null || true
    done
    sudo ip link del "${BR}" 2>/dev/null || true
}

teardown() {
    teardown_quietly
    echo ">>> torn down"
}

cmd="${1:-run}"; shift || true
case "${cmd}" in
    setup)    setup ;;
    dht)      start_dht ;;
    start)    start_helsinki; start_nuremberg; start_alice; start_bob; wait_for_up ;;
    verify)   verify ;;
    stop)     stop_all ;;
    teardown) teardown ;;
    run)
        trap 'teardown' EXIT
        ensure_demo_compiled
        setup
        start_dht
        start_helsinki
        start_nuremberg
        start_alice
        start_bob
        wait_for_up
        verify
        ;;
    *)
        echo "usage: $0 {run|setup|dht|start|verify|stop|teardown}" >&2
        exit 2
        ;;
esac
