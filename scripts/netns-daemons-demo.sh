#!/usr/bin/env bash
#
# Phase 3 hosted-identity gateway demo — runs on a host with a working
# kernel (beam02), driven from anywhere over ssh by lan-demo.sh
# auto-daemons.
#
# Topology:
#
#   bridge mb0 (10.99.0.254/24, root ns) + mock DHT on :5555
#       /            |             \
#     /              |               \
#   netS           netH              netD
#   10.99.0.1      10.99.0.2          10.99.0.3
#   sender          helsinki           alice
#   macula0 (TUN)   macula0 (TUN)      no TUN — daemon
#
# Acceptance (per PLAN_MACULA_NET_PHASE3 §8 + milestone 3.7):
#   * Alice attaches to Helsinki, the host publishes Alice's
#     hosted_address_map record into the DHT.
#   * Sender constructs an IPv6 packet dst=Alice via the kernel
#     (curl --max-time 2 to fd<realm>::alice). The TUN reader picks
#     it up; route_packet resolves Alice through the DHT, finds the
#     hosted_address_map, follows the redirect to Helsinki's
#     station_endpoint, and ships a data-envelope to Helsinki.
#   * Helsinki's host_attach_controller dispatches the envelope on
#     Alice's attach stream.
#   * Alice's daemon logs the inbound envelope:
#         [daemon] received <src> -> <alice-addr>, N byte(s)
#
# Subcommands:
#   run       full pipeline: setup → dht → start nodes → trigger → teardown
#   setup     bridge + 3 netns + 3 veth pairs + addresses
#   dht       start the mock DHT in root ns
#   start     spawn the three BEAM nodes (sender, helsinki, alice)
#   trigger   curl from sender → alice (acceptance signal)
#   verify    grep alice's log for the receive line
#   stop      kill DHT + BEAMs
#   teardown  remove everything

set -euo pipefail

BR=mb0
BR_IP=10.99.0.254
NETNS_S=maculaSender
NETNS_H=maculaHelsinki
NETNS_D=maculaAlice
DHT_PORT=5555
TUN=macula0

SRC_DIR=${SRC_DIR:-${HOME}/macula-src}
ERL_BIN=${ERL_BIN:-${HOME}/.asdf/installs/erlang/26.0/bin/erl}
ERLC_BIN=${ERLC_BIN:-${HOME}/.asdf/installs/erlang/26.0/bin/erlc}

PIDFILE_DHT=/tmp/macula-daemons-dht.pid
PIDFILE_S=/tmp/macula-daemons-sender.pid
PIDFILE_H=/tmp/macula-daemons-helsinki.pid
PIDFILE_D=/tmp/macula-daemons-alice.pid

LOG_DIR=/tmp/macula-daemons
mkdir -p "${LOG_DIR}"

erl_pa_args() {
    local out=()
    for d in "${SRC_DIR}"/_build/default/lib/*/ebin; do
        out+=(-pa "${d}")
    done
    printf '%s\n' "${out[@]}"
}

print_addr_for_seed_hex() {
    local hex="$1"
    mapfile -t pa < <(erl_pa_args)
    "${ERL_BIN}" -noshell "${pa[@]}" -eval "
        Realm = <<16#11:256>>,
        Seed = <<16#${hex}:256>>,
        {Pub, _Priv} = crypto:generate_key(eddsa, ed25519, Seed),
        Pk = iolist_to_binary(Pub),
        io:format(\"~s\", [macula_address:format(macula_address:derive(Realm, Pk))]),
        init:stop()
    "
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

# Seeds (must match the demo node modules):
#   sender    = aa  (lan_demo_node3:start_a)
#   helsinki  = bb  (lan_demo_host_node)
#   alice     = dd  (lan_demo_daemon_node)
SENDER_SEED=aa
HOST_SEED=bb
DAEMON_SEED=dd

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

    create_netns "${NETNS_S}" vs0 vs1 10.99.0.1
    create_netns "${NETNS_H}" vh0 vh1 10.99.0.2
    create_netns "${NETNS_D}" vd0 vd1 10.99.0.3
    echo ">>> bridge + 3 netns ready"
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

start_sender() {
    : > "${LOG_DIR}/sender.log"
    mapfile -t pa < <(erl_pa_args)
    sudo ip netns exec "${NETNS_S}" \
        env MACULA_DHT_HOST="${BR_IP}" MACULA_DHT_PORT="${DHT_PORT}" \
        "${ERL_BIN}" -noshell "${pa[@]}" -s lan_demo_node3 start_a \
        > "${LOG_DIR}/sender.log" 2>&1 &
    echo $! > "${PIDFILE_S}"
}

start_helsinki() {
    : > "${LOG_DIR}/helsinki.log"
    mapfile -t pa < <(erl_pa_args)
    sudo ip netns exec "${NETNS_H}" \
        env MACULA_DHT_HOST="${BR_IP}" MACULA_DHT_PORT="${DHT_PORT}" \
            MACULA_HOST_VETH_IP=10.99.0.2 \
        "${ERL_BIN}" -noshell "${pa[@]}" -s lan_demo_host_node start \
        > "${LOG_DIR}/helsinki.log" 2>&1 &
    echo $! > "${PIDFILE_H}"
}

start_alice() {
    local host_pk_hex
    host_pk_hex="$(print_pubkey_hex_for_seed_hex ${HOST_SEED})"
    : > "${LOG_DIR}/alice.log"
    mapfile -t pa < <(erl_pa_args)
    sudo ip netns exec "${NETNS_D}" \
        env MACULA_HOST_HOST=10.99.0.2 MACULA_HOST_PORT=4400 \
            MACULA_HOST_PUBKEY_HEX="${host_pk_hex}" \
        "${ERL_BIN}" -noshell "${pa[@]}" -s lan_demo_daemon_node start \
        > "${LOG_DIR}/alice.log" 2>&1 &
    echo $! > "${PIDFILE_D}"
}

wait_for_up() {
    wait_for_pattern "${LOG_DIR}/sender.log"   "up; idling"
    wait_for_pattern "${LOG_DIR}/helsinki.log" "up; idling"
    wait_for_pattern "${LOG_DIR}/alice.log"    "up; idling"
    echo ">>> sender + helsinki + alice all up"
}

wait_for_pattern() {
    local log="$1" pat="$2"
    for _ in $(seq 1 60); do
        if grep -q "${pat}" "${log}" 2>/dev/null; then return 0; fi
        sleep 0.25
    done
    echo "node failed; tail of ${log}:"
    tail -30 "${log}"
    return 1
}

trigger() {
    local addr_alice
    addr_alice="$(print_addr_for_seed_hex ${DAEMON_SEED})"
    echo ">>> sending probe from sender to alice (${addr_alice})"
    # curl will time out (Alice has no HTTP listener), but the IPv6
    # packet flows: sender TUN -> route_packet -> DHT lookup -> Helsinki
    # -> controller -> Alice's attach stream. Alice logs the inbound
    # envelope. --max-time short so we don't hang the demo.
    sudo ip netns exec "${NETNS_S}" \
        curl --silent --show-error --max-time 3 \
        "http://[${addr_alice}]:8080/" 2>/dev/null || true
    sleep 0.5
}

verify() {
    if grep -q "\[daemon\] received" "${LOG_DIR}/alice.log"; then
        echo ">>> Phase 3.7 acceptance:"
        grep "\[daemon\] received" "${LOG_DIR}/alice.log"
        return 0
    fi
    echo "FAIL: no [daemon] received line in alice.log"
    echo "--- alice.log tail ---"
    tail -30 "${LOG_DIR}/alice.log"
    echo "--- helsinki.log tail ---"
    tail -30 "${LOG_DIR}/helsinki.log"
    echo "--- sender.log tail ---"
    tail -30 "${LOG_DIR}/sender.log"
    return 1
}

stop_all() {
    for pf in "${PIDFILE_DHT}" "${PIDFILE_S}" "${PIDFILE_H}" "${PIDFILE_D}"; do
        if [ -f "${pf}" ]; then
            local pid; pid="$(cat "${pf}")"
            sudo kill "${pid}" 2>/dev/null || true
            rm -f "${pf}"
        fi
    done
}

teardown_quietly() {
    stop_all 2>/dev/null || true
    for ns in "${NETNS_S}" "${NETNS_H}" "${NETNS_D}"; do
        sudo ip netns pids "${ns}" 2>/dev/null | xargs -r sudo kill -9 2>/dev/null || true
        sudo ip netns del "${ns}" 2>/dev/null || true
    done
    for v in vs0 vs1 vh0 vh1 vd0 vd1; do
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
    start)    start_sender; start_helsinki; start_alice; wait_for_up ;;
    trigger)  trigger ;;
    verify)   verify ;;
    stop)     stop_all ;;
    teardown) teardown ;;
    run)
        trap 'teardown' EXIT
        ensure_demo_compiled
        setup
        start_dht
        start_sender
        start_helsinki
        start_alice
        wait_for_up
        trigger
        verify
        ;;
    *)
        echo "usage: $0 {run|setup|dht|start|trigger|verify|stop|teardown}" >&2
        exit 2
        ;;
esac
