#!/usr/bin/env bash
#
# Three-netns macula-net Phase 2 demo — runs on a host with a
# working kernel (beam02), driven from anywhere over ssh by
# scripts/lan-demo.sh auto3.
#
# Topology:
#
#   bridge mb0 (10.99.0.254/24, root ns)
#         /         |          \
#       /           |            \
#   netA          netB          netC
#  10.99.0.1    10.99.0.2     10.99.0.3
#  macula0      macula0        macula0  (TUN — fd<realm>::a/b/c)
#
# Mock DHT runs in root ns on 10.99.0.254:5555. Each station's
# advertise_station calls the DHT over TCP; resolve_address looks
# up the same way.
#
# Acceptance (per PLAN_MACULA_NET §12 Phase 2):
#   * 3-station mesh forms (each can resolve the others)
#   * curl http://[B-addr]:8080/ from netns A (and from netns C)
#     reaches an HTTP server bound to B's macula-net address
#
# Subcommands:
#   run       full pipeline: setup → dht → start → curls → teardown
#   setup     bridge + 3 netns + 3 veth pairs + addresses
#   dht       start the mock DHT in root ns
#   start     spawn all three BEAM nodes
#   curls     curl from A→B and from C→B
#   stop      kill DHT + BEAMs
#   teardown  remove everything

set -euo pipefail

BR=mb0
BR_IP=10.99.0.254
NETNS_A=maculaA
NETNS_B=maculaB
NETNS_C=maculaC
DHT_PORT=5555
HTTPD_PORT=8080

SRC_DIR=${SRC_DIR:-${HOME}/macula-src}
ERL_BIN=${ERL_BIN:-${HOME}/.asdf/installs/erlang/26.0/bin/erl}
ERLC_BIN=${ERLC_BIN:-${HOME}/.asdf/installs/erlang/26.0/bin/erlc}

PIDFILE_DHT=/tmp/macula-netns3-dht.pid
PIDFILE_A=/tmp/macula-netns3-a.pid
PIDFILE_B=/tmp/macula-netns3-b.pid
PIDFILE_C=/tmp/macula-netns3-c.pid
PIDFILE_HTTPD=/tmp/macula-netns3-httpd.pid

LOG_DIR=/tmp/macula-netns3
mkdir -p "${LOG_DIR}"

erl_pa_args() {
    local out=()
    for d in "${SRC_DIR}"/_build/default/lib/*/ebin; do
        out+=(-pa "${d}")
    done
    printf '%s\n' "${out[@]}"
}

print_addr() {
    # Must match lan_demo_node3.erl: the seed feeds an Ed25519 key
    # derivation, the resulting *public* key is what we hash for the
    # macula-net address. Using the raw seed here would print a
    # different address (Phase 1 used the seed-as-pubkey shortcut).
    local hex
    case "$1" in a) hex=aa ;; b) hex=bb ;; c) hex=cc ;; esac
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

ensure_demo_compiled() {
    "${ERLC_BIN}" -pa "${SRC_DIR}/_build/default/lib/macula/ebin" \
        -o "${SRC_DIR}/_build/default/lib/macula/ebin" \
        "${SRC_DIR}/scripts/lan_demo_node3.erl" \
        "${SRC_DIR}/scripts/lan_demo_dht.erl"
}

setup() {
    sudo modprobe veth 2>/dev/null || true
    teardown_quietly

    sudo ip link add name "${BR}" type bridge
    sudo ip addr add "${BR_IP}/24" dev "${BR}"
    sudo ip link set "${BR}" up

    for role in a b c; do
        local ns; case "${role}" in a) ns=$NETNS_A ;; b) ns=$NETNS_B ;; c) ns=$NETNS_C ;; esac
        local v0=v${role}0 v1=v${role}1
        local ip="10.99.0.${role/a/1}"
        ip="${ip/b/2}"; ip="${ip/c/3}"
        # cleaner: explicit
        case "${role}" in a) ip=10.99.0.1 ;; b) ip=10.99.0.2 ;; c) ip=10.99.0.3 ;; esac

        sudo ip netns add "${ns}"
        sudo ip link add "${v0}" type veth peer name "${v1}"
        sudo ip link set "${v0}" netns "${ns}"
        sudo ip link set "${v1}" master "${BR}"
        sudo ip link set "${v1}" up
        sudo ip -n "${ns}" addr add "${ip}/24" dev "${v0}"
        sudo ip -n "${ns}" link set lo up
        sudo ip -n "${ns}" link set "${v0}" up
        sudo ip -n "${ns}" route add default via "${BR_IP}" 2>/dev/null || true
    done
    echo ">>> bridge + 3 netns ready"
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

start_node() {
    local role="$1"
    local ns; case "${role}" in a) ns=$NETNS_A ;; b) ns=$NETNS_B ;; c) ns=$NETNS_C ;; esac
    local pidfile log entry
    case "${role}" in
        a) pidfile=$PIDFILE_A; log=${LOG_DIR}/a.log; entry=start_a ;;
        b) pidfile=$PIDFILE_B; log=${LOG_DIR}/b.log; entry=start_b ;;
        c) pidfile=$PIDFILE_C; log=${LOG_DIR}/c.log; entry=start_c ;;
    esac
    : > "${log}"
    mapfile -t pa < <(erl_pa_args)
    sudo ip netns exec "${ns}" \
        env MACULA_DHT_HOST="${BR_IP}" MACULA_DHT_PORT="${DHT_PORT}" \
        "${ERL_BIN}" -noshell "${pa[@]}" -s lan_demo_node3 "${entry}" \
        > "${log}" 2>&1 &
    echo $! > "${pidfile}"
}

wait_for_up() {
    for log in "${LOG_DIR}/a.log" "${LOG_DIR}/b.log" "${LOG_DIR}/c.log"; do
        for _ in $(seq 1 40); do
            if grep -q "up; idling" "${log}" 2>/dev/null; then break; fi
            sleep 0.25
        done
        if ! grep -q "up; idling" "${log}" 2>/dev/null; then
            echo "node failed; tail of ${log}:"
            tail -25 "${log}"
            return 1
        fi
    done
    echo ">>> all three nodes up"
}

start_httpd_in_b() {
    local addr_b
    addr_b="$(print_addr b)"
    local doc_root=/tmp/macula-netns3-www
    sudo rm -rf "${doc_root}"
    sudo mkdir -p "${doc_root}"
    echo "macula-net phase2 ok" | sudo tee "${doc_root}/hello.txt" >/dev/null
    : > "${LOG_DIR}/httpd.log"
    sudo ip netns exec "${NETNS_B}" \
        python3 -m http.server "${HTTPD_PORT}" \
        --bind "${addr_b}" --directory "${doc_root}" \
        > "${LOG_DIR}/httpd.log" 2>&1 &
    echo $! > "${PIDFILE_HTTPD}"
    for _ in $(seq 1 40); do
        if sudo ip netns exec "${NETNS_B}" ss -ltn 2>/dev/null \
                | grep -q ":${HTTPD_PORT} "; then
            echo ">>> httpd bound on [${addr_b}]:${HTTPD_PORT}"
            return 0
        fi
        sleep 0.25
    done
    echo "httpd failed to bind"
    sudo cat "${LOG_DIR}/httpd.log"
    return 1
}

curl_one() {
    local from="$1" addr_b="$2"
    echo ">>> curl from ${from}"
    local body
    body="$(sudo ip netns exec "${from}" \
        curl --silent --show-error --fail --max-time 8 \
        "http://[${addr_b}]:${HTTPD_PORT}/hello.txt")"
    echo "    body: ${body}"
    if [ "${body}" != "macula-net phase2 ok" ]; then
        echo "    UNEXPECTED BODY"
        return 1
    fi
}

curls() {
    local addr_b
    addr_b="$(print_addr b)"
    start_httpd_in_b
    curl_one "${NETNS_A}" "${addr_b}"
    curl_one "${NETNS_C}" "${addr_b}"
    echo ">>> Phase 2 acceptance: 3-station mesh, A→B and C→B both pass"
}

stop_all() {
    for pf in "${PIDFILE_DHT}" "${PIDFILE_A}" "${PIDFILE_B}" "${PIDFILE_C}" "${PIDFILE_HTTPD}"; do
        if [ -f "${pf}" ]; then
            local pid; pid="$(cat "${pf}")"
            sudo kill "${pid}" 2>/dev/null || true
            rm -f "${pf}"
        fi
    done
}

teardown_quietly() {
    stop_all 2>/dev/null || true
    # Kill anything still living in the netns before deleting them
    # (otherwise `ip netns del` succeeds but the name lingers).
    for ns in "${NETNS_A}" "${NETNS_B}" "${NETNS_C}"; do
        sudo ip netns pids "${ns}" 2>/dev/null | xargs -r sudo kill -9 2>/dev/null || true
        sudo ip netns del "${ns}" 2>/dev/null || true
    done
    # veth pairs: deleting one end auto-deletes the other.
    for v in va0 va1 vb0 vb1 vc0 vc1; do
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
    start)    start_node a; start_node b; start_node c; wait_for_up ;;
    curls)    curls ;;
    stop)     stop_all ;;
    teardown) teardown ;;
    run)
        trap 'teardown' EXIT
        ensure_demo_compiled
        setup
        start_dht
        start_node a; start_node b; start_node c
        wait_for_up
        curls
        ;;
    *) echo "usage: $0 {run|setup|dht|start|curls|stop|teardown}" >&2; exit 2 ;;
esac
