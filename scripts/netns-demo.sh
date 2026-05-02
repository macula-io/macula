#!/usr/bin/env bash
#
# Self-contained two-netns macula-net demo. Designed to run *on* a
# host with a working kernel (beam02), driven from anywhere over ssh.
#
# Topology:
#   netns maculaA (10.99.0.1)  <-veth->  netns maculaB (10.99.0.2)
#   each has macula0 TUN; ping6 from A to B's macula-net addr.
#
# Subcommands:
#   run    setup → start nodes → ping → curl → teardown (default)
#   setup  netns + veth + addresses
#   start  background-spawn both BEAM nodes
#   ping   ping -6 from A to B's macula-net addr
#   curl   spin python http.server in B, curl from A
#   stop   kill both BEAMs (and http.server)
#   teardown remove netns + veth

set -euo pipefail

NETNS_A=maculaA
NETNS_B=maculaB
VETH_A=veth-mA
VETH_B=veth-mB
VETH_A_IP=10.99.0.1
VETH_B_IP=10.99.0.2
PREFIX=24

SRC_DIR=${SRC_DIR:-${HOME}/macula-src}
ERL_BIN=${ERL_BIN:-${HOME}/.asdf/installs/erlang/26.0/bin/erl}
PIDFILE_A=/tmp/macula-netns-a.pid
PIDFILE_B=/tmp/macula-netns-b.pid
LOG_A=/tmp/macula-netns-a.log
LOG_B=/tmp/macula-netns-b.log
HTTPD_PIDFILE=/tmp/macula-netns-httpd.pid
HTTPD_LOG=/tmp/macula-netns-httpd.log
HTTPD_PORT=8080

erl_pa_args() {
    local out=()
    for d in "${SRC_DIR}"/_build/default/lib/*/ebin; do
        out+=(-pa "${d}")
    done
    printf '%s\n' "${out[@]}"
}

print_addr() {
    local hex
    case "$1" in a) hex=aa ;; b) hex=bb ;; esac
    mapfile -t pa < <(erl_pa_args)
    "${ERL_BIN}" -noshell "${pa[@]}" -eval "
        Realm = <<16#11:256>>,
        Ident = <<16#${hex}:256>>,
        io:format(\"~s\", [macula_address:format(macula_address:derive(Realm, Ident))]),
        init:stop()
    "
}

setup() {
    sudo modprobe tun  2>/dev/null || true
    sudo modprobe veth 2>/dev/null || true
    sudo ip netns del "${NETNS_A}" 2>/dev/null || true
    sudo ip netns del "${NETNS_B}" 2>/dev/null || true
    sudo ip link  del "${VETH_A}" 2>/dev/null || true

    sudo ip netns add "${NETNS_A}"
    sudo ip netns add "${NETNS_B}"
    sudo ip link add "${VETH_A}" type veth peer name "${VETH_B}"
    sudo ip link set "${VETH_A}" netns "${NETNS_A}"
    sudo ip link set "${VETH_B}" netns "${NETNS_B}"
    sudo ip -n "${NETNS_A}" link set lo up
    sudo ip -n "${NETNS_B}" link set lo up
    sudo ip -n "${NETNS_A}" addr add "${VETH_A_IP}/${PREFIX}" dev "${VETH_A}"
    sudo ip -n "${NETNS_B}" addr add "${VETH_B_IP}/${PREFIX}" dev "${VETH_B}"
    sudo ip -n "${NETNS_A}" link set "${VETH_A}" up
    sudo ip -n "${NETNS_B}" link set "${VETH_B}" up
}

# Spawn one BEAM node in a netns. Args: role(a|b) netns
start_node() {
    local role="$1" ns="$2"
    local entry pidfile log
    case "${role}" in
        a) entry=start_a; pidfile="${PIDFILE_A}"; log="${LOG_A}" ;;
        b) entry=start_b; pidfile="${PIDFILE_B}"; log="${LOG_B}" ;;
    esac
    mapfile -t pa < <(erl_pa_args)
    : > "${log}"
    sudo ip netns exec "${ns}" \
        "${ERL_BIN}" -noshell "${pa[@]}" -s lan_demo_node "${entry}" \
        > "${log}" 2>&1 &
    echo $! > "${pidfile}"
}

start_all() {
    cd "${SRC_DIR}"
    start_node a "${NETNS_A}"
    start_node b "${NETNS_B}"
    # Wait until each has printed "up; idling"
    for log in "${LOG_A}" "${LOG_B}"; do
        for _ in $(seq 1 30); do
            if grep -q "up; idling" "${log}" 2>/dev/null; then break; fi
            sleep 0.5
        done
        if ! grep -q "up; idling" "${log}" 2>/dev/null; then
            echo "node failed to start; log=${log}:"
            tail -20 "${log}"
            return 1
        fi
    done
    echo ">>> both nodes up"
}

ping_a_to_b() {
    local addr_b
    addr_b="$(print_addr b)"
    echo ">>> ping -6 ${addr_b} from ${NETNS_A}"
    sudo ip netns exec "${NETNS_A}" ping -6 -c 4 -W 2 "${addr_b}"
}

# Phase 1 acceptance bullet 4: a standard userspace tool (curl) reaches
# a TCP server bound to a macula-net address — proving it's a real
# IPv6 substrate, not a closed protocol.
curl_a_to_b() {
    local addr_b
    addr_b="$(print_addr b)"
    echo ">>> http.server bind=[${addr_b}]:${HTTPD_PORT} in ${NETNS_B}"

    # Serve a tiny static dir so we know exactly what should come back.
    local doc_root=/tmp/macula-netns-www
    sudo rm -rf "${doc_root}"
    sudo mkdir -p "${doc_root}"
    echo "macula-net phase1 ok" | sudo tee "${doc_root}/hello.txt" >/dev/null

    : > "${HTTPD_LOG}"
    sudo ip netns exec "${NETNS_B}" \
        python3 -m http.server "${HTTPD_PORT}" \
        --bind "${addr_b}" --directory "${doc_root}" \
        > "${HTTPD_LOG}" 2>&1 &
    echo $! > "${HTTPD_PIDFILE}"

    # Wait for bind: poll ss for the LISTEN socket inside netns B.
    local ready=0
    for _ in $(seq 1 40); do
        if sudo ip netns exec "${NETNS_B}" ss -ltn 2>/dev/null \
                | grep -q ":${HTTPD_PORT} "; then
            ready=1; break
        fi
        sleep 0.25
    done
    if [ "${ready}" -ne 1 ]; then
        echo "httpd failed to bind; log:"
        sudo cat "${HTTPD_LOG}"
        return 1
    fi

    echo ">>> curl -6 from ${NETNS_A}"
    local body
    body="$(sudo ip netns exec "${NETNS_A}" \
        curl --silent --show-error --fail --max-time 5 \
        "http://[${addr_b}]:${HTTPD_PORT}/hello.txt")"
    echo "    body: ${body}"
    if [ "${body}" != "macula-net phase1 ok" ]; then
        echo "    UNEXPECTED BODY"
        return 1
    fi
    echo ">>> curl OK — Phase 1 TCP acceptance met"
}

stop_all() {
    for pf in "${PIDFILE_A}" "${PIDFILE_B}" "${HTTPD_PIDFILE}"; do
        if [ -f "${pf}" ]; then
            local pid; pid="$(cat "${pf}")"
            sudo kill "${pid}" 2>/dev/null || true
            rm -f "${pf}"
        fi
    done
}

teardown() {
    stop_all
    sudo ip netns del "${NETNS_A}" 2>/dev/null || true
    sudo ip netns del "${NETNS_B}" 2>/dev/null || true
    sudo ip link  del "${VETH_A}"  2>/dev/null || true
}

cmd="${1:-run}"; shift || true

case "${cmd}" in
    setup)    setup ;;
    start)    start_all ;;
    ping)     ping_a_to_b ;;
    curl)     curl_a_to_b ;;
    stop)     stop_all ;;
    teardown) teardown ;;
    run)
        trap 'teardown' EXIT
        setup
        start_all
        ping_a_to_b
        curl_a_to_b
        ;;
    *) echo "usage: $0 {run|setup|start|ping|curl|stop|teardown}" >&2; exit 2 ;;
esac
