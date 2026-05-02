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
#   run    setup → start nodes → ping → teardown (default)
#   setup  netns + veth + addresses
#   start  background-spawn both BEAM nodes
#   ping   ping -6 from A to B's macula-net addr
#   stop   kill both BEAMs
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

stop_all() {
    for pf in "${PIDFILE_A}" "${PIDFILE_B}"; do
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
    stop)     stop_all ;;
    teardown) teardown ;;
    run)
        trap 'teardown' EXIT
        setup
        start_all
        ping_a_to_b
        ;;
    *) echo "usage: $0 {run|setup|start|ping|stop|teardown}" >&2; exit 2 ;;
esac
