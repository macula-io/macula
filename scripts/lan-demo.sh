#!/usr/bin/env bash
#
# Real-LAN demo for macula-net (PLAN_MACULA_NET §12 Phase 1).
# Two real boxes over the lab LAN.
#
#   workstation 192.168.1.100  <-- QUIC over LAN -->  beam02 192.168.1.12
#       macula0 fd<realm>::a                            macula0 fd<realm>::b
#
# Subcommands:
#   addrs        print derived macula-net addrs
#   prep-beam02  install rebar3 + rust on beam02 (one-time, asdf)
#   sync-beam02  rsync source to beam02:~/macula-src/
#   build-beam02 rebar3 as prod release on beam02
#   run-local    start macula-net on workstation (sudo, foreground)
#   run-beam02   start macula-net on beam02 (sudo, foreground over ssh)
#   ping         ping -6 from workstation to beam02's macula-net addr
#   teardown     remove TUN devices on both sides
#   auto         Phase 1: two netns + ping6 across (PLAN_MACULA_NET §12)
#   auto3        Phase 2: three netns + curl over DHT-resolved mesh
#   auto-daemons   Phase 3.7: hosted-identity gateway, sender → helsinki → alice
#   auto-alice-bob Phase 3 §8 #4: full Alice ↔ Bob via Helsinki ↔ Nuremberg

set -euo pipefail

BEAM02_HOST=${BEAM02_HOST:-rl@beam02.lab}
LOCAL_LAN_IP=${LOCAL_LAN_IP:-192.168.1.100}
BEAM02_LAN_IP=${BEAM02_LAN_IP:-192.168.1.12}
QUIC_PORT=4400
TUN=macula0
REMOTE_SRC='${HOME}/macula-src'

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"

erl_pa_args() {
    for ebin in "${ROOT_DIR}"/_build/default/lib/*/ebin; do
        printf -- '-pa %s ' "${ebin}"
    done
}

# Resolve a binary that might live in the invoking user's asdf shims,
# even when this script runs under sudo (root PATH lacks shims).
resolve_bin() {
    local name="$1"
    local invoking_user="${SUDO_USER:-${USER}}"
    local user_home
    user_home="$(getent passwd "${invoking_user}" | cut -d: -f6)"
    local shim="${user_home}/.asdf/shims/${name}"
    if [ -x "${shim}" ]; then
        echo "${shim}"
        return 0
    fi
    command -v "${name}" || { echo "ERROR: ${name} not found" >&2; return 1; }
}

print_addr() {
    local role="$1"
    local hex
    case "${role}" in a) hex=aa ;; b) hex=bb ;; esac
    local erl_bin
    erl_bin="$(resolve_bin erl)"
    "${erl_bin}" -noshell $(erl_pa_args) -eval "
        Realm = <<16#11:256>>,
        Ident = <<16#${hex}:256>>,
        io:format(\"~s\", [macula_address:format(macula_address:derive(Realm, Ident))]),
        init:stop()
    "
}

ensure_local_build() {
    local rebar3_bin erlc_bin
    rebar3_bin="$(resolve_bin rebar3)"
    erlc_bin="$(resolve_bin erlc)"
    if [ ! -d "${ROOT_DIR}/_build/default/lib/macula/ebin" ]; then
        ( cd "${ROOT_DIR}" && "${rebar3_bin}" compile )
    fi
    "${erlc_bin}" -pa "${ROOT_DIR}/_build/default/lib/macula/ebin" \
         -o "${ROOT_DIR}/_build/default/lib/macula/ebin" \
         "${ROOT_DIR}/scripts/lan_demo_node.erl"
}

ssh_beam02() { ssh -o ConnectTimeout=10 "${BEAM02_HOST}" "$@"; }

cmd="${1:-help}"; shift || true

case "${cmd}" in

    addrs)
        ensure_local_build
        printf 'A: %s\n' "$(print_addr a)"
        printf 'B: %s\n' "$(print_addr b)"
        ;;

    prep-beam02)
        # Rust 1.90 is the right pin on beam02 as of 2026-05-02. The
        # transitive `time` crate started requiring rustc 1.88.0+ so
        # 1.85 no longer compiles; the late-handover note about 1.88
        # being the safe ceiling turned out to be stale because 1.88's
        # pre-built libstd embeds GLIBC_2.34 pthread_* symbols that
        # Ubuntu 20.04's glibc 2.31 doesn't provide. 1.90 happens to
        # link only up to GLIBC_2.29 — verified end-to-end by the
        # auto-daemons demo (Phase 3.7).
        ssh_beam02 'set -eu
            export PATH=$HOME/.asdf/shims:$PATH
            ~/.asdf/bin/asdf plugin add rebar  2>/dev/null || true
            ~/.asdf/bin/asdf plugin add rust   2>/dev/null || true
            ~/.asdf/bin/asdf install rebar 3.25.0
            ~/.asdf/bin/asdf install rust 1.90.0
            ~/.asdf/bin/asdf global rebar 3.25.0
            ~/.asdf/bin/asdf global rust  1.90.0
            grep -q "^erlang " ~/.tool-versions || echo "erlang 26.0" >> ~/.tool-versions
            grep -q "^rebar "  ~/.tool-versions || echo "rebar 3.25.0"  >> ~/.tool-versions
            grep -q "^rust "   ~/.tool-versions || echo "rust 1.90.0"   >> ~/.tool-versions
            echo "--- versions ---"
            ~/.asdf/shims/erl -version
            ~/.asdf/shims/rebar3 version
            ~/.asdf/shims/rustc --version
        '
        ;;

    sync-beam02)
        rsync -a --delete \
            --exclude '_build/' \
            --exclude '_checkouts/' \
            --exclude '.git/' \
            --exclude 'doc/' \
            --exclude '*.beam' \
            --exclude 'native/*/target/' \
            --exclude '.tool-versions' \
            "${ROOT_DIR}/" "${BEAM02_HOST}:macula-src/"
        echo ">>> synced -> ${BEAM02_HOST}:~/macula-src/"
        ;;

    build-beam02)
        # Skipping `as prod release` — rebar.config points at config/{sys.config,vm.args}
        # which aren't checked in. Plain compile + raw erl is enough for the demo.
        # rebar3's NIF hook will skip rebuilding the .so if it sees a
        # cached one even when the underlying Rust toolchain has been
        # bumped, which lands us with a stale glibc-incompatible NIF.
        # Force rebuild by clearing target/ and the priv .so files.
        ssh_beam02 'set -eu
            export PATH=$HOME/.asdf/shims:$PATH
            cd ~/macula-src
            for nif in native/*/; do rm -rf "${nif}target"; done
            rm -f _build/default/lib/macula/priv/*.so
            rebar3 compile
            erlc -pa _build/default/lib/macula/ebin \
                 -o _build/default/lib/macula/ebin \
                 scripts/lan_demo_node.erl
            ls _build/default/lib/macula/priv/*.so | head
        '
        ;;

    run-local)
        ensure_local_build
        addr_a="$(print_addr a)"
        addr_b="$(print_addr b)"
        echo ">>> local self=${addr_a}  peer=${addr_b} via ${BEAM02_LAN_IP}:${QUIC_PORT}"
        ERL_BIN="$(resolve_bin erl)"
        # If we're already root (script invoked via `sudo ...`) skip the
        # nested sudo; otherwise self-elevate.
        if [ "$(id -u)" -eq 0 ]; then
            exec env MACULA_PEER_HOST="${BEAM02_LAN_IP}" \
                "${ERL_BIN}" -noshell $(erl_pa_args) -s lan_demo_node start_a
        else
            exec sudo MACULA_PEER_HOST="${BEAM02_LAN_IP}" \
                "${ERL_BIN}" -noshell $(erl_pa_args) -s lan_demo_node start_a
        fi
        ;;

    run-beam02)
        addr_a="$(print_addr a)"
        addr_b="$(print_addr b)"
        echo ">>> beam02 self=${addr_b}  peer=${addr_a} via ${LOCAL_LAN_IP}:${QUIC_PORT}"
        ssh "${BEAM02_HOST}" "set -eu
            cd ~/macula-src
            sudo MACULA_PEER_HOST=${LOCAL_LAN_IP} \\
                \$HOME/.asdf/installs/erlang/26.0/bin/erl -noshell \\
                \$(for d in _build/default/lib/*/ebin; do printf ' -pa %s' \$d; done) \\
                -s lan_demo_node start_b
        "
        ;;

    ping)
        ensure_local_build
        addr_b="$(print_addr b)"
        echo ">>> ping -6 ${addr_b}"
        exec ping -6 -c 4 "${addr_b}"
        ;;

    teardown)
        sudo ip link del "${TUN}" 2>/dev/null || true
        ssh_beam02 "sudo ip link del ${TUN} 2>/dev/null || true"
        echo ">>> torn down"
        ;;

    auto)
        # Push the netns demo script to beam02 and run it. Two BEAM
        # nodes, two netns, ping6 across — fully automated, no
        # human-in-the-loop. Workstation just orchestrates over ssh.
        scp -q "${ROOT_DIR}/scripts/netns-demo.sh" "${BEAM02_HOST}:macula-src/scripts/netns-demo.sh"
        ssh_beam02 'chmod +x ~/macula-src/scripts/netns-demo.sh'
        ssh_beam02 '~/macula-src/scripts/netns-demo.sh run'
        ;;

    auto3)
        # Phase 2 auto demo: 3 netns + bridge + mock DHT + curl
        # across the macula-net mesh. Same pattern as `auto` but
        # exercises the DHT-resolution path end-to-end.
        scp -q "${ROOT_DIR}/scripts/netns3-demo.sh" \
              "${ROOT_DIR}/scripts/lan_demo_node3.erl" \
              "${ROOT_DIR}/scripts/lan_demo_dht.erl" \
              "${BEAM02_HOST}:macula-src/scripts/"
        ssh_beam02 'chmod +x ~/macula-src/scripts/netns3-demo.sh'
        ssh_beam02 '~/macula-src/scripts/netns3-demo.sh run'
        ;;

    auto-daemons)
        # Phase 3.7 auto demo: 3 netns + bridge + mock DHT + the
        # hosted-identity gateway. Sender curls Alice's address via
        # the kernel TUN; the IPv6 packet traverses Helsinki via the
        # host_attach_controller and lands in Alice's daemon process.
        # Acceptance is a "[daemon] received" line in alice.log.
        scp -q "${ROOT_DIR}/scripts/netns-daemons-demo.sh" \
              "${ROOT_DIR}/scripts/lan_demo_node3.erl" \
              "${ROOT_DIR}/scripts/lan_demo_dht.erl" \
              "${ROOT_DIR}/scripts/lan_demo_host_node.erl" \
              "${ROOT_DIR}/scripts/lan_demo_daemon_node.erl" \
              "${BEAM02_HOST}:macula-src/scripts/"
        ssh_beam02 'chmod +x ~/macula-src/scripts/netns-daemons-demo.sh'
        ssh_beam02 '~/macula-src/scripts/netns-daemons-demo.sh run'
        ;;

    auto-alice-bob)
        # Phase 3 §8 #4 acceptance: 4 netns + bridge + mock DHT.
        # Two host stations (Helsinki, Nuremberg) and two daemons
        # (Alice attached to Helsinki, Bob attached to Nuremberg).
        # Both daemons send a CBOR data envelope to the other; the
        # bytes traverse alice -> helsinki -> nuremberg -> bob (and
        # the symmetric path) via the controller's forward_fn ->
        # route_packet:dispatch_envelope chain. Acceptance is a
        # "[daemon] received" line in BOTH alice.log and bob.log.
        scp -q "${ROOT_DIR}/scripts/netns-alice-bob-demo.sh" \
              "${ROOT_DIR}/scripts/lan_demo_node3.erl" \
              "${ROOT_DIR}/scripts/lan_demo_dht.erl" \
              "${ROOT_DIR}/scripts/lan_demo_host_node.erl" \
              "${ROOT_DIR}/scripts/lan_demo_daemon_node.erl" \
              "${BEAM02_HOST}:macula-src/scripts/"
        ssh_beam02 'chmod +x ~/macula-src/scripts/netns-alice-bob-demo.sh'
        ssh_beam02 '~/macula-src/scripts/netns-alice-bob-demo.sh run'
        ;;

    *)
        sed -n '/^# /,/^$/p' "$0" | sed 's/^# \{0,1\}//'
        ;;
esac
