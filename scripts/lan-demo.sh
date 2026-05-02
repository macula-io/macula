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

print_addr() {
    local role="$1"
    local hex
    case "${role}" in a) hex=aa ;; b) hex=bb ;; esac
    erl -noshell $(erl_pa_args) -eval "
        Realm = <<16#11:256>>,
        Ident = <<16#${hex}:256>>,
        io:format(\"~s\", [macula_address:format(macula_address:derive(Realm, Ident))]),
        init:stop()
    "
}

ensure_local_build() {
    if [ ! -d "${ROOT_DIR}/_build/default/lib/macula/ebin" ]; then
        ( cd "${ROOT_DIR}" && rebar3 compile )
    fi
    erlc -pa "${ROOT_DIR}/_build/default/lib/macula/ebin" \
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
            "${ROOT_DIR}/" "${BEAM02_HOST}:macula-src/"
        echo ">>> synced -> ${BEAM02_HOST}:~/macula-src/"
        ;;

    build-beam02)
        ssh_beam02 'set -eu
            export PATH=$HOME/.asdf/shims:$PATH
            cd ~/macula-src
            rebar3 as prod release
            erlc -pa _build/default/lib/macula/ebin \
                 -o _build/default/lib/macula/ebin \
                 scripts/lan_demo_node.erl
            ls -la _build/prod/rel/macula/bin/macula
        '
        ;;

    run-local)
        ensure_local_build
        addr_a="$(print_addr a)"
        addr_b="$(print_addr b)"
        echo ">>> local self=${addr_a}  peer=${addr_b} via ${BEAM02_LAN_IP}:${QUIC_PORT}"
        # sudo strips PATH; pass MACULA_PEER_HOST through and use erl by full path.
        ERL_BIN=$(command -v erl)
        exec sudo MACULA_PEER_HOST="${BEAM02_LAN_IP}" \
            "${ERL_BIN}" -noshell $(erl_pa_args) -s lan_demo_node start_a
        ;;

    run-beam02)
        addr_a="$(print_addr a)"
        addr_b="$(print_addr b)"
        echo ">>> beam02 self=${addr_b}  peer=${addr_a} via ${LOCAL_LAN_IP}:${QUIC_PORT}"
        ssh -t "${BEAM02_HOST}" "set -eu
            cd ~/macula-src
            ERL=\$(ls _build/prod/rel/macula/erts-*/bin/erl)
            sudo MACULA_PEER_HOST=${LOCAL_LAN_IP} \\
                \"\${ERL}\" -noshell \\
                -pa _build/default/lib/macula/ebin \\
                \$(for d in _build/prod/rel/macula/lib/*/ebin; do printf ' -pa %s' \$d; done) \\
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

    *)
        sed -n '/^# /,/^$/p' "$0" | sed 's/^# \{0,1\}//'
        ;;
esac
