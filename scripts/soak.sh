#!/usr/bin/env bash
#
# Phase 4.7 soak harness for macula-net.
#
# Usage:
#   soak.sh start --duration-min N [--loop-ms M] [--payload-bytes B]
#   soak.sh stop
#   soak.sh report
#   soak.sh status
#
# Boots the netns-alice-bob demo on beam02 in soak mode (continuous
# Alice ↔ Bob traffic via MACULA_DAEMON_LOOP_MS), scrapes both host
# stations' /metrics endpoints every 30 s into a CSV, and grades the
# result on stop.
#
# Designed for both 5-minute sanity runs and 24-hour bake-off runs.
# The harness itself only times out after the requested duration;
# operators can also stop early with `soak.sh stop`.

set -euo pipefail

ROOT_DIR=${ROOT_DIR:-$(cd "$(dirname "$0")/.." && pwd)}
BEAM02_HOST=${BEAM02_HOST:-rl@beam02.lab}
SCRAPE_INTERVAL_S=${SCRAPE_INTERVAL_S:-30}
SOAK_LOG_DIR=${SOAK_LOG_DIR:-/tmp/macula-soak}
HELSINKI_URL=${HELSINKI_URL:-http://10.99.0.2:9145/metrics}
NUREMBERG_URL=${NUREMBERG_URL:-http://10.99.0.3:9146/metrics}
DEFAULT_LOOP_MS=${DEFAULT_LOOP_MS:-5000}
DEFAULT_PAYLOAD_BYTES=${DEFAULT_PAYLOAD_BYTES:-4096}
LOCAL_SCRAPE_PIDFILE=${LOCAL_SCRAPE_PIDFILE:-/tmp/macula-soak/scrape.pid}
LOCAL_TIMER_PIDFILE=${LOCAL_TIMER_PIDFILE:-/tmp/macula-soak/timer.pid}

mkdir -p "${SOAK_LOG_DIR}"

ssh_beam02() {
    ssh -q -o LogLevel=ERROR "${BEAM02_HOST}" "$@"
}

cmd_start() {
    local duration_min=0
    local loop_ms="${DEFAULT_LOOP_MS}"
    local payload="${DEFAULT_PAYLOAD_BYTES}"
    while [ $# -gt 0 ]; do
        case "$1" in
            --duration-min)  duration_min="$2"; shift 2 ;;
            --loop-ms)       loop_ms="$2"; shift 2 ;;
            --payload-bytes) payload="$2"; shift 2 ;;
            *) echo "unknown option: $1" >&2; exit 2 ;;
        esac
    done
    [ "${duration_min}" -gt 0 ] || { echo "FAIL: --duration-min required" >&2; exit 2; }

    if [ -f "${LOCAL_SCRAPE_PIDFILE}" ] && kill -0 "$(cat "${LOCAL_SCRAPE_PIDFILE}")" 2>/dev/null; then
        echo "FAIL: soak scraper already running (pid=$(cat "${LOCAL_SCRAPE_PIDFILE}"))" >&2
        exit 2
    fi
    rm -f "${LOCAL_SCRAPE_PIDFILE}" "${LOCAL_TIMER_PIDFILE}"
    rm -rf "${SOAK_LOG_DIR}"
    mkdir -p "${SOAK_LOG_DIR}"

    local csv="${SOAK_LOG_DIR}/metrics.csv"
    local meta="${SOAK_LOG_DIR}/meta.txt"
    local soaklog="${SOAK_LOG_DIR}/soak.log"

    echo "duration_min=${duration_min}"     >  "${meta}"
    echo "loop_ms=${loop_ms}"                >> "${meta}"
    echo "payload_bytes=${payload}"          >> "${meta}"
    echo "scrape_interval_s=${SCRAPE_INTERVAL_S}" >> "${meta}"
    echo "started_at=$(date -Iseconds)"      >> "${meta}"

    echo "ts,host,envelopes_forwarded,envelopes_dropped,resolve_total,attach_active,route_cache_entries,transport_connections,path_mtu" \
        > "${csv}"

    echo ">>> staging soak inputs to beam02"
    rsync -a -e 'ssh -q -o LogLevel=ERROR' \
        "${ROOT_DIR}/scripts/netns-alice-bob-demo.sh" \
        "${ROOT_DIR}/scripts/lan_demo_node3.erl" \
        "${ROOT_DIR}/scripts/lan_demo_dht.erl" \
        "${ROOT_DIR}/scripts/lan_demo_host_node.erl" \
        "${ROOT_DIR}/scripts/lan_demo_daemon_node.erl" \
        "${BEAM02_HOST}:macula-src/scripts/"
    ssh_beam02 'chmod +x ~/macula-src/scripts/netns-alice-bob-demo.sh'

    echo ">>> starting soak on beam02 (loop_ms=${loop_ms} payload=${payload} duration=${duration_min}min)"
    # Remote log path on beam02; the workstation-side log captures
    # only the ssh session.
    local remote_log=/tmp/macula-soak.beam02.log
    # `disown` is load-bearing: without it the remote sshd waits on the
    # backgrounded child's session-channel FDs (inherited from the
    # remote shell) before closing the SSH connection. Symptom is the
    # local ssh client hangs forever, blocking cmd_start from reaching
    # the scraper-launch lines below — workload runs, no scrape data.
    # `setsid` alone is insufficient on bash-over-ssh.
    ssh_beam02 "mkdir -p /tmp && rm -f /tmp/macula-alicebob/soak.pid &&
                MACULA_DAEMON_LOOP_MS=${loop_ms} \
                MACULA_DAEMON_PAYLOAD_BYTES=${payload} \
                setsid nohup bash ~/macula-src/scripts/netns-alice-bob-demo.sh soak \
                > ${remote_log} 2>&1 < /dev/null &
                disown
                echo started" || true

    # Wait for the soak.pid to appear so we know setup completed.
    local waited=0
    until ssh_beam02 'test -f /tmp/macula-alicebob/soak.pid'; do
        waited=$((waited + 5))
        if [ "${waited}" -gt 60 ]; then
            echo "FAIL: soak didn't come up within 60 s" >&2
            echo "--- remote log tail ---"
            ssh_beam02 "tail -30 ${remote_log}" 2>/dev/null || true
            exit 1
        fi
        sleep 5
    done
    echo ">>> soak running on beam02; scraper begins"

    # Local scraper loop. Plain subshell — no `local` (subshell isn't
    # a function); duration_min reaches the subshell via export.
    local end_ts=$(( $(date +%s) + duration_min * 60 ))
    (
        while [ "$(date +%s)" -lt "${end_ts}" ]; do
            scrape_one helsinki  "${HELSINKI_URL}"  "${csv}"
            scrape_one nuremberg "${NUREMBERG_URL}" "${csv}"
            sleep "${SCRAPE_INTERVAL_S}"
        done
        echo "duration_complete" >> "${meta}"
    ) > "${soaklog}" 2>&1 &
    local scrape_pid=$!
    echo "${scrape_pid}" > "${LOCAL_SCRAPE_PIDFILE}"

    # Timer that stops the soak when duration elapses
    (
        sleep $((duration_min * 60))
        cmd_stop --internal
    ) > /dev/null 2>&1 &
    echo $! > "${LOCAL_TIMER_PIDFILE}"

    echo ">>> soak in progress; scraper pid=${scrape_pid}; will auto-stop after ${duration_min} min"
    echo ">>> follow with: tail -f ${csv}"

    # Sanity check: confirm scraper actually launched and is writing.
    # Wait one scrape interval then verify the pidfile and at least one
    # data row exist. Catches the silent-scraper-death class of bugs.
    sleep "$((SCRAPE_INTERVAL_S + 5))"
    if ! [ -f "${LOCAL_SCRAPE_PIDFILE}" ] || ! kill -0 "$(cat "${LOCAL_SCRAPE_PIDFILE}")" 2>/dev/null; then
        echo "FAIL: scraper died within first scrape interval; see ${soaklog}" >&2
        exit 1
    fi
    local rows; rows=$(($(wc -l < "${csv}") - 1))
    if [ "${rows}" -lt 1 ]; then
        echo "FAIL: scraper alive but no rows after ${SCRAPE_INTERVAL_S}s; see ${soaklog}" >&2
        exit 1
    fi
    echo ">>> sanity check passed: ${rows} row(s) scraped after first interval"
}

scrape_one() {
    local host="$1" url="$2" csv="$3"
    local body
    # Metrics endpoints bind inside beam02 netns IPs (10.99.0.0/24);
    # only reachable from beam02 root ns. Curl via ssh.
    body="$(ssh_beam02 "curl -s --max-time 5 ${url}" || true)"
    if [ -z "${body}" ]; then
        # Use - to record absence
        printf '%s,%s,-,-,-,-,-,-,-\n' "$(date -Iseconds)" "${host}" >> "${csv}"
        return 0
    fi
    local fwd dropped resolve attach cache conns mtu
    fwd="$(printf '%s\n' "${body}" \
        | awk '/^macula_net_envelopes_forwarded_total/ {sum+=$NF} END {print sum+0}')"
    dropped="$(printf '%s\n' "${body}" \
        | awk '/^macula_net_envelopes_dropped_total/ {sum+=$NF} END {print sum+0}')"
    resolve="$(printf '%s\n' "${body}" \
        | awk '/^macula_net_resolve_total/ {sum+=$NF} END {print sum+0}')"
    attach="$(printf '%s\n' "${body}" \
        | awk '/^macula_net_attach_active / {print $NF}' | head -1)"
    cache="$(printf '%s\n' "${body}" \
        | awk '/^macula_net_route_cache_entries / {print $NF}' | head -1)"
    conns="$(printf '%s\n' "${body}" \
        | awk '/^macula_net_transport_connections / {print $NF}' | head -1)"
    mtu="$(printf '%s\n' "${body}" \
        | awk '/^macula_net_transport_path_mtu_bytes\{/ {print $NF}' | head -1)"
    printf '%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
        "$(date -Iseconds)" "${host}" \
        "${fwd:-0}" "${dropped:-0}" "${resolve:-0}" \
        "${attach:--}" "${cache:--}" "${conns:--}" "${mtu:--}" >> "${csv}"
}

cmd_stop() {
    local internal=0
    [ "${1:-}" = "--internal" ] && internal=1

    # Stop local scraper + timer
    for pf in "${LOCAL_SCRAPE_PIDFILE}" "${LOCAL_TIMER_PIDFILE}"; do
        if [ -f "${pf}" ]; then
            local pid; pid="$(cat "${pf}")"
            kill "${pid}" 2>/dev/null || true
            rm -f "${pf}"
        fi
    done
    # Belt-and-braces: pkill any orchestrator process. Recovers from
    # the broken state where the orchestrator hung in cmd_start before
    # writing the pidfiles (see cmd_status BROKEN diagnostic).
    pkill -u "$(id -u)" -f 'soak\.sh start' 2>/dev/null || true

    # Stop remote soak
    ssh_beam02 'if [ -f /tmp/macula-alicebob/soak.pid ]; then
                    sudo kill "$(cat /tmp/macula-alicebob/soak.pid)" 2>/dev/null || true
                    rm -f /tmp/macula-alicebob/soak.pid
                fi' || true

    if [ "${internal}" -eq 0 ]; then
        echo ">>> soak stopped"
    fi
    cmd_report
}

cmd_status() {
    if [ -f "${LOCAL_SCRAPE_PIDFILE}" ] && kill -0 "$(cat "${LOCAL_SCRAPE_PIDFILE}")" 2>/dev/null; then
        echo "soak: RUNNING (scraper pid=$(cat "${LOCAL_SCRAPE_PIDFILE}"))"
        if [ -f "${SOAK_LOG_DIR}/metrics.csv" ]; then
            local rows; rows=$(($(wc -l < "${SOAK_LOG_DIR}/metrics.csv") - 1))
            echo "       rows scraped: ${rows}"
        fi
        return 0
    fi
    # Detect "broken" state: orchestrator process alive but no scraper
    # pidfile. This is the symptom of an SSH that hung during cmd_start
    # (workload runs on beam02, no scrape data captured). Reports
    # honestly instead of misleading "NOT RUNNING".
    local orchestrators
    orchestrators=$(pgrep -f 'soak\.sh start' 2>/dev/null | grep -v $$ || true)
    if [ -n "${orchestrators}" ]; then
        echo "soak: BROKEN (orchestrator alive but scraper pidfile missing)"
        echo "      orchestrator pids: ${orchestrators}"
        echo "      most likely the cmd_start SSH hung at workload-launch"
        echo "      recover with: $0 stop"
        return 1
    fi
    echo "soak: NOT RUNNING"
}

cmd_report() {
    local csv="${SOAK_LOG_DIR}/metrics.csv"
    [ -f "${csv}" ] || { echo "no metrics.csv; nothing to report" >&2; exit 1; }
    local rows; rows=$(($(wc -l < "${csv}") - 1))
    [ "${rows}" -ge 2 ] || { echo "FAIL: only ${rows} scrape(s) — need at least 2" >&2; exit 1; }

    echo
    echo "=== macula-net soak report ==="
    if [ -f "${SOAK_LOG_DIR}/meta.txt" ]; then
        sed 's/^/  /' "${SOAK_LOG_DIR}/meta.txt"
    fi
    echo "  scrape_rows=${rows}"

    # Warmup-aware delta: skip the first WARMUP_SCRAPES scrapes per host
    # to exclude boot-time DHT-resolve failures and other startup
    # transients from the steady-state metric. Default 4 (= 2 minutes
    # at 30s scrape interval).
    local warmup="${WARMUP_SCRAPES:-4}"
    awk -F, -v warmup="${warmup}" '
        NR == 1 { next }
        {
            host = $2
            fwd  = ($3 == "-") ? 0 : $3
            drop = ($4 == "-") ? 0 : $4
            seen[host]++
            if (seen[host] == warmup) {
                base_fwd[host]  = fwd
                base_drop[host] = drop
                base_ts[host]   = $1
            }
            last_fwd[host]  = fwd
            last_drop[host] = drop
            last_ts[host]   = $1
        }
        END {
            fail = 0
            printf "  warmup_scrapes=%d (per host) — deltas measured from end of warmup\n",
                   warmup
            for (h in last_fwd) {
                if (!(h in base_fwd)) {
                    printf "  %-10s (run too short for warmup-aware delta; need ≥ %d scrapes)\n",
                           h, warmup
                    continue
                }
                df = last_fwd[h] - base_fwd[h]
                dd = last_drop[h] - base_drop[h]
                printf "  %-10s forwarded_delta=%d  dropped_delta=%d", h, df, dd
                drop_ratio = (df > 0) ? (dd * 100.0 / df) : 0
                if (df > 0 && drop_ratio > 1.0) {
                    printf "  ⚠ drop_ratio=%.2f%% (>1%% suspicious)\n", drop_ratio
                    fail = 1
                } else if (df == 0 && dd == 0) {
                    printf "  (no traffic)\n"
                } else {
                    printf "  ✓ drop_ratio=%.2f%%\n", drop_ratio
                }
            }
            if (fail == 1) {
                print "==> FAIL"
                exit 1
            } else {
                print "==> PASS: no excess drops in steady state"
                exit 0
            }
        }
    ' "${csv}"
}

cmd="${1:-}"; shift || true
case "${cmd}" in
    start)  cmd_start "$@" ;;
    stop)   cmd_stop ;;
    status) cmd_status ;;
    report) cmd_report ;;
    *)
        echo "usage: $0 {start --duration-min N|stop|status|report}" >&2
        exit 2
        ;;
esac
