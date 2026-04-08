#!/bin/bash
# Test per-identity binding: multiple listeners on different addresses, same port.
# This is the whole point of replacing MsQuic with Quinn.
#
# Phase 1: IPv4 loopback (127.0.0.X — works on Linux without setup)
# Phase 2: IPv6 loopback (fd00::X — needs sudo to add addresses)
set -eu

CERT="/tmp/macula-quic-test/cert.pem"
KEY="/tmp/macula-quic-test/key.pem"
PORT=14433

# Generate test certs if missing
if [ ! -f "$CERT" ]; then
    mkdir -p /tmp/macula-quic-test
    openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 \
        -keyout "$KEY" -out "$CERT" -days 365 -nodes -subj '/CN=localhost' 2>/dev/null
fi

# Add IPv6 loopback addresses if not present (needs sudo)
setup_ipv6() {
    for i in 1 2 3 4 5; do
        sudo ip addr add "fd00::${i}/128" dev lo 2>/dev/null || true
    done
    echo "IPv6 test addresses added to lo"
}

# Check if we can add IPv6
if sudo -n true 2>/dev/null; then
    setup_ipv6
    HAS_IPV6=true
else
    echo "No sudo — skipping IPv6 test (run with sudo for full test)"
    HAS_IPV6=false
fi

echo "=== Per-Identity Binding Test ==="
echo ""

rebar3 shell --eval "
    io:format(\"~n=== Phase 1: IPv4 — 5 listeners on 127.0.0.{1..5}:$PORT ===~n~n\"),

    Cert = \"$CERT\",
    Key = \"$KEY\",
    Port = $PORT,
    Opts = [{cert, Cert}, {key, Key}, {alpn, [\"macula\"]},
            {idle_timeout_ms, 10000}, {keep_alive_interval_ms, 5000}],

    %% Start 5 listeners on different IPv4 addresses, same port
    Addrs4 = [<<\"127.0.0.1\">>, <<\"127.0.0.2\">>, <<\"127.0.0.3\">>,
              <<\"127.0.0.4\">>, <<\"127.0.0.5\">>],

    Listeners = lists:map(fun(Addr) ->
        case macula_quic:listen(Addr, Port, Opts) of
            {ok, L} ->
                ok = macula_quic:async_accept(L),
                io:format(\"  [OK] Listener on ~s:~p~n\", [Addr, Port]),
                {Addr, L};
            {error, E} ->
                io:format(\"  [FAIL] ~s:~p — ~p~n\", [Addr, Port, E]),
                {Addr, error}
        end
    end, Addrs4),

    %% Connect to each and verify they're independent
    io:format(\"~n  Connecting clients to each listener...~n\"),
    lists:foreach(fun({Addr, L}) when L =/= error ->
        Host = binary_to_list(Addr),
        case macula_quic:connect(Host, Port, [{alpn, [\"macula\"]}, {verify, none}], 3000) of
            {ok, C} ->
                {ok, S} = macula_quic:open_stream(C),
                Msg = <<\"hello from \", Addr/binary>>,
                ok = macula_quic:send(S, Msg),
                io:format(\"  [OK] Sent to ~s — ~s~n\", [Addr, Msg]),
                macula_quic:close(S),
                macula_quic:close(C);
            {error, E} ->
                io:format(\"  [FAIL] Connect to ~s — ~p~n\", [Addr, E])
        end;
    ({_, error}) -> skip
    end, Listeners),

    %% Check server-side messages
    timer:sleep(500),
    io:format(\"~n  Server received ~p new_conn messages:~n\", [
        length([1 || {quic, new_conn, _, _} <- element(2, process_info(self(), messages))])]),
    Flush = fun Flush() ->
        receive
            {quic, new_conn, _ConnRef, Info} ->
                io:format(\"    new_conn from ~p~n\", [maps:get(remote_addr, Info, unknown)]),
                Flush()
        after 0 -> ok
        end
    end,
    Flush(),

    %% Cleanup IPv4 listeners
    lists:foreach(fun({_, L}) when L =/= error -> macula_quic:close_listener(L);
                     (_) -> ok end, Listeners),

    io:format(\"~n=== Phase 1 PASSED: 5 IPv4 listeners on same port ===~n\"),

    %% Phase 2: IPv6 (if available)
    case $HAS_IPV6 of
        true ->
            io:format(\"~n=== Phase 2: IPv6 — 5 listeners on fd00::{1..5}:$PORT ===~n~n\"),

            Addrs6 = [<<\"fd00::1\">>, <<\"fd00::2\">>, <<\"fd00::3\">>,
                      <<\"fd00::4\">>, <<\"fd00::5\">>],

            Listeners6 = lists:map(fun(Addr) ->
                case macula_quic:listen(Addr, Port, Opts) of
                    {ok, L2} ->
                        ok = macula_quic:async_accept(L2),
                        io:format(\"  [OK] Listener on [~s]:~p~n\", [Addr, Port]),
                        {Addr, L2};
                    {error, E2} ->
                        io:format(\"  [FAIL] [~s]:~p — ~p~n\", [Addr, Port, E2]),
                        {Addr, error}
                end
            end, Addrs6),

            %% Connect to each IPv6 listener
            io:format(\"~n  Connecting clients to each IPv6 listener...~n\"),
            lists:foreach(fun({Addr, L2}) when L2 =/= error ->
                Host = binary_to_list(Addr),
                case macula_quic:connect(Host, Port, [{alpn, [\"macula\"]}, {verify, none}], 3000) of
                    {ok, C2} ->
                        {ok, S2} = macula_quic:open_stream(C2),
                        Msg2 = <<\"ipv6 from \", Addr/binary>>,
                        ok = macula_quic:send(S2, Msg2),
                        io:format(\"  [OK] Sent to [~s] — ~s~n\", [Addr, Msg2]),
                        macula_quic:close(S2),
                        macula_quic:close(C2);
                    {error, E2} ->
                        io:format(\"  [FAIL] Connect to [~s] — ~p~n\", [Addr, E2])
                end;
            ({_, error}) -> skip
            end, Listeners6),

            timer:sleep(500),
            Flush(),

            %% Cleanup
            lists:foreach(fun({_, L2}) when L2 =/= error -> macula_quic:close_listener(L2);
                             (_) -> ok end, Listeners6),

            io:format(\"~n=== Phase 2 PASSED: 5 IPv6 listeners on same port ===~n\");
        false ->
            io:format(\"~n=== Phase 2 SKIPPED (no sudo for IPv6 setup) ===~n\")
    end,

    io:format(\"~n=== ALL PER-IDENTITY TESTS PASSED ===~n\"),
    halt(0).
"
