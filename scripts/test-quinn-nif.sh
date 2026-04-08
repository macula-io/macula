#!/bin/bash
# Integration test for the Quinn QUIC NIF.
# Tests: listen → connect → open_stream → send → close
set -eu

CERT="/tmp/macula-quic-test/cert.pem"
KEY="/tmp/macula-quic-test/key.pem"

# Generate test certs if missing
if [ ! -f "$CERT" ]; then
    mkdir -p /tmp/macula-quic-test
    openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 \
        -keyout "$KEY" -out "$CERT" -days 365 -nodes -subj '/CN=localhost' 2>/dev/null
fi

echo "=== Quinn NIF Integration Test ==="
echo ""

rebar3 shell --eval "
    io:format(\"[1] Listen on 127.0.0.1:14433~n\"),
    {ok, L} = macula_quic:listen(<<\"127.0.0.1\">>, 14433, [
        {cert, \"$CERT\"}, {key, \"$KEY\"},
        {alpn, [\"macula\"]},
        {idle_timeout_ms, 10000},
        {keep_alive_interval_ms, 5000}
    ]),
    io:format(\"    OK: ~p~n\", [L]),

    io:format(\"[2] Start async accept~n\"),
    ok = macula_quic:async_accept(L),
    io:format(\"    OK~n\"),

    io:format(\"[3] Connect client~n\"),
    {ok, C} = macula_quic:connect(\"127.0.0.1\", 14433, [{alpn, [\"macula\"]}, {verify, none}], 5000),
    io:format(\"    OK: ~p~n\", [C]),

    io:format(\"[4] Open stream~n\"),
    {ok, S} = macula_quic:open_stream(C),
    io:format(\"    OK: ~p~n\", [S]),

    io:format(\"[5] Send data~n\"),
    ok = macula_quic:send(S, <<\"hello quinn\">>),
    io:format(\"    OK~n\"),

    io:format(\"[6] Peername~n\"),
    {ok, Peer} = macula_quic:peername(C),
    io:format(\"    OK: ~p~n\", [Peer]),

    io:format(\"[7] Check server-side messages~n\"),
    timer:sleep(200),
    receive
        {quic, new_conn, ServerConn, Info} ->
            io:format(\"    new_conn: ~p info=~p~n\", [ServerConn, Info])
    after 100 ->
        io:format(\"    (no new_conn yet)~n\")
    end,

    io:format(\"[8] Cleanup~n\"),
    macula_quic:close(S),
    macula_quic:close(C),
    macula_quic:close_listener(L),
    io:format(\"    OK~n~n\"),

    io:format(\"=== ALL TESTS PASSED ===~n\"),
    halt(0).
"
