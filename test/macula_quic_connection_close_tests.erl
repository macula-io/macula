%%%-------------------------------------------------------------------
%%% @doc Real two-endpoint coverage for `macula_quic:close_connection/3' and
%%% `macula_quic:close_reason/1'.
%%%
%%% A station that refuses a connection closes it with an application error
%%% code from include/macula_quic_error_codes.hrl, so the peer can tell a
%%% busy station from any other close. This drives Quinn against itself on
%%% loopback: the code and reason a side closes with must reach the peer's
%%% `close_reason/1', while the closing side reports that it closed, and a
%%% connection that is still open reports `open'.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_connection_close_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("macula/include/macula_quic_error_codes.hrl").

%% The longest reason close_connection/3 sends.
-define(MAX_REASON_BYTES, 256).
-define(EVENT_MS, 5_000).

close_test_() ->
    {timeout, 60,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(Ctx) ->
          [{"close_connection/3 reaches the peer with its code and reason",
            fun() -> code_and_reason_reach_the_peer(Ctx) end},
           {"the side that closed reports locally_closed",
            fun() -> closing_side_reports_locally_closed(Ctx) end},
           {"a connection that is still open reports open",
            fun() -> open_connection_reports_open(Ctx) end},
           {"close_connection/1 still reaches the peer as code 0 with reason closed",
            fun() -> plain_close_is_code_zero(Ctx) end},
           {"a code that does not fit a QUIC variable-length integer is refused, and the connection stays open",
            fun() -> out_of_range_code_is_refused(Ctx) end},
           {"a code of 2^64 is refused the same way, and the connection stays open",
            fun() -> code_of_64_bits_is_refused(Ctx) end},
           {"the largest code a QUIC variable-length integer holds reaches the peer",
            fun() -> largest_code_reaches_the_peer(Ctx) end},
           {"a reason longer than 256 bytes is refused, and the connection stays open",
            fun() -> overlong_reason_is_refused(Ctx) end}]
      end}}.

%%%===================================================================
%%% Fixture
%%%===================================================================

setup() ->
    {Pub, Priv} = ephemeral_keypair(),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(Pub, Priv, [<<"localhost">>, <<"127.0.0.1">>]),
    Dir  = macula_test_tmp:dir("macula-quic-connection-close"),
    Cert = filename:join(Dir, "listener.crt"),
    Key  = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key,  KeyPem),
    #{dir => Dir, cert => Cert, key => Key}.

cleanup(#{dir := Dir}) ->
    ok = file:del_dir_r(Dir),
    drain_quic_messages(),
    ok.

%%%===================================================================
%%% Test bodies
%%%===================================================================

code_and_reason_reach_the_peer(Ctx) ->
    {ClientConn, ServerConn, ClientStream, Cleanup} = connected_pair(Ctx),
    ok = macula_quic:close_connection(ServerConn, ?QUIC_CODE_REFUSED_BUSY, <<"busy">>),
    ok = stream_ended(ClientStream),
    ?assertEqual({application_closed, ?QUIC_CODE_REFUSED_BUSY, <<"busy">>},
                 macula_quic:close_reason(ClientConn)),
    Cleanup().

closing_side_reports_locally_closed(Ctx) ->
    {_ClientConn, ServerConn, ClientStream, Cleanup} = connected_pair(Ctx),
    ok = macula_quic:close_connection(ServerConn, ?QUIC_CODE_REFUSED_BUSY, <<"busy">>),
    ok = stream_ended(ClientStream),
    ?assertEqual(locally_closed, macula_quic:close_reason(ServerConn)),
    Cleanup().

open_connection_reports_open(Ctx) ->
    {ClientConn, ServerConn, _ClientStream, Cleanup} = connected_pair(Ctx),
    ?assertEqual({open, open},
                 {macula_quic:close_reason(ClientConn), macula_quic:close_reason(ServerConn)}),
    Cleanup().

plain_close_is_code_zero(Ctx) ->
    {ClientConn, ServerConn, ClientStream, Cleanup} = connected_pair(Ctx),
    ok = macula_quic:close_connection(ServerConn),
    ok = stream_ended(ClientStream),
    ?assertEqual({application_closed, 0, <<"closed">>}, macula_quic:close_reason(ClientConn)),
    Cleanup().

out_of_range_code_is_refused(Ctx) ->
    {_ClientConn, ServerConn, _ClientStream, Cleanup} = connected_pair(Ctx),
    %% QUIC variable-length integers top out at 2^62 - 1.
    ?assertEqual({{error, error_code_out_of_range}, open},
                 {macula_quic:close_connection(ServerConn, 1 bsl 62, <<"busy">>),
                  macula_quic:close_reason(ServerConn)}),
    Cleanup().

%% A code of 2^64 is a bignum, past any 64-bit integer, and gets the same error.
code_of_64_bits_is_refused(Ctx) ->
    {_ClientConn, ServerConn, _ClientStream, Cleanup} = connected_pair(Ctx),
    ?assertEqual({{error, error_code_out_of_range}, open},
                 {macula_quic:close_connection(ServerConn, 1 bsl 64, <<"busy">>),
                  macula_quic:close_reason(ServerConn)}),
    Cleanup().

largest_code_reaches_the_peer(Ctx) ->
    {ClientConn, ServerConn, ClientStream, Cleanup} = connected_pair(Ctx),
    Largest = (1 bsl 62) - 1,
    ok = macula_quic:close_connection(ServerConn, Largest, <<"busy">>),
    ok = stream_ended(ClientStream),
    ?assertEqual({application_closed, Largest, <<"busy">>}, macula_quic:close_reason(ClientConn)),
    Cleanup().

overlong_reason_is_refused(Ctx) ->
    {_ClientConn, ServerConn, _ClientStream, Cleanup} = connected_pair(Ctx),
    Reason = binary:copy(<<"x">>, ?MAX_REASON_BYTES + 1),
    ?assertEqual({{error, reason_too_long}, open},
                 {macula_quic:close_connection(ServerConn, ?QUIC_CODE_REFUSED_BUSY, Reason),
                  macula_quic:close_reason(ServerConn)}),
    Cleanup().

%%%===================================================================
%%% Helpers
%%%===================================================================

%% A loopback connection pair with one stream the client opened and the
%% server accepted, the client's side active, so the client hears when the
%% connection ends. Returns `{ClientConn, ServerConn, ClientStream, Cleanup}'.
connected_pair(Ctx) ->
    {ok, ClientConn, ServerConn, ConnCleanup} = setup_loopback_pair(Ctx),
    ok = macula_quic:async_accept_stream(ServerConn),
    {ok, ClientStream} = macula_quic:open_stream(ClientConn),
    ok = macula_quic:setopt(ClientStream, active, true),
    ok = macula_quic:send(ClientStream, <<"prime">>),
    ServerStream = receive
        {quic, new_stream, S, _Props} -> S
    after ?EVENT_MS ->
        erlang:error(no_server_stream)
    end,
    Cleanup = fun() ->
        catch macula_quic:close_stream(ClientStream),
        catch macula_quic:close_stream(ServerStream),
        ConnCleanup()
    end,
    {ClientConn, ServerConn, ClientStream, Cleanup}.

%% Waits until the client's stream reports that it ended, which it does when
%% its connection closes.
stream_ended(Stream) ->
    receive
        {quic, stream_closed, Stream, _Detail} -> ok;
        {quic, peer_send_shutdown, Stream, _Detail} -> ok
    after ?EVENT_MS ->
        erlang:error(no_stream_end_after_close)
    end.

setup_loopback_pair(#{cert := Cert, key := Key}) ->
    Port = pick_free_port(),
    {ok, Listener} = macula_quic:listen(<<"127.0.0.1">>, Port,
                                        [{cert, Cert}, {key, Key},
                                         {alpn, [<<"macula-net">>]},
                                         {idle_timeout_ms, 30000},
                                         {keep_alive_interval_ms, 5000}]),
    ok = macula_quic:async_accept(Listener),
    {ok, ClientConn} = macula_quic:connect(<<"127.0.0.1">>, Port,
                                            [{verify, none},
                                             {alpn, [<<"macula-net">>]},
                                             {idle_timeout_ms, 30000},
                                             {keep_alive_interval_ms, 5000}],
                                            ?EVENT_MS),
    ServerConn = receive
        {quic, new_conn, C, _Info} -> C
    after ?EVENT_MS ->
        erlang:error(no_server_conn)
    end,
    Cleanup = fun() ->
        catch macula_quic:close_connection(ClientConn),
        catch macula_quic:close_connection(ServerConn),
        catch macula_quic:close_listener(Listener),
        drain_quic_messages()
    end,
    {ok, ClientConn, ServerConn, Cleanup}.

pick_free_port() ->
    {ok, S} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, P} = inet:port(S),
    gen_udp:close(S),
    P.

drain_quic_messages() ->
    receive
        {quic, _, _, _} -> drain_quic_messages()
    after 0 -> ok
    end.

ephemeral_keypair() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {iolist_to_binary(Pub), iolist_to_binary(Priv)}.
