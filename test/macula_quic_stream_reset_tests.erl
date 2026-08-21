%%%-------------------------------------------------------------------
%%% @doc Real two-endpoint coverage for `macula_quic:reset_stream/2'.
%%%
%%% Content-transfer cancel (`macula_content_transfer:cancel/3', see
%%% PLAN_PUSH_UPLOAD.md Phase 1) needs a genuinely peer-visible abort
%%% signal, not just a dropped connection. `macula_stream:abort/3'
%%% (streaming RPC's abort) can't be reused — content streams are raw
%%% QUIC dedicated streams owned by `macula_station_link', not
%%% `macula_stream' gen_servers. This drives Quinn against itself on
%%% loopback (same pattern as `macula_net_phase4_2_mtu_tests') to
%%% prove the QUIC-native alternative actually reaches the peer: a
%%% `reset_stream/2' on one side's send half must surface as a
%%% distinguished `{reset, ErrorCode}' reason on the other side's
%%% `stream_closed' event — never silently collapsed into `none' the
%%% way every other read failure still is, and never confused with a
%%% graceful `close_stream/1' (which the peer sees as
%%% `peer_send_shutdown', not `stream_closed' at all).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_stream_reset_tests).

-include_lib("eunit/include/eunit.hrl").

reset_test_() ->
    {timeout, 30,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(Ctx) ->
          [{"reset_stream/2 delivers {reset, Code} to the peer, not a bare close",
            fun() -> reset_reaches_peer_with_code(Ctx) end},
           {"a graceful close_stream/1 is peer_send_shutdown, never stream_closed/reset",
            fun() -> graceful_close_is_not_a_reset(Ctx) end},
           {"an out-of-range error code is rejected before touching the wire",
            fun() -> out_of_range_code_is_rejected(Ctx) end}]
      end}}.

%%%===================================================================
%%% Fixture
%%%===================================================================

setup() ->
    {Pub, Priv} = ephemeral_keypair(),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            Pub, Priv, [<<"localhost">>, <<"127.0.0.1">>]),
    Tmp  = lists:flatten(io_lib:format("/tmp/macula-quic-reset-~p",
                                       [erlang:unique_integer([positive])])),
    Cert = Tmp ++ ".crt",
    Key  = Tmp ++ ".key",
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key,  KeyPem),
    #{cert => Cert, key => Key}.

cleanup(#{cert := Cert, key := Key}) ->
    file:delete(Cert),
    file:delete(Key),
    drain_quic_messages(),
    ok.

%%%===================================================================
%%% Test bodies
%%%===================================================================

reset_reaches_peer_with_code(Ctx) ->
    {ClientStream, ServerStream, Cleanup} = stream_pair(Ctx),
    Code = 4242,
    ok = macula_quic:reset_stream(ClientStream, Code),
    receive
        {quic, stream_closed, ServerStream, {reset, Code}} -> ok;
        {quic, stream_closed, ServerStream, Other} ->
            erlang:error({wrong_reset_detail, Other})
    after 5_000 ->
        erlang:error(no_stream_closed_after_reset)
    end,
    Cleanup().

graceful_close_is_not_a_reset(Ctx) ->
    {ClientStream, ServerStream, Cleanup} = stream_pair(Ctx),
    ok = macula_quic:close_stream(ClientStream),
    receive
        {quic, peer_send_shutdown, ServerStream, _} -> ok;
        {quic, stream_closed, ServerStream, Detail} ->
            erlang:error({graceful_close_misreported_as_stream_closed, Detail})
    after 5_000 ->
        erlang:error(no_peer_send_shutdown_after_close)
    end,
    Cleanup().

out_of_range_code_is_rejected(Ctx) ->
    {ClientStream, _ServerStream, Cleanup} = stream_pair(Ctx),
    %% QUIC VarInt tops out at 2^62 - 1.
    TooBig = 1 bsl 62,
    ?assertEqual({error, error_code_out_of_range},
                 macula_quic:reset_stream(ClientStream, TooBig)),
    Cleanup().

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Loopback listener + client connection + one bidi stream opened by
%% the client and accepted server-side, both sides set active. Returns
%% `{ClientStream, ServerStream, Cleanup}'.
stream_pair(Ctx) ->
    {ok, ClientConn, ServerConn, ConnCleanup} = setup_loopback_pair(Ctx),
    ok = macula_quic:async_accept_stream(ServerConn),
    {ok, ClientStream} = macula_quic:open_stream(ClientConn),
    %% Quinn's open_bi/1 only allocates LOCAL stream state — the peer's
    %% accept_bi/1 doesn't fire until a STREAM frame with actual bytes
    %% crosses the wire. Prime it before waiting for `new_stream'.
    ok = macula_quic:setopt(ClientStream, active, true),
    ok = macula_quic:send(ClientStream, <<"prime">>),
    ServerStream = receive
        {quic, new_stream, S, _Props} -> S
    after 5_000 ->
        erlang:error(no_server_stream)
    end,
    ok = macula_quic:setopt(ServerStream, active, true),
    receive
        {quic, <<"prime">>, ServerStream, _} -> ok
    after 5_000 ->
        erlang:error(no_prime_data)
    end,
    Cleanup = fun() ->
        catch macula_quic:close_stream(ClientStream),
        catch macula_quic:close_stream(ServerStream),
        ConnCleanup()
    end,
    {ClientStream, ServerStream, Cleanup}.

setup_loopback_pair(#{cert := Cert, key := Key}) ->
    Self = self(),
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
                                            5000),
    ServerConn = receive
        {quic, new_conn, C, _Info} -> C
    after 5000 ->
        error(no_server_conn)
    end,
    Cleanup = fun() ->
        try macula_quic:close_connection(ClientConn) catch _:_ -> ok end,
        try macula_quic:close_connection(ServerConn) catch _:_ -> ok end,
        try macula_quic:close_listener(Listener) catch _:_ -> ok end,
        Self ! cleaned,
        receive cleaned -> ok after 0 -> ok end
    end,
    {ok, ClientConn, ServerConn, Cleanup}.

pick_free_port() ->
    {ok, S} = gen_udp:open(0, [binary, {ip, {127,0,0,1}}]),
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
