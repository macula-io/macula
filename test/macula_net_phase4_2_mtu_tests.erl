%%%-------------------------------------------------------------------
%%% @doc Phase 4.2 §5 acceptance — MTU + path-MTU observability.
%%%
%%% Covers rows #1, #2, #3, #5 of the §5 grid. Rows #4 and #6 are
%%% live-demo rows that run on beam02 (see lan-demo.sh auto-mtu-large
%%% and netns demo confirmation).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_phase4_2_mtu_tests).

-include_lib("eunit/include/eunit.hrl").

-define(OWN,    <<16#fd, 1:40, 16#01:80>>).
-define(REMOTE, <<16#fd, 1:40, 16#aa:80>>).

phase4_2_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        fun nif_path_mtu_returns_bytes_for_loopback/1,
        fun transport_peer_path_mtu_returns_not_connected_for_unknown/1,
        fun large_packet_round_trip/1,
        fun path_mtu_metric_registered/1
     ]}.

setup() ->
    {ok, _} = application:ensure_all_started(telemetry),
    {ok, _} = macula_metrics:start_link(#{install_default_gauges => false}),
    macula_metrics:reset_all(),
    ok.

teardown(_) ->
    case whereis(macula_metrics) of
        undefined -> ok;
        _ -> macula_metrics:stop()
    end,
    ok.

%% --- §5 #1 + §5 #2 — NIF + transport API ----------------------------

nif_path_mtu_returns_bytes_for_loopback(_) ->
    fun() ->
        %% Smoke-test the NIF: spin up a Quinn loopback listener +
        %% client connection, query max_datagram_size, expect a
        %% positive number (Quinn's initial MTU default is ≥ 1200).
        {ok, ServerConn, ClientConn, Cleanup} = setup_loopback_pair(),
        {ok, ClientMtu} = macula_quic:max_datagram_size(ClientConn),
        ?assert(ClientMtu >= 1200),
        {ok, ServerMtu} = macula_quic:max_datagram_size(ServerConn),
        ?assert(ServerMtu >= 1200),
        Cleanup()
    end.

transport_peer_path_mtu_returns_not_connected_for_unknown(_) ->
    fun() ->
        {ok, _Pid} = macula_net_transport_quic:start_link(#{port => 0}),
        Reply = macula_net_transport_quic:peer_path_mtu(<<"never-connected">>),
        ?assertEqual({error, not_connected}, Reply),
        macula_net_transport_quic:stop()
    end.

%% --- §5 #3 — boundary: 60 KiB inner packet --------------------------

large_packet_round_trip(_) ->
    fun() ->
        Self = self(),
        Sender = fun(_StationId, Cbor) ->
            Self ! {sent, Cbor}, ok
        end,
        Stations = [#{address => ?REMOTE, station => <<"s">>, send => Sender}],
        ok = macula_route_packet:configure(
               #{own_address => ?OWN, stations => Stations}),
        ok = macula_deliver_packet:configure(#{
            local_addresses => [?OWN],
            tun_writer => fun(P) -> Self ! {tun, P}, ok end
        }),
        Payload = crypto:strong_rand_bytes(60 * 1024),
        Pkt     = ipv6_packet(?OWN, ?REMOTE, Payload),
        {ok, _} = macula_route_packet:dispatch(Pkt),
        Cbor = receive {sent, C} -> C
               after 1000 -> error(no_send)
               end,
        %% Decode and feed back as ingress (with dst flipped to OWN):
        {ok, Decoded} = macula_cbor_nif:unpack(Cbor),
        Roundtrip = macula_route_packet:encapsulate(
                      maps:get(<<"payload">>, Decoded), ?REMOTE, ?OWN),
        ok = macula_deliver_packet:handle_envelope(Roundtrip),
        Tun = receive {tun, P} -> P after 1000 -> error(no_tun) end,
        ?assertEqual(byte_size(Pkt), byte_size(Tun)),
        ?assertEqual(Pkt, Tun)
    end.

%% --- §5 #5 — metric registration ------------------------------------

path_mtu_metric_registered(_) ->
    fun() ->
        Snap = macula_metrics:gather(),
        Names = [N || #{name := N} <- Snap],
        ?assert(lists:member(<<"macula_net_transport_path_mtu_bytes">>, Names)),
        %% Emit a synthetic event and verify the gauge picks it up.
        telemetry:execute([macula, net, transport, path_mtu],
                          #{bytes => 1480},
                          #{peer => <<"abcdef01">>}),
        Snap2 = macula_metrics:gather(),
        [G] = [M || #{name := N} = M <- Snap2,
                    N =:= <<"macula_net_transport_path_mtu_bytes">>],
        Samples = maps:get(samples, G),
        Match = [V || #{labels := L, value := V} <- Samples,
                      proplists:get_value(peer, L) =:= <<"abcdef01">>],
        ?assertEqual([1480], Match)
    end.

%% --- helpers --------------------------------------------------------

ipv6_packet(Src, Dst, Payload) ->
    PayloadLen = byte_size(Payload),
    Header = <<6:4, 0:8, 0:20, PayloadLen:16, 59:8, 64:8,
               Src/binary, Dst/binary>>,
    <<Header/binary, Payload/binary>>.

%% Set up a Quinn server + client over loopback. Returns
%% {ok, ServerConn, ClientConn, Cleanup}. Both connections share the
%% same pubkey-pinned cert produced inline.
setup_loopback_pair() ->
    Self = self(),
    {Pub, Priv} = ephemeral_keypair(),
    {ok, {CertPem, KeyPem}} = macula_quic:generate_self_signed_cert(
                                  Pub, Priv, [<<"localhost">>, <<"127.0.0.1">>]),
    Tmp  = lists:flatten(io_lib:format("/tmp/macula-mtu-~p", [erlang:unique_integer([positive])])),
    Cert = Tmp ++ ".crt",
    Key  = Tmp ++ ".key",
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key,  KeyPem),
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
    %% Wait for server's new_conn message.
    ServerConn = receive
        {quic, new_conn, C, _Info} -> C
    after 5000 ->
        error(no_server_conn)
    end,
    Cleanup = fun() ->
        catch macula_quic:close_connection(ClientConn),
        catch macula_quic:close_connection(ServerConn),
        catch macula_quic:close_listener(Listener),
        file:delete(Cert),
        file:delete(Key),
        Self ! cleaned,
        receive cleaned -> ok after 0 -> ok end
    end,
    {ok, ServerConn, ClientConn, Cleanup}.

ephemeral_keypair() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {iolist_to_binary(Pub), iolist_to_binary(Priv)}.

pick_free_port() ->
    {ok, S} = gen_udp:open(0, [binary, {ip, {127,0,0,1}}]),
    {ok, P} = inet:port(S),
    gen_udp:close(S),
    P.
