%%%-------------------------------------------------------------------
%%% @doc Two-node end-to-end test for the full macula-net stack
%%% (sans TUN device).
%%%
%%% On node A:
%%%   - configure macula_route_packet with own address + station -> peer
%%%   - synthesize an IPv6 packet, dispatch it
%%%
%%% On node B:
%%%   - configure macula_deliver_packet with local addresses +
%%%     tun_writer that forwards captured payloads back to the test
%%%   - QUIC transport handler = macula_deliver_packet:handle_envelope
%%%
%%% Verify the IPv6 payload arrives byte-for-byte on node B.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_full_stack_e2e_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LISTEN_PORT_A, 44402).
-define(LISTEN_PORT_B, 44403).
-define(STATION_B,    <<"peer-station">>).
-define(WAIT_MS,      3000).
-define(PAYLOAD_SINK, macula_net_full_stack_payload_sink).

%% Reuse a fixed realm + two distinct identity pubkeys; the addresses
%% derived are stable per-test so we can refer to them by name.
-define(REALM_PK, <<16#11:256>>).
-define(IDENT_A_PK, <<16#aa:256>>).
-define(IDENT_B_PK, <<16#bb:256>>).

%% =============================================================================
%% Top-level fixture
%% =============================================================================

full_stack_e2e_test_() ->
    {timeout, 30,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun run_full_stack/1}}.

%% =============================================================================
%% Setup / cleanup
%% =============================================================================

setup() ->
    ensure_dist(),
    {ok, PeerPid, PeerNode} = start_peer_node(),
    ok = preload_code_on_peer(PeerNode),
    TestNode = node(),

    AddrA = macula_address:derive(?REALM_PK, ?IDENT_A_PK),
    AddrB = macula_address:derive(?REALM_PK, ?IDENT_B_PK),

    %% Peer (B): start QUIC listener, set handler = deliver_packet,
    %% configure deliver_packet with local addrs + capturing tun_writer.
    {ok, Holder} = peer_call(PeerNode, fun() ->
        Caller = self(),
        H = spawn(fun() ->
            process_flag(trap_exit, true),
            {ok, _} = macula_net_transport_quic:start_link(
                        #{port => ?LISTEN_PORT_B}),
            ok = macula_net_transport_quic:set_handler(
                    fun macula_deliver_packet:handle_envelope/1),
            Sink = {?PAYLOAD_SINK, TestNode},
            Writer = fun(Pkt) -> Sink ! {delivered, Pkt}, ok end,
            ok = macula_deliver_packet:configure(#{
                local_addresses => [AddrB],
                tun_writer      => Writer
            }),
            Caller ! {ready, self()},
            holder_loop()
        end),
        receive {ready, H} -> {ok, H} after 5000 -> {error, holder_timeout} end
    end),

    %% Local (A): start QUIC transport (so we have a send pipe).
    {ok, _} = macula_net_transport_quic:start_link(#{port => ?LISTEN_PORT_A}),

    %% Configure route_packet: A's own addr is AddrA, single station
    %% entry mapping AddrB -> peer-station via QUIC send.
    SendFun = fun(StationId, Cbor) ->
        macula_net_transport_quic:send(StationId, Cbor)
    end,
    ok = macula_route_packet:configure(#{
        own_address => AddrA,
        stations    => [#{address => AddrB,
                          station => ?STATION_B,
                          send    => SendFun}]
    }),
    ok = macula_net_transport_quic:connect(
           ?STATION_B, <<"127.0.0.1">>, ?LISTEN_PORT_B),
    timer:sleep(150),  %% let stream accept settle

    #{peer => PeerPid, peer_node => PeerNode, holder => Holder,
      addr_a => AddrA, addr_b => AddrB}.

cleanup(#{peer := PeerPid}) ->
    catch macula_net_transport_quic:stop(),
    catch peer:stop(PeerPid),
    ok.

holder_loop() ->
    receive _ -> holder_loop() end.

%% =============================================================================
%% Test body
%% =============================================================================

run_full_stack(#{addr_a := AddrA, addr_b := AddrB}) ->
    fun() ->
        catch unregister(?PAYLOAD_SINK),
        true = register(?PAYLOAD_SINK, self()),

        Payload = ipv6_packet(AddrA, AddrB, <<"phase-1-acceptance">>),
        {ok, ?STATION_B} = macula_route_packet:dispatch(Payload),
        receive
            {delivered, Got} ->
                ?assertEqual(Payload, Got)
        after ?WAIT_MS ->
            erlang:error(timeout_waiting_payload)
        end
    end.

%% =============================================================================
%% Helpers
%% =============================================================================

%% Build a minimum-viable IPv6 packet (header + payload) using NextHeader
%% 59 (no-next-header) so we don't have to construct an inner protocol.
ipv6_packet(Src, Dst, PayloadBody) ->
    PayloadLen = byte_size(PayloadBody),
    Header = <<6:4, 0:8, 0:20,
               PayloadLen:16, 59:8, 64:8,
               Src/binary, Dst/binary>>,
    <<Header/binary, PayloadBody/binary>>.

ensure_dist() ->
    case net_kernel:get_state() of
        #{started := no} ->
            {ok, _} = net_kernel:start([macula_net_full_test, shortnames]),
            ok;
        _ ->
            ok
    end.

start_peer_node() ->
    Args = ["-pa" | code:get_path()],
    peer:start_link(#{name => peer:random_name(),
                      args => Args,
                      connection => standard_io,
                      shutdown => 5000}).

preload_code_on_peer(Node) ->
    Mods = [macula_net_transport_quic, macula_quic, macula_cbor_nif,
            macula_blake3_nif, macula_crypto_nif, macula_net_transport,
            macula_address, macula_deliver_packet, macula_route_packet,
            macula_route_packet_ipv6,
            crypto, public_key],
    lists:foreach(fun(M) ->
        {Mod, Bin, File} = code:get_object_code(M),
        {module, Mod} = erpc:call(Node, code, load_binary, [Mod, File, Bin])
    end, Mods),
    %% Force NIF on_load.
    _ = erpc:call(Node, macula_cbor_nif, pack, [#{<<"k">> => 1}]),
    _ = erpc:call(Node, macula_blake3_nif, hash, [<<"x">>]),
    ok.

peer_call(Node, Fun) ->
    erpc:call(Node, erlang, apply, [Fun, []]).
