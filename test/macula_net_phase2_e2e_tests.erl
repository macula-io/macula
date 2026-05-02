%%%-------------------------------------------------------------------
%%% @doc Phase 2 end-to-end integration test for macula-net.
%%%
%%% Wires the four Phase 2 slices together against a real
%%% `macula_net_transport_quic' between two BEAM nodes (no TUN —
%%% transport-level only). The DHT is replaced with a tiny in-memory
%%% map living on the test node; both peers `put_fn'/`find_fn' against
%%% it via erpc so the records they advertise are mutually visible.
%%%
%%% Flow under test:
%%%
%%%   peer A                                       peer B
%%%   ┌──────────────────────────────┐            ┌──────────────────────────────┐
%%%   │ advertise_station -> put_fn  │--rpc-->    │                              │
%%%   │ route_packet (dht mode):     │            │ transport_quic (listener)    │
%%%   │   cache miss -> resolve --rpc│-->         │ deliver_packet:configure     │
%%%   │   connect to host:port       │----QUIC--->│ deliver_packet:handle_envelope│
%%%   │   send envelope              │            │ -> tun_writer captures payload│
%%%   └──────────────────────────────┘            └──────────────────────────────┘
%%%
%%% Acceptance:
%%%   * peer A can resolve peer B's address and reach it without any
%%%     pre-configured station table.
%%%   * the spoof case (attacker signs a redirect for B's address) is
%%%     rejected; route_packet returns `{error, bad_address_binding}'.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_phase2_e2e_tests).

-include_lib("eunit/include/eunit.hrl").

%% Exported because the peer nodes RPC into this module.
-export([dht_put/1, dht_find/1]).

-define(REALM,         <<16#11:256>>).
-define(LISTEN_PORT_A, 44500).
-define(LISTEN_PORT_B, 44501).
-define(DHT_NAME,      macula_net_phase2_dht).
-define(SINK_NAME,     macula_net_phase2_sink).

%% =============================================================================
%% Top-level fixture
%% =============================================================================

phase2_e2e_test_() ->
    {timeout, 60,
     {setup, fun setup/0, fun cleanup/1, fun(Ctx) ->
        [
            {"resolves and delivers via DHT path",
             ?_test(resolves_and_delivers(Ctx))},
            {"spoofed redirect is rejected",
             ?_test(spoofed_redirect_rejected(Ctx))}
        ]
     end}}.

%% =============================================================================
%% Mock DHT — runs on the test node, both peers RPC into it
%% =============================================================================

mock_dht_loop(Tab) ->
    receive
        {put, Record, From} ->
            Key = macula_record:storage_key(Record),
            ets:insert(Tab, {Key, Record}),
            From ! {ok, put},
            mock_dht_loop(Tab);
        {find, Key, From} ->
            Reply = case ets:lookup(Tab, Key) of
                [{_, R}] -> {ok, R};
                []       -> {error, not_found}
            end,
            From ! {find_reply, Reply},
            mock_dht_loop(Tab);
        {wipe, From} ->
            ets:delete_all_objects(Tab),
            From ! {ok, wiped},
            mock_dht_loop(Tab);
        stop ->
            ets:delete(Tab)
    end.

dht_put(Record) ->
    ?DHT_NAME ! {put, Record, self()},
    receive {ok, put} -> ok after 1000 -> {error, dht_timeout} end.

dht_find(Key) ->
    ?DHT_NAME ! {find, Key, self()},
    receive {find_reply, R} -> R after 1000 -> {error, dht_timeout} end.

%% =============================================================================
%% Setup / cleanup
%% =============================================================================

setup() ->
    ensure_dist(),
    Tab = ets:new(phase2_dht_table, [set, public]),
    catch unregister(?DHT_NAME),
    DhtPid = spawn(fun() -> mock_dht_loop(Tab) end),
    register(?DHT_NAME, DhtPid),

    StationA = new_station(),
    StationB = new_station(),

    {ok, PeerAPid, PeerANode} = start_peer(),
    {ok, PeerBPid, PeerBNode} = start_peer(),
    ok = preload_code_on_peer(PeerANode),
    ok = preload_code_on_peer(PeerBNode),

    %% Peer B: stand up the inbound side. Listener + handler =
    %% deliver_packet, configured with B's address; tun_writer
    %% delivers payloads back to the test node sink.
    TestNode = node(),
    {ok, _HolderB} = peer_call(PeerBNode, fun() ->
        Caller = self(),
        H = spawn(fun() ->
            process_flag(trap_exit, true),
            {ok, _} = macula_net_transport_quic:start_link(
                        #{port => ?LISTEN_PORT_B}),
            ok = macula_net_transport_quic:set_handler(
                    fun(Cbor, _StreamRef) ->
                        macula_deliver_packet:handle_envelope(Cbor)
                    end),
            Sink = {?SINK_NAME, TestNode},
            Writer = fun(Pkt) -> Sink ! {delivered, Pkt}, ok end,
            ok = macula_deliver_packet:configure(#{
                local_addresses => [maps:get(addr, StationB)],
                tun_writer      => Writer
            }),
            Caller ! {ready, self()},
            holder_loop()
        end),
        receive {ready, H} -> {ok, H} after 5000 -> {error, holder_timeout} end
    end),

    %% Peer A: outbound side. Transport listener (so it can connect
    %% out from a real QUIC endpoint) plus cache_route + route_packet
    %% in dht mode. The find_fn / connect_fn / send_fn / put_fn are
    %% wired to RPC-back calls into the test node.
    {ok, _HolderA} = peer_call(PeerANode, fun() ->
        Caller = self(),
        H = spawn(fun() ->
            process_flag(trap_exit, true),
            {ok, _} = macula_net_transport_quic:start_link(
                        #{port => ?LISTEN_PORT_A}),
            {ok, _} = macula_cache_route:start_link(#{sweep_ms => 30_000}),

            FindFn = fun(K) -> erpc:call(TestNode, ?MODULE, dht_find, [K]) end,
            ConnFn = fun(StationId, Host, Port) ->
                macula_net_transport_quic:connect(StationId, Host, Port)
            end,
            SendFn = fun(StationId, Cbor) ->
                macula_net_transport_quic:send(StationId, Cbor)
            end,
            ok = macula_route_packet:configure(#{
                own_address => maps:get(addr, StationA),
                resolver    => #{realm_pubkey => ?REALM,
                                 find_fn      => FindFn,
                                 connect_fn   => ConnFn,
                                 send_fn      => SendFn}
            }),
            Caller ! {ready, self()},
            holder_loop()
        end),
        receive {ready, H} -> {ok, H} after 5000 -> {error, holder_timeout} end
    end),

    #{peer_a => PeerAPid, peer_b => PeerBPid,
      node_a => PeerANode, node_b => PeerBNode,
      station_a => StationA, station_b => StationB,
      dht_pid => DhtPid}.

cleanup(#{peer_a := PA, peer_b := PB, dht_pid := Dht}) ->
    catch peer:stop(PA),
    catch peer:stop(PB),
    catch (Dht ! stop),
    catch unregister(?DHT_NAME),
    ok.

holder_loop() ->
    receive _ -> holder_loop() end.

%% =============================================================================
%% Tests
%% =============================================================================

resolves_and_delivers(#{node_a := NodeA, station_a := StationA,
                         station_b := StationB} = _Ctx) ->
    catch unregister(?SINK_NAME),
    true = register(?SINK_NAME, self()),

    %% Both peers' identities advertise into the shared DHT (we just
    %% feed records straight into the table — the put_fn end of
    %% advertise_station isn't load-bearing here).
    ok = advertise_into_dht(StationA, ?LISTEN_PORT_A, <<"127.0.0.1">>),
    ok = advertise_into_dht(StationB, ?LISTEN_PORT_B, <<"127.0.0.1">>),

    %% Synthesise an IPv6 packet on peer A and dispatch.
    AddrA = maps:get(addr, StationA),
    AddrB = maps:get(addr, StationB),
    Pkt   = ipv6_packet(AddrA, AddrB, <<"phase2-acceptance">>),
    {ok, _StationId} = erpc:call(NodeA, macula_route_packet, dispatch, [Pkt]),

    receive
        {delivered, Got} ->
            ?assertEqual(Pkt, Got)
    after 5000 ->
        erlang:error(timeout_waiting_for_delivery)
    end.

spoofed_redirect_rejected(#{node_a := NodeA, station_a := StationA,
                             station_b := StationB} = _Ctx) ->
    %% Wipe both the DHT and peer A's route cache so test 1's
    %% legitimate resolution doesn't make this case a cache hit.
    ?DHT_NAME ! {wipe, self()},
    receive {ok, wiped} -> ok after 1000 -> error(wipe_timeout) end,
    ok = erpc:call(NodeA, macula_cache_route, invalidate,
                   [maps:get(addr, StationB)]),

    %% Attacker publishes an address_pubkey_map for StationB's address
    %% but signed by an unrelated key — derive_address(realm, attacker)
    %% won't equal StationB's address.
    AttackerKp = macula_identity:generate(),
    AttackerPk = macula_identity:public(AttackerKp),
    Spoof = macula_record:sign(
              macula_record:address_pubkey_map(
                AttackerPk, maps:get(addr, StationB)),
              AttackerKp),
    ok = dht_put(Spoof),

    AddrA = maps:get(addr, StationA),
    AddrB = maps:get(addr, StationB),
    Pkt   = ipv6_packet(AddrA, AddrB, <<"should-not-go-through">>),
    Res   = erpc:call(NodeA, macula_route_packet, dispatch, [Pkt]),
    ?assertEqual({error, bad_address_binding}, Res).

%% =============================================================================
%% Helpers
%% =============================================================================

new_station() ->
    Kp = macula_identity:generate(),
    Pk = macula_identity:public(Kp),
    Addr = macula_address:derive(?REALM, Pk),
    #{kp => Kp, pk => Pk, addr => Addr}.

advertise_into_dht(#{kp := Kp, pk := Pk, addr := Addr}, Port, Host) ->
    EndpointR = macula_record:sign(
                  macula_record:station_endpoint(Pk, Port, #{
                      host_advertised => [Host]
                  }), Kp),
    RedirectR = macula_record:sign(
                  macula_record:address_pubkey_map(Pk, Addr), Kp),
    ok = dht_put(EndpointR),
    ok = dht_put(RedirectR),
    ok.

ipv6_packet(Src, Dst, Body) ->
    Hdr = <<6:4, 0:8, 0:20, (byte_size(Body)):16, 59:8, 64:8,
            Src/binary, Dst/binary>>,
    <<Hdr/binary, Body/binary>>.

ensure_dist() ->
    case net_kernel:get_state() of
        #{started := no} ->
            {ok, _} = net_kernel:start([macula_net_phase2_test, shortnames]),
            ok;
        _ ->
            ok
    end.

start_peer() ->
    Args = ["-pa" | code:get_path()],
    peer:start_link(#{name => peer:random_name(),
                      args => Args,
                      connection => standard_io,
                      shutdown => 5000}).

preload_code_on_peer(Node) ->
    Mods = [macula_net_transport_quic, macula_net_transport, macula_quic,
            macula_cbor_nif, macula_blake3_nif, macula_crypto_nif,
            macula_address, macula_record, macula_record_cbor, macula_record_uuid,
            macula_identity, macula_route_packet, macula_route_packet_ipv6,
            macula_resolve_address, macula_cache_route,
            macula_deliver_packet,
            crypto, public_key],
    lists:foreach(fun(M) ->
        {Mod, Bin, File} = code:get_object_code(M),
        {module, Mod} = erpc:call(Node, code, load_binary, [Mod, File, Bin])
    end, Mods),
    %% Force NIF on_load so later calls don't race.
    _ = erpc:call(Node, macula_cbor_nif, pack, [#{<<"k">> => 1}]),
    _ = erpc:call(Node, macula_blake3_nif, hash, [<<"x">>]),
    ok.

peer_call(Node, Fun) ->
    erpc:call(Node, erlang, apply, [Fun, []]).
