%%%-------------------------------------------------------------------
%%% @doc Phase 3 §8 acceptance #4 — full Alice ↔ Bob daemon-to-daemon.
%%%
%%% Four peer nodes plus the test process as the DHT host:
%%%
%%%   Alice  (daemon)   ───┐
%%%                        ├── Helsinki (host station)
%%%                        │        │
%%%                        │        │  Phase 2 station-to-station route
%%%                        │        ▼
%%%                        ├── Nuremberg (host station)
%%%   Bob    (daemon)   ───┘
%%%
%%% Alice attaches to Helsinki, Bob attaches to Nuremberg. Both host
%%% stations participate in the same shared (mock) DHT. The §7.1
%%% three-way controller split closes the loop:
%%%
%%%   * Alice -> Helsinki: attach stream carries a data envelope with
%%%     dst = Bob's address. Helsinki's controller sees dst is not
%%%     hosted on it (Alice yes, Bob no) and not Helsinki's own addr,
%%%     so it falls into forward_fn = route_packet:dispatch_envelope.
%%%   * route_packet resolves Bob via the DHT (finds Bob's
%%%     hosted_address_map signed by Nuremberg, then Nuremberg's
%%%     station_endpoint), dials Nuremberg, ships the same envelope.
%%%   * Nuremberg's transport receives. Its controller sees dst=Bob,
%%%     hosted on Nuremberg, forwards on Bob's attach stream.
%%%   * Bob's daemon process logs the inbound envelope.
%%%
%%% The reverse direction (Bob -> Alice) is the same chain mirrored.
%%% The test asserts both directions byte-identically.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_phase3_alice_bob_e2e_tests).

-include_lib("eunit/include/eunit.hrl").

%% Exported because peers RPC into this module.
-export([dht_put/1, dht_find/1, peer_send_envelope/3, never/1]).

-define(REALM,             <<16#11:256>>).
-define(HELSINKI_PORT,     44600).
-define(NUREMBERG_PORT,    44601).
-define(ALICE_PORT,        44602).
-define(BOB_PORT,          44603).
-define(HELSINKI_STATION,  <<"helsinki-3.alicebob">>).
-define(NUREMBERG_STATION, <<"nuremberg-3.alicebob">>).
-define(DHT_NAME,          macula_net_phase3_alicebob_dht).
-define(SINK_ALICE,        macula_net_phase3_alicebob_alice_sink).
-define(SINK_BOB,          macula_net_phase3_alicebob_bob_sink).

%% =============================================================================
%% Top-level
%% =============================================================================

phase3_alice_bob_test_() ->
    {timeout, 60,
     {setup, fun setup/0, fun cleanup/1, fun(Ctx) ->
        [
            {"alice -> bob byte-identically",
             ?_test(alice_to_bob(Ctx))},
            {"bob -> alice byte-identically",
             ?_test(bob_to_alice(Ctx))}
        ]
     end}}.

%% =============================================================================
%% Mock DHT (lives on the test node; both stations RPC in)
%% =============================================================================

dht_loop(Tab) ->
    receive
        {put, Record, From} ->
            ets:insert(Tab, {macula_record:storage_key(Record), Record}),
            From ! {ok, put},
            dht_loop(Tab);
        {find, Key, From} ->
            Reply = case ets:lookup(Tab, Key) of
                [{_, R}] -> {ok, R};
                []       -> {error, not_found}
            end,
            From ! {find_reply, Reply},
            dht_loop(Tab);
        stop ->
            catch ets:delete(Tab),
            ok
    end.

dht_put(Record) ->
    ?DHT_NAME ! {put, Record, self()},
    receive {ok, put} -> ok after 1000 -> {error, dht_timeout} end.

dht_find(Key) ->
    ?DHT_NAME ! {find, Key, self()},
    receive {find_reply, R} -> R after 1000 -> {error, dht_timeout} end.

never(_X) -> ok.

%% Exported helper so the test can dispatch a send through one of the
%% peer-side handles via erpc.
peer_send_envelope(Handle, Cbor, _Marker) when is_pid(Handle) ->
    macula_attach_identity:send(Handle, Cbor).

%% =============================================================================
%% Setup / cleanup
%% =============================================================================

setup() ->
    ensure_dist(),
    Tab = ets:new(macula_net_phase3_alicebob_dht_table, [set, public]),
    catch unregister(?DHT_NAME),
    DhtPid = spawn(fun() -> dht_loop(Tab) end),
    register(?DHT_NAME, DhtPid),

    Alice     = new_actor(),
    Helsinki  = new_actor(),
    Nuremberg = new_actor(),
    Bob       = new_actor(),

    {ok, PeerH, NodeH} = start_peer(),
    {ok, PeerN, NodeN} = start_peer(),
    {ok, PeerA, NodeA} = start_peer(),
    {ok, PeerB, NodeB} = start_peer(),
    ok = preload_code_on_peer(NodeH),
    ok = preload_code_on_peer(NodeN),
    ok = preload_code_on_peer(NodeA),
    ok = preload_code_on_peer(NodeB),

    TestNode = node(),

    %% --- Helsinki (host station) -------------------------------------------
    setup_host_station(NodeH,
                        ?HELSINKI_STATION,
                        Helsinki,
                        ?HELSINKI_PORT,
                        TestNode),
    %% --- Nuremberg (host station) ------------------------------------------
    setup_host_station(NodeN,
                        ?NUREMBERG_STATION,
                        Nuremberg,
                        ?NUREMBERG_PORT,
                        TestNode),

    %% --- Alice (daemon) ---------------------------------------------------
    %% Daemon attaches to Helsinki. Captures inbound envelopes back to
    %% the test node sink. Returns the attach handle so the test can
    %% drive sends via erpc.
    AliceHandle = setup_daemon(NodeA,
                                Alice,
                                ?ALICE_PORT,
                                Helsinki,
                                ?HELSINKI_STATION,
                                ?HELSINKI_PORT,
                                ?SINK_ALICE,
                                TestNode),

    %% --- Bob (daemon) -----------------------------------------------------
    BobHandle = setup_daemon(NodeB,
                              Bob,
                              ?BOB_PORT,
                              Nuremberg,
                              ?NUREMBERG_STATION,
                              ?NUREMBERG_PORT,
                              ?SINK_BOB,
                              TestNode),

    %% Give attach + advertise loops time to publish into the shared DHT.
    timer:sleep(500),

    #{peer_h => PeerH, peer_n => PeerN, peer_a => PeerA, peer_b => PeerB,
      node_h => NodeH, node_n => NodeN, node_a => NodeA, node_b => NodeB,
      alice => Alice, helsinki => Helsinki,
      nuremberg => Nuremberg, bob => Bob,
      alice_handle => AliceHandle, bob_handle => BobHandle,
      dht_pid => DhtPid}.

cleanup(#{peer_h := PH, peer_n := PN, peer_a := PA, peer_b := PB,
          dht_pid := Dht}) ->
    catch peer:stop(PH),
    catch peer:stop(PN),
    catch peer:stop(PA),
    catch peer:stop(PB),
    catch (Dht ! stop),
    catch unregister(?DHT_NAME),
    catch unregister(?SINK_ALICE),
    catch unregister(?SINK_BOB),
    flush(),
    ok.

flush() ->
    receive _ -> flush() after 0 -> ok end.

setup_host_station(Node, StationId, Station, Port, TestNode) ->
    {ok, _Holder} = peer_call(Node, fun() ->
        Caller = self(),
        H = spawn(fun() ->
            process_flag(trap_exit, true),
            {ok, _} = macula_net_transport_quic:start_link(#{port => Port}),
            {ok, _} = macula_cache_route:start_link(#{sweep_ms => 30_000}),
            FindFn = fun(K) ->
                erpc:call(TestNode, ?MODULE, dht_find, [K])
            end,
            PutFn  = fun(R) ->
                erpc:call(TestNode, ?MODULE, dht_put, [R])
            end,
            ConnectFn = fun(SId, Host, P) ->
                handle_connect_result(macula_net_transport_quic:connect(SId, Host, P))
            end,
            SendFn = fun macula_net_transport_quic:send/2,
            %% advertise_station so the OTHER station can find this one
            %% via the DHT during forwarding.
            {ok, _} = macula_advertise_station:start_link(#{
                realm_pubkey     => ?REALM,
                identity_pubkey  => maps:get(pk, Station),
                identity_privkey => maps:get(kp, Station),
                quic_port        => Port,
                addresses        => [maps:get(addr, Station)],
                advertised_ips   => [<<"127.0.0.1">>],
                put_fn           => PutFn,
                refresh_ms       => 60_000
            }),
            ok = macula_route_packet:configure(#{
                own_address => maps:get(addr, Station),
                resolver    => #{realm_pubkey => ?REALM,
                                 find_fn      => FindFn,
                                 connect_fn   => ConnectFn,
                                 send_fn      => SendFn}
            }),
            {ok, _} = macula_host_identity:start_link(#{
                realm_pubkey => ?REALM,
                host_pubkey  => maps:get(pk, Station),
                host_privkey => maps:get(kp, Station),
                put_fn       => PutFn,
                refresh_ms   => 60_000
            }),
            {ok, _} = macula_host_attach_controller:start_link(#{
                realm_pubkey   => ?REALM,
                host_pubkey    => maps:get(pk, Station),
                attach_send_fn => fun macula_quic:send/2,
                lookup_fn      => fun macula_host_identity:lookup/1,
                attach_fn      => fun macula_host_identity:attach/4,
                fallback_fn    => fun ?MODULE:never/1,
                forward_fn     => fun macula_route_packet:dispatch_envelope/2
            }),
            ok = macula_net_transport_quic:set_handler(
                    fun macula_host_attach_controller:handle/2),
            _ = StationId,  %% reserved for log/diagnostic; not used.
            Caller ! {ready, self()},
            holder_loop()
        end),
        receive {ready, H} -> {ok, H} after 5000 -> {error, holder_timeout} end
    end).

setup_daemon(Node, Daemon, OwnPort, Host, _HostStation, HostPort,
             SinkName, TestNode) ->
    HostPk = maps:get(pk, Host),
    HostHostStr = "127.0.0.1",
    SinkRef = {SinkName, TestNode},
    %% Spawn a holder process on the peer that keeps the transport
    %% gen_server + attach_identity gen_server alive. peer_call's
    %% erpc:call would otherwise leave them linked to a temporary
    %% RPC process that dies as soon as the call returns.
    {ok, Handle} = peer_call(Node, fun() ->
        Caller = self(),
        Holder = spawn(fun() ->
            process_flag(trap_exit, true),
            {ok, _} = macula_net_transport_quic:start_link(#{port => OwnPort}),
            InboundHandler = fun(Cbor) ->
                SinkRef ! {inbound, Cbor}, ok
            end,
            {ok, AttachHandle} = macula_attach_identity:attach(
                #{station_pubkey => HostPk,
                  host           => HostHostStr,
                  port           => HostPort},
                ?REALM,
                maps:get(kp, Daemon),
                #{inbound_handler => InboundHandler}),
            Caller ! {ready, AttachHandle},
            holder_loop()
        end),
        receive
            {ready, AttachHandle} -> {ok, AttachHandle}
        after 5000 ->
            exit(Holder, kill),
            {error, daemon_holder_timeout}
        end
    end),
    Handle.

handle_connect_result(ok)                          -> ok;
handle_connect_result({error, already_connected})  -> ok;
handle_connect_result({error, _} = E)              -> E.

holder_loop() ->
    receive _ -> holder_loop() end.

%% =============================================================================
%% Tests
%% =============================================================================

alice_to_bob(#{node_a := NodeA, alice_handle := AliceHandle,
                alice := Alice, bob := Bob} = _Ctx) ->
    safe_register(?SINK_BOB),
    Envelope = build_envelope(maps:get(addr, Alice), maps:get(addr, Bob),
                               <<"alice -> bob payload">>),
    ok = erpc:call(NodeA, ?MODULE, peer_send_envelope,
                   [AliceHandle, Envelope, alice_to_bob]),
    receive
        {inbound, Got} ->
            ?assertEqual(Envelope, Got)
    after 5000 ->
        erlang:error(timeout_alice_to_bob)
    end.

bob_to_alice(#{node_b := NodeB, bob_handle := BobHandle,
                alice := Alice, bob := Bob} = _Ctx) ->
    safe_register(?SINK_ALICE),
    Envelope = build_envelope(maps:get(addr, Bob), maps:get(addr, Alice),
                               <<"bob -> alice payload">>),
    ok = erpc:call(NodeB, ?MODULE, peer_send_envelope,
                   [BobHandle, Envelope, bob_to_alice]),
    receive
        {inbound, Got} ->
            ?assertEqual(Envelope, Got)
    after 5000 ->
        erlang:error(timeout_bob_to_alice)
    end.

%% =============================================================================
%% Helpers
%% =============================================================================

safe_register(Name) ->
    %% Each test runs in the same process under {inorder, ...}; a
    %% process can only hold one registered name at a time, so drop
    %% the previous test's name before claiming this one.
    drop_my_existing_registration(),
    catch unregister(Name),
    flush(),
    true = register(Name, self()),
    ok.

drop_my_existing_registration() ->
    case erlang:process_info(self(), registered_name) of
        {registered_name, OldName} -> catch unregister(OldName), ok;
        _                           -> ok
    end.

build_envelope(Src, Dst, Payload) ->
    macula_cbor_nif:pack(#{
        <<"v">>       => 1,
        <<"type">>    => <<"data">>,
        <<"src">>     => Src,
        <<"dst">>     => Dst,
        <<"payload">> => Payload
    }).

new_actor() ->
    Kp = macula_identity:generate(),
    Pk = macula_identity:public(Kp),
    Addr = macula_address:derive(?REALM, Pk),
    #{kp => Kp, pk => Pk, addr => Addr}.

ensure_dist() ->
    handle_dist(net_kernel:get_state()).

handle_dist(#{started := no}) ->
    {ok, _} = net_kernel:start([macula_net_phase3_alicebob_test, shortnames]),
    ok;
handle_dist(_) ->
    ok.

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
            macula_advertise_station,
            macula_host_identity, macula_attach_identity,
            macula_host_attach_controller,
            ?MODULE,
            crypto, public_key],
    lists:foreach(fun(M) ->
        {Mod, Bin, File} = code:get_object_code(M),
        {module, Mod} = erpc:call(Node, code, load_binary, [Mod, File, Bin])
    end, Mods),
    _ = erpc:call(Node, macula_cbor_nif, pack, [#{<<"k">> => 1}]),
    _ = erpc:call(Node, macula_blake3_nif, hash, [<<"x">>]),
    ok.

peer_call(Node, Fun) ->
    erpc:call(Node, erlang, apply, [Fun, []]).
