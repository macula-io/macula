%%%-------------------------------------------------------------------
%%% @doc Phase 3.6 end-to-end integration test.
%%%
%%% Three roles, two peer nodes:
%%%
%%%   * Host (peer node H) — runs transport, host_identity,
%%%     host_attach_controller. Wires the controller as the transport
%%%     handler. Owns the inbound stream from the daemon.
%%%   * Daemon (peer node D) — runs transport + attach_identity.
%%%     Dials the host. Inbound handler captures forwarded data into
%%%     a sink on the test node so the test can assert byte-identical
%%%     delivery.
%%%   * Sender (test node) — runs transport. Dials the host. Sends a
%%%     macula-net data envelope with `dst' = daemon's address.
%%%
%%% Acceptance: the data envelope arrives at the daemon's inbound
%%% handler byte-identically. The host's host_identity table records
%%% the daemon as hosted; the controller's lookup_fn -> attach_send_fn
%%% chain forwards the envelope on the same bidi stream the daemon
%%% dialed in on.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_phase3_6_e2e_tests).

-include_lib("eunit/include/eunit.hrl").

%% Exported because peer nodes RPC into this module.
-export([put_fn_noop/1, never/1]).

-define(REALM,         <<16#11:256>>).
-define(HOST_PORT,     44510).
-define(DAEMON_PORT,   44511).
-define(SENDER_PORT,   44512).
-define(HOST_STATION,  <<"host-3-6">>).
-define(SINK,          macula_net_phase3_6_sink).

%% =============================================================================
%% Top-level
%% =============================================================================

phase3_6_test_() ->
    {timeout, 60,
     {setup, fun setup/0, fun cleanup/1, fun(Ctx) ->
        [
            {"data envelope for hosted daemon arrives byte-identically",
             ?_test(envelope_routes_to_daemon(Ctx))}
        ]
     end}}.

%% =============================================================================
%% Helpers exported for RPC
%% =============================================================================

put_fn_noop(_Record) -> ok.

never(_X) -> ok.

%% =============================================================================
%% Setup / cleanup
%% =============================================================================

setup() ->
    ensure_dist(),

    %% Identities.
    Host    = new_actor(),
    Daemon  = new_actor(),
    Sender  = new_actor(),

    {ok, PeerH, NodeH} = start_peer(),
    {ok, PeerD, NodeD} = start_peer(),
    ok = preload_code_on_peer(NodeH),
    ok = preload_code_on_peer(NodeD),

    TestNode = node(),

    %% --- Host node: transport + host_identity + controller ----------------
    {ok, _HolderH} = peer_call(NodeH, fun() ->
        Caller = self(),
        H = spawn(fun() ->
            process_flag(trap_exit, true),
            {ok, _} = macula_net_transport_quic:start_link(
                        #{port => ?HOST_PORT}),
            {ok, _} = macula_host_identity:start_link(#{
                realm_pubkey => ?REALM,
                host_pubkey  => maps:get(pk, Host),
                host_privkey => maps:get(kp, Host),
                put_fn       => fun ?MODULE:put_fn_noop/1,
                refresh_ms   => 60_000
            }),
            {ok, _} = macula_host_attach_controller:start_link(#{
                realm_pubkey   => ?REALM,
                host_pubkey    => maps:get(pk, Host),
                attach_send_fn => fun macula_quic:send/2,
                lookup_fn      => fun macula_host_identity:lookup/1,
                attach_fn      => fun macula_host_identity:attach/4,
                fallback_fn    => fun ?MODULE:never/1
            }),
            ok = macula_net_transport_quic:set_handler(
                    fun macula_host_attach_controller:handle/2),
            Caller ! {ready, self()},
            holder_loop()
        end),
        receive {ready, H} -> {ok, H} after 5000 -> {error, holder_timeout} end
    end),

    %% --- Daemon node: transport + attach_identity ------------------------
    %% The daemon's inbound handler ships every received envelope back to
    %% the test node's sink so the test can ?assertEqual the bytes.
    %% station_pubkey here is the host's REAL Ed25519 pubkey — both
    %% the transport's routing key and the delegation's host_pubkey
    %% derive from it (delegation requires <<_:256>>).
    HostPk  = maps:get(pk, Host),
    SinkRef = {?SINK, TestNode},
    {ok, _HolderD} = peer_call(NodeD, fun() ->
        Caller = self(),
        H = spawn(fun() ->
            process_flag(trap_exit, true),
            {ok, _} = macula_net_transport_quic:start_link(
                        #{port => ?DAEMON_PORT}),
            HostEndpoint = #{
                station_pubkey => HostPk,
                host           => "127.0.0.1",
                port           => ?HOST_PORT
            },
            InboundHandler = fun(Cbor) ->
                SinkRef ! {daemon_inbound, Cbor}, ok
            end,
            {ok, _DaemonHandle} = macula_attach_identity:attach(
                                     HostEndpoint, ?REALM,
                                     maps:get(kp, Daemon),
                                     #{inbound_handler => InboundHandler}),
            Caller ! {ready, self()},
            holder_loop()
        end),
        receive {ready, H} -> {ok, H} after 5000 -> {error, holder_timeout} end
    end),

    %% Give the host a moment to process the attach_v1 frame the
    %% daemon's attach_identity sent on init.
    timer:sleep(300),

    #{peer_h => PeerH, peer_d => PeerD,
      node_h => NodeH, node_d => NodeD,
      host => Host, daemon => Daemon, sender => Sender}.

cleanup(#{peer_h := PH, peer_d := PD}) ->
    catch peer:stop(PH),
    catch peer:stop(PD),
    catch macula_net_transport_quic:stop(),
    catch unregister(?SINK),
    flush(),
    ok.

flush() ->
    receive _ -> flush() after 0 -> ok end.

holder_loop() ->
    receive _ -> holder_loop() end.

%% =============================================================================
%% Test
%% =============================================================================

envelope_routes_to_daemon(#{daemon := Daemon} = _Ctx) ->
    catch unregister(?SINK),
    true = register(?SINK, self()),

    %% Sender's transport on the test node.
    catch macula_net_transport_quic:stop(),
    {ok, _} = macula_net_transport_quic:start_link(
                #{port => ?SENDER_PORT}),
    %% Sender doesn't expect inbound traffic; install a no-op handler
    %% to satisfy the arity-2 contract.
    ok = macula_net_transport_quic:set_handler(
            fun(_Cbor, _StreamRef) -> ok end),

    %% Sender uses an arbitrary station_id (just a routing key into the
    %% transport's `out` map) — only the embedded dst matters for routing.
    SenderStation = <<"sender-3-6">>,
    ok = macula_net_transport_quic:connect(
           SenderStation, "127.0.0.1", ?HOST_PORT),

    Envelope = macula_cbor_nif:pack(#{
        <<"v">>       => 1,
        <<"type">>    => <<"data">>,
        <<"src">>     => <<0:128>>,
        <<"dst">>     => maps:get(addr, Daemon),
        <<"payload">> => <<"phase3.6 acceptance">>
    }),
    ok = macula_net_transport_quic:send(SenderStation, Envelope),

    receive
        {daemon_inbound, Got} ->
            ?assertEqual(Envelope, Got)
    after 5000 ->
        erlang:error(timeout_waiting_for_daemon_delivery)
    end.

%% =============================================================================
%% Misc helpers
%% =============================================================================

new_actor() ->
    Kp = macula_identity:generate(),
    Pk = macula_identity:public(Kp),
    Addr = macula_address:derive(?REALM, Pk),
    #{kp => Kp, pk => Pk, addr => Addr}.

ensure_dist() ->
    handle_dist(net_kernel:get_state()).

handle_dist(#{started := no}) ->
    {ok, _} = net_kernel:start([macula_net_phase3_6_test, shortnames]),
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
            macula_host_identity, macula_attach_identity,
            macula_host_attach_controller,
            ?MODULE,
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
