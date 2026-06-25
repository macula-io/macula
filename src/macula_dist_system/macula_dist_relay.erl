%%%-------------------------------------------------------------------
%%% @doc Relay-routed Erlang distribution.
%%%
%%% When MACULA_DIST_MODE=relay, Erlang distribution connections
%%% are tunneled through the Macula relay mesh instead of direct QUIC.
%%% This enables distribution across firewalls and NATs — nodes only
%%% need outbound connectivity to a relay.
%%%
%%% The tunnel works by:
%%% 1. Node A requests a tunnel via mesh RPC (_dist.tunnel.{node})
%%% 2. Node B creates a gen_tcp loopback pair bridged to relay pub/sub
%%% 3. Node A creates its own loopback pair bridged to the tunnel
%%% 4. dist_util handshake flows through the relay tunnel
%%% 5. Post-handshake: tick keepalive + distribution traffic via bridge
%%%
%%% Bridge processes are supervised by `macula_dist_bridge_sup'
%%% (simple_one_for_one under `macula_dist_system').
%%%
%%% Tunnel bytes are encrypted with AES-256-GCM derived from the
%%% Erlang distribution cookie. The relay cannot read ETF content.
%%%
%%% == Recommended net_ticktime ==
%%%
%%% Relay adds WAN latency to every tick. Increase net_ticktime
%%% to avoid false disconnects:
%%%   -kernel net_ticktime 120
%%% The default 60s may cause spurious node-DOWN on high-latency relays.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay).

-include_lib("kernel/include/logger.hrl").

-export([connect/3]).
-export([is_relay_mode/0, get_mesh_pool/0]).
-export([register_mesh_pool/1]).
-export([advertise_dist_accept/0]).
-export([get_tunnel_metrics/0, get_tunnel_metrics/1]).

%% Must be shorter than OTP's SetupTime. Default SetupTime is 7000ms
%% but we set it to 15000ms via dist_setup_timeout (see macula_dist.erl).
%% If the tunnel RPC takes longer than DIST_TIMEOUT, the caller gets
%% {error, timeout} with diagnostics. If it takes longer than
%% SetupTime, dist_util kills do_setup with zero diagnostics (just pang).
-define(DIST_TIMEOUT, 10000).
-define(CONTROLLER_TIMEOUT, 30000).

%% Realm tag stamped on every dist tunnel frame. Dist tunnel traffic
%% is protocol-internal infrastructure (like the DHT) and not bound
%% to any user realm; the all-zeros tag is the SDK convention for
%% realm-agnostic infrastructure traffic.
-define(DIST_REALM, <<0:256>>).

-define(METRIC_BYTES_OUT, 1).
-define(METRIC_BYTES_IN, 2).
-define(METRIC_MSGS_OUT, 3).
-define(METRIC_MSGS_IN, 4).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Register a Macula V2 pool (`macula_client:pool()') as the
%% carrier for distribution tunnel traffic. Stored in `persistent_term'
%% so the bridge processes (which run inside dist_util's setup process,
%% not in any supervised tree) can pick it up.
-spec register_mesh_pool(pid()) -> ok.
register_mesh_pool(Pid) when is_pid(Pid) ->
    persistent_term:put(macula_dist_mesh_pool, Pid),
    ?LOG_INFO("[dist_relay] Mesh pool registered: ~p", [Pid]),
    ok.

%% @doc Check if relay distribution mode is enabled.
-spec is_relay_mode() -> boolean().
is_relay_mode() ->
    os:getenv("MACULA_DIST_MODE") =:= "relay".

%% @doc Connect to a remote node via relay mesh.
-spec connect(string(), string(), integer()) -> {ok, port(), port()} | {error, term()}.
connect(NodeStr, _Host, _Port) ->
    ?LOG_INFO("[dist_relay] Connecting to ~s via mesh", [NodeStr]),
    case get_mesh_pool() of
        undefined ->
            {error, no_mesh_connection};
        Pool ->
            request_tunnel(Pool, NodeStr)
    end.

%% @doc Advertise this node as accepting distribution connections via relay.
-spec advertise_dist_accept() -> ok.
advertise_dist_accept() ->
    ensure_bridge_sup(),
    case get_mesh_pool() of
        undefined ->
            ?LOG_WARNING("[dist_relay] Cannot advertise — no mesh pool registered"),
            ok;
        Pool ->
            NodeName = atom_to_binary(node()),
            Procedure = <<"_dist.tunnel.", NodeName/binary>>,
            macula_client:advertise(Pool, ?DIST_REALM, Procedure,
                                    fun handle_tunnel_request/1),
            ?LOG_INFO("[dist_relay] Advertised distribution accept: ~s", [Procedure]),
            ok
    end.

%% Bridge supervisor is started by macula_root (application supervisor),
%% so it survives shell crashes and user code failures. This is a no-op
%% kept for backwards compatibility.
ensure_bridge_sup() ->
    case whereis(macula_dist_bridge_sup) of
        undefined ->
            ?LOG_WARNING("[dist_relay] Bridge sup not running — macula app may not be started");
        _Pid ->
            ok
    end.

%% @doc Get metrics for all active tunnels.
-spec get_tunnel_metrics() -> [{binary(), map()}].
get_tunnel_metrics() ->
    case persistent_term:get(macula_dist_tunnels, undefined) of
        undefined -> [];
        Tunnels -> maps:fold(fun collect_tunnel_metric/3, [], Tunnels)
    end.

collect_tunnel_metric(TunnelId, Ref, Acc) ->
    [{TunnelId, read_metrics(Ref)} | Acc].

%% @doc Get metrics for a specific tunnel.
-spec get_tunnel_metrics(binary()) -> map() | undefined.
get_tunnel_metrics(TunnelId) ->
    tunnel_metrics(persistent_term:get(macula_dist_tunnels, undefined), TunnelId).

tunnel_metrics(undefined, _TunnelId) ->
    undefined;
tunnel_metrics(Tunnels, TunnelId) ->
    tunnel_metrics_ref(maps:get(TunnelId, Tunnels, undefined)).

tunnel_metrics_ref(undefined) -> undefined;
tunnel_metrics_ref(Ref) -> read_metrics(Ref).

%%%===================================================================
%%% Internal — Mesh Pool Lookup
%%%===================================================================

-spec get_mesh_pool() -> pid() | undefined.
get_mesh_pool() ->
    pool_or_undef(persistent_term:get(macula_dist_mesh_pool, undefined)).

pool_or_undef(undefined) -> undefined;
pool_or_undef(Pid) when is_pid(Pid) ->
    pool_alive(is_process_alive(Pid), Pid).

pool_alive(true,  Pid) -> Pid;
pool_alive(false, _Pid) -> undefined.

%%%===================================================================
%%% Internal — Tunnel Negotiation (connecting side)
%%%===================================================================

request_tunnel(Pool, NodeStr) ->
    Procedure = <<"_dist.tunnel.", (list_to_binary(NodeStr))/binary>>,
    Args = #{<<"from_node">> => atom_to_binary(node()),
             <<"target_node">> => list_to_binary(NodeStr)},
    ?LOG_INFO("[dist_relay] RPC ~s via ~p", [Procedure, Pool]),
    %% V2 pool: first-success across healthy links. The pool itself
    %% does the multi-station fan-out the V1 multi_relay used to do.
    Result = macula_client:call(Pool, ?DIST_REALM, Procedure, Args,
                                ?DIST_TIMEOUT),
    on_tunnel_rpc_reply(Result, Pool).

on_tunnel_rpc_reply({ok, #{<<"tunnel_id">> := TunnelId}}, Pool) ->
    ?LOG_INFO("[dist_relay] Tunnel established: ~s", [TunnelId]),
    create_dist_socket(Pool, TunnelId);
on_tunnel_rpc_reply({ok, #{<<"error">> := ErrorInfo}}, _Pool) ->
    ?LOG_WARNING("[dist_relay] Tunnel error: ~p", [ErrorInfo]),
    {error, {tunnel_error, ErrorInfo}};
on_tunnel_rpc_reply({error, Reason}, _Pool) ->
    ?LOG_WARNING("[dist_relay] Tunnel request failed: ~p", [Reason]),
    {error, {tunnel_failed, Reason}};
on_tunnel_rpc_reply(Other, _Pool) ->
    ?LOG_WARNING("[dist_relay] Unexpected RPC result: ~p", [Other]),
    {error, {unexpected_result, Other}}.

%%%===================================================================
%%% Internal — Tunnel Negotiation (accepting side)
%%%===================================================================

handle_tunnel_request(Args) ->
    FromNode = maps:get(<<"from_node">>, Args, <<>>),
    TunnelId = base64:encode(crypto:strong_rand_bytes(16)),
    ?LOG_INFO("[dist_relay] Tunnel request from ~s, id: ~s", [FromNode, TunnelId]),

    SendTopic = <<"_dist.data.", TunnelId/binary, ".in">>,
    RecvTopic = <<"_dist.data.", TunnelId/binary, ".out">>,

    on_tunnel_request_pool(get_mesh_pool(), TunnelId, SendTopic, RecvTopic).

on_tunnel_request_pool(undefined, _TunnelId, _SendTopic, _RecvTopic) ->
    {error, <<"no_mesh_pool">>};
on_tunnel_request_pool(Pool, TunnelId, SendTopic, RecvTopic) ->
    spawn_accept_bridge(Pool, TunnelId, SendTopic, RecvTopic),
    {ok, #{<<"tunnel_id">> => TunnelId,
           <<"send_topic">> => SendTopic,
           <<"recv_topic">> => RecvTopic}}.

%% The accept side has a setup phase (kernel negotiation) before the
%% supervised bridge can start. This setup process is short-lived —
%% it creates the loopback pair, negotiates with net_kernel, then
%% hands off to the supervised bridge.
spawn_accept_bridge(Pool, TunnelId, SendTopic, RecvTopic) ->
    {Pid, _Ref} = spawn_monitor(fun() ->
        dist_accept_setup(Pool, TunnelId, SendTopic, RecvTopic)
    end),
    ?LOG_INFO("[dist_relay] Accept setup ~p for ~s", [Pid, TunnelId]),
    Pid.

%%%===================================================================
%%% Internal — Accept-side Setup (short-lived, then hands to bridge)
%%%===================================================================

dist_accept_setup(Pool, TunnelId, SendTopic, RecvTopic) ->
    Self = self(),
    Key = tunnel_key(),

    %% Subscribe for buffering before kernel knows about us. The V2
    %% pool delivers `{macula_event, SubRef, Topic, Payload, Meta}'
    %% to Self directly; no callback indirection.
    {ok, BufferSubRef} = macula_pubsub:subscribe(Pool, ?DIST_REALM,
                                                  RecvTopic, Self),

    {DistSock, BridgeSock} = create_loopback_pair(),

    on_kernel_lookup(whereis(net_kernel), Self, Pool, DistSock, BridgeSock,
                     BufferSubRef, SendTopic, RecvTopic, TunnelId, Key).

on_kernel_lookup(undefined, _Self, Pool, _DistSock, _BridgeSock, BufferSubRef,
                 _SendTopic, _RecvTopic, _TunnelId, _Key) ->
    ?LOG_WARNING("[dist_bridge] net_kernel not found"),
    macula_pubsub:unsubscribe(Pool, BufferSubRef);
on_kernel_lookup(KernelPid, Self, Pool, DistSock, BridgeSock, BufferSubRef,
                 SendTopic, RecvTopic, TunnelId, Key) ->
    negotiate_with_kernel(Self, KernelPid, Pool, DistSock, BridgeSock,
                          BufferSubRef, SendTopic, RecvTopic, TunnelId, Key).

negotiate_with_kernel(Self, KernelPid, Pool, DistSock, BridgeSock,
                      BufferSubRef, SendTopic, RecvTopic, TunnelId, Key) ->
    KernelPid ! {accept, Self, DistSock, inet, macula_dist},
    receive
        {KernelPid, controller, DistCtrl} ->
            DistCtrl ! {Self, controller, ok},
            %% Transfer DistSock to the dist controller so it survives
            %% when this setup process exits.
            gen_tcp:controlling_process(DistSock, DistCtrl),
            macula_pubsub:unsubscribe(Pool, BufferSubRef),
            flush_buffered_to_socket(BridgeSock, Key, BufferSubRef),
            start_supervised_bridge(Pool, BridgeSock, SendTopic, RecvTopic,
                                     TunnelId, Key);
        {KernelPid, unsupported_protocol} ->
            ?LOG_WARNING("[dist_bridge] Unsupported protocol"),
            macula_pubsub:unsubscribe(Pool, BufferSubRef)
    after ?CONTROLLER_TIMEOUT ->
        ?LOG_WARNING("[dist_bridge] Controller timeout"),
        macula_pubsub:unsubscribe(Pool, BufferSubRef)
    end.

%%%===================================================================
%%% Internal — Connect-side Bridge Creation
%%%===================================================================

create_dist_socket(Pool, TunnelId) ->
    SendTopic = <<"_dist.data.", TunnelId/binary, ".out">>,
    RecvTopic = <<"_dist.data.", TunnelId/binary, ".in">>,

    {DistSock, BridgeSock} = create_loopback_pair(),

    Key = tunnel_key(),
    start_supervised_bridge(Pool, BridgeSock, SendTopic, RecvTopic,
                             TunnelId, Key),

    {ok, DistSock, DistSock}.

%%%===================================================================
%%% Internal — Supervised Bridge Startup
%%%===================================================================

start_supervised_bridge(Pool, BridgeSock, SendTopic, RecvTopic, TunnelId, Key) ->
    Metrics = init_metrics(TunnelId),
    BridgeArgs = #{
        pool => Pool,
        bridge_sock => BridgeSock,
        send_topic => SendTopic,
        recv_topic => RecvTopic,
        tunnel_id => TunnelId,
        key => Key,
        metrics => Metrics
    },
    case macula_dist_bridge_sup:start_bridge(BridgeArgs) of
        {ok, Pid} ->
            gen_tcp:controlling_process(BridgeSock, Pid),
            Pid ! socket_ready,
            ?LOG_INFO("[dist_relay] Supervised bridge ~p for ~s", [Pid, TunnelId]),
            {ok, Pid};
        {error, Reason} ->
            ?LOG_ERROR("[dist_relay] Failed to start bridge for ~s: ~p",
                        [TunnelId, Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% Internal — Encryption
%%%===================================================================

tunnel_key() ->
    Cookie = atom_to_binary(erlang:get_cookie()),
    crypto:hash(sha256, <<"macula-dist-tunnel:", Cookie/binary>>).

%%%===================================================================
%%% Internal — Metrics
%%%===================================================================

init_metrics(TunnelId) ->
    Ref = counters:new(4, [write_concurrency]),
    Tunnels = persistent_term:get(macula_dist_tunnels, #{}),
    persistent_term:put(macula_dist_tunnels, Tunnels#{TunnelId => Ref}),
    Ref.

read_metrics(Ref) ->
    #{bytes_out => counters:get(Ref, ?METRIC_BYTES_OUT),
      bytes_in  => counters:get(Ref, ?METRIC_BYTES_IN),
      msgs_out  => counters:get(Ref, ?METRIC_MSGS_OUT),
      msgs_in   => counters:get(Ref, ?METRIC_MSGS_IN)}.

%%%===================================================================
%%% Internal — Loopback Pair + Helpers
%%%===================================================================

create_loopback_pair() ->
    ListenOpts = [binary, {active, false}, {reuseaddr, true}, {ip, {127,0,0,1}}],
    {ok, LSock} = gen_tcp:listen(0, ListenOpts),
    {ok, Port} = inet:port(LSock),
    {ok, CSock} = gen_tcp:connect({127,0,0,1}, Port,
                                  [binary, {active, false}, {packet, 2}, {nodelay, true}]),
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    inet:setopts(ASock, [{packet, raw}, {nodelay, true}]),
    {CSock, ASock}.

%% Drain the buffered events that arrived while net_kernel was still
%% setting up the dist controller. The V2 pool delivers a tagged
%% 5-tuple — match on the SubRef we used to subscribe so we don't
%% accidentally consume unrelated events (the same process may host
%% other subscriptions during setup). `macula_event_gone' is the
%% terminal signal sent once the subscription tears down.
flush_buffered_to_socket(BridgeSock, Key, SubRef) ->
    receive
        {macula_event, SubRef, _Topic, EncData, _Meta} ->
            ship_buffered(BridgeSock, decrypt(Key, EncData), EncData),
            flush_buffered_to_socket(BridgeSock, Key, SubRef);
        {macula_event_gone, SubRef, _Reason} ->
            ok
    after 0 ->
        ok
    end.

ship_buffered(BridgeSock, {ok, Data}, _EncData) ->
    gen_tcp:send(BridgeSock, Data);
ship_buffered(BridgeSock, {error, _}, EncData) ->
    gen_tcp:send(BridgeSock, EncData).

decrypt(Key, <<Nonce:12/binary, Tag:16/binary, Ciphertext/binary>>) ->
    aead_decrypt(crypto:crypto_one_time_aead(
                   aes_256_gcm, Key, Nonce, Ciphertext, <<>>, Tag, false));
decrypt(_Key, _Data) ->
    {error, decrypt_failed}.

aead_decrypt(error)     -> {error, decrypt_failed};
aead_decrypt(Plaintext) -> {ok, Plaintext}.
