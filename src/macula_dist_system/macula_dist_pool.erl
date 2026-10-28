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
%%% (simple_one_for_one, started by `macula_root').
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
-module(macula_dist_pool).

-include_lib("kernel/include/logger.hrl").

-export([connect/3]).
-export([is_relay_mode/0, get_mesh_pool/0]).
-export([register_mesh_pool/1]).
-export([advertise_dist_accept/0]).
-export([get_tunnel_metrics/0, get_tunnel_metrics/1]).

-ifdef(TEST).
%% Readers of decoded tunnel RPC payloads, the two ends of the peer declaration,
%% and the tunnel key, exported for unit tests.
-export([tunnel_reply/1, tunnel_request_from_node/1, peer_source/1,
         handle_tunnel_request/1, on_tunnel_rpc_reply/2, tunnel_key/0]).
-endif.

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
    ?LOG_INFO("[dist_pool] Mesh pool registered: ~p", [Pid]),
    ok.

%% @doc Check if relay distribution mode is enabled.
-spec is_relay_mode() -> boolean().
is_relay_mode() ->
    os:getenv("MACULA_DIST_MODE") =:= "relay".

%% @doc Connect to a remote node via relay mesh.
-spec connect(string(), string(), integer()) -> {ok, port(), port()} | {error, term()}.
connect(NodeStr, _Host, _Port) ->
    ?LOG_INFO("[dist_pool] Connecting to ~s via mesh", [NodeStr]),
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
            ?LOG_WARNING("[dist_pool] Cannot advertise — no mesh pool registered"),
            ok;
        Pool ->
            NodeName = atom_to_binary(node()),
            Procedure = <<"_dist.tunnel.", NodeName/binary>>,
            %% `open' is what this procedure had by the old default; naming it
            %% keeps the bridge's behaviour unchanged (macula#147 removed the
            %% default). Tightening it is a decision about distribution, not a
            %% side effect of declaring call classes. It is not what names the
            %% tunnel's peer: whoever calls this gets a tunnel whose topics only
            %% they may publish on, named from the CALL's `caller' (macula#156),
            %% and the bytes that tunnel carries are salted with the
            %% distribution cookie (`tunnel_key/0'), so a caller gets a tunnel
            %% into its own handshake and no further.
            macula_client:advertise(Pool, ?DIST_REALM, Procedure,
                                    fun handle_tunnel_request/1, open),
            ?LOG_INFO("[dist_pool] Advertised distribution accept: ~s", [Procedure]),
            ok
    end.

%% Bridge supervisor is started by macula_root (application supervisor),
%% so it survives shell crashes and user code failures. This is a no-op
%% kept for backwards compatibility.
ensure_bridge_sup() ->
    case whereis(macula_dist_bridge_sup) of
        undefined ->
            ?LOG_WARNING("[dist_pool] Bridge sup not running — macula app may not be started");
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
    ?LOG_INFO("[dist_pool] RPC ~s via ~p", [Procedure, Pool]),
    %% The tunnel procedure's provider is resolved through its
    %% advertisement and called at the station that advertisement names.
    %% The seal report names the provider the call was addressed to, which
    %% is the peer of the tunnel it opens (macula#156): see `peer_source/1'.
    Result = macula:call(Pool, ?DIST_REALM, Procedure, Args, ?DIST_TIMEOUT,
                         #{report => true}),
    on_tunnel_rpc_reply(Result, Pool).

on_tunnel_rpc_reply({ok, Reply, #{provider := Provider}}, Pool) when is_map(Reply) ->
    on_tunnel_reply(tunnel_reply(Reply), Reply, Provider, Pool);
on_tunnel_rpc_reply({error, Reason}, _Pool) ->
    ?LOG_WARNING("[dist_pool] Tunnel request failed: ~p", [Reason]),
    {error, {tunnel_failed, Reason}};
on_tunnel_rpc_reply(Other, _Pool) ->
    ?LOG_WARNING("[dist_pool] Unexpected RPC result: ~p", [Other]),
    {error, {unexpected_result, Other}}.

on_tunnel_reply({tunnel, TunnelId}, _Reply, Provider, Pool) ->
    ?LOG_INFO("[dist_pool] Tunnel established with ~s: ~s",
              [binary:encode_hex(Provider, lowercase), TunnelId]),
    create_dist_socket(Pool, TunnelId, Provider);
on_tunnel_reply({tunnel_error, ErrorInfo}, _Reply, _Provider, _Pool) ->
    ?LOG_WARNING("[dist_pool] Tunnel error: ~p", [ErrorInfo]),
    {error, {tunnel_error, ErrorInfo}};
on_tunnel_reply(unexpected, Reply, _Provider, _Pool) ->
    ?LOG_WARNING("[dist_pool] Unexpected RPC result: ~p", [{ok, Reply}]),
    {error, {unexpected_result, {ok, Reply}}}.

%% The tunnel RPC's reply payload, read through the D26 facade accessors in
%% whatever form it arrives.
tunnel_reply(Reply) ->
    tunnel_reply_fields(text_value(macula:field(tunnel_id, Reply)),
                        text_value(macula:field(error, Reply))).

tunnel_reply_fields(TunnelId, _ErrorInfo) when is_binary(TunnelId) ->
    {tunnel, TunnelId};
tunnel_reply_fields(undefined, ErrorInfo) when ErrorInfo =/= undefined ->
    {tunnel_error, ErrorInfo};
tunnel_reply_fields(_TunnelId, _ErrorInfo) ->
    unexpected.

%% A text value arrives as {text, Bin} and is read as the binary; any other
%% value is kept as it is.
text_value({text, Bin}) when is_binary(Bin) -> Bin;
text_value(Value) -> Value.

%%%===================================================================
%%% Internal — Tunnel Negotiation (accepting side)
%%%===================================================================

%% The requesting node named in the tunnel RPC's arguments, read through the
%% D26 facade accessors; empty when absent.
tunnel_request_from_node(Args) ->
    text_value(macula:field(from_node, Args, <<>>)).

%% A CALL's payload need not be a map (`with_caller/2' can only merge the caller
%% into one), and this procedure is advertised `open', so a payload that is not
%% one is refused by name here rather than crashing a reader on every probe
%% (macula#159).
handle_tunnel_request(Args) when not is_map(Args) ->
    ?LOG_WARNING("[dist_pool] Tunnel request whose payload is not a map"),
    {error, <<"bad_args">>};
handle_tunnel_request(Args) ->
    FromNode = tunnel_request_from_node(Args),
    TunnelId = base64:encode(crypto:strong_rand_bytes(16)),
    ?LOG_INFO("[dist_pool] Tunnel request from ~s, id: ~s", [FromNode, TunnelId]),

    SendTopic = <<"_dist.data.", TunnelId/binary, ".in">>,
    RecvTopic = <<"_dist.data.", TunnelId/binary, ".out">>,

    on_tunnel_request_pool(get_mesh_pool(), caller_id(Args), TunnelId,
                           SendTopic, RecvTopic).

%% The calling node's wire-authenticated node_id, merged into the handler's
%% payload by the link (`with_caller/2'), and `undefined' when there is none.
caller_id(Args) ->
    maps:get(caller, Args, undefined).

on_tunnel_request_pool(undefined, _Caller, _TunnelId, _SendTopic, _RecvTopic) ->
    {error, <<"no_mesh_pool">>};
on_tunnel_request_pool(_Pool, undefined, _TunnelId, _SendTopic, _RecvTopic) ->
    %% A CALL's `caller' is a required, wire-authenticated field, so args
    %% without one are not a tunnel request this node can name a peer for. It is
    %% refused rather than opened to any publisher (macula#156).
    ?LOG_WARNING("[dist_pool] Tunnel request with no wire-authenticated caller"),
    {error, <<"no_caller">>};
on_tunnel_request_pool(Pool, Caller, TunnelId, SendTopic, RecvTopic) ->
    spawn_accept_bridge(Pool, Caller, TunnelId, SendTopic, RecvTopic),
    {ok, #{<<"tunnel_id">> => TunnelId,
           <<"send_topic">> => SendTopic,
           <<"recv_topic">> => RecvTopic}}.

%% The one publisher a tunnel's topics accept (macula#156). Both ends hold the
%% other's node_id, and both hold it from the wire rather than from the request
%% body: the accept side from the CALL's `caller', the connect side from the
%% seal report's `provider', which `macula_frame:verify_reply/3' admits only
%% when the reply is signed by the key of the node it claims to be and that node
%% is the call's target. So the node that requested the tunnel is the node that
%% publishes on it, and anything else that publishes to those topics is refused
%% before the socket.
peer_source(Peer) when is_binary(Peer), byte_size(Peer) =:= 32 ->
    #{sources => {nodes, [Peer]}}.

%% The accept side has a setup phase (kernel negotiation) before the
%% supervised bridge can start. This setup process is short-lived —
%% it creates the loopback pair, negotiates with net_kernel, then
%% hands off to the supervised bridge.
spawn_accept_bridge(Pool, Caller, TunnelId, SendTopic, RecvTopic) ->
    {Pid, _Ref} = spawn_monitor(fun() ->
        dist_accept_setup(Pool, Caller, TunnelId, SendTopic, RecvTopic)
    end),
    ?LOG_INFO("[dist_pool] Accept setup ~p for ~s", [Pid, TunnelId]),
    Pid.

%%%===================================================================
%%% Internal — Accept-side Setup (short-lived, then hands to bridge)
%%%===================================================================

dist_accept_setup(Pool, Caller, TunnelId, SendTopic, RecvTopic) ->
    Self = self(),
    Key = tunnel_key(),
    Source = peer_source(Caller),
    {DistSock, BridgeSock} = create_loopback_pair(),

    %% The bridge starts before the kernel is asked for a controller (macula#157).
    %% It owns the tunnel's only subscription from that moment, so a fact that
    %% arrives while the handshake is still in flight waits where it will be read
    %% -- the bridge's mailbox, then the loopback socket -- instead of falling
    %% into the gap between a buffer subscription's unsubscribe and the bridge's
    %% subscribe. One subscription also means one copy: the pool fans a
    %% publication out to every matching subscription, so two would double it.
    on_bridge_started(start_supervised_bridge(Pool, BridgeSock, SendTopic, RecvTopic,
                                              TunnelId, Key, Source),
                      Self, DistSock, TunnelId).

on_bridge_started({ok, Bridge}, Self, DistSock, TunnelId) ->
    on_kernel_lookup(whereis(net_kernel), Self, Bridge, DistSock, TunnelId);
on_bridge_started({error, Reason}, _Self, DistSock, TunnelId) ->
    %% No bridge means no reader on the socket, so the handshake is not offered
    %% to the kernel at all: it fails here, with the pair given up in one place,
    %% rather than half-owned and half-read (macula#158).
    _ = gen_tcp:close(DistSock),
    ?LOG_ERROR("[dist_bridge] Accept bridge did not start for ~s: ~p", [TunnelId, Reason]).

on_kernel_lookup(undefined, _Self, Bridge, DistSock, TunnelId) ->
    ?LOG_WARNING("[dist_bridge] net_kernel not found"),
    abandon_accept(Bridge, DistSock, TunnelId);
on_kernel_lookup(KernelPid, Self, Bridge, DistSock, TunnelId) ->
    negotiate_with_kernel(Self, KernelPid, Bridge, DistSock, TunnelId).

negotiate_with_kernel(Self, KernelPid, Bridge, DistSock, TunnelId) ->
    KernelPid ! {accept, Self, DistSock, inet, macula_dist},
    receive
        {KernelPid, controller, DistCtrl} ->
            DistCtrl ! {Self, controller, ok, <<>>},
            %% Transfer DistSock to the dist controller so it survives
            %% when this setup process exits. The bridge keeps the other end and
            %% keeps reading it; nothing is handed over between subscriptions.
            gen_tcp:controlling_process(DistSock, DistCtrl);
        {KernelPid, unsupported_protocol} ->
            ?LOG_WARNING("[dist_bridge] Unsupported protocol"),
            abandon_accept(Bridge, DistSock, TunnelId)
    after ?CONTROLLER_TIMEOUT ->
        ?LOG_WARNING("[dist_bridge] Controller timeout"),
        abandon_accept(Bridge, DistSock, TunnelId)
    end.

%% The kernel never took the socket: the bridge goes with it, so no subscription
%% and no socket outlive the handshake that did not complete.
abandon_accept(Bridge, DistSock, TunnelId) ->
    _ = gen_tcp:close(DistSock),
    _ = catch gen_server:stop(Bridge, normal, 5000),
    ?LOG_WARNING("[dist_bridge] Accept abandoned for ~s", [TunnelId]).

%%%===================================================================
%%% Internal — Connect-side Bridge Creation
%%%===================================================================

create_dist_socket(Pool, TunnelId, Provider) ->
    SendTopic = <<"_dist.data.", TunnelId/binary, ".out">>,
    RecvTopic = <<"_dist.data.", TunnelId/binary, ".in">>,

    {DistSock, BridgeSock} = create_loopback_pair(),

    Key = tunnel_key(),
    started_bridge(start_supervised_bridge(Pool, BridgeSock, SendTopic, RecvTopic,
                                           TunnelId, Key, peer_source(Provider)),
                   DistSock, BridgeSock).

%% A tunnel whose bridge did not start has no reader on its socket, so handing
%% the socket to the distribution setup only buys a handshake that writes into
%% nothing and a `pang' at the end of it with no cause attached. The bridge's own
%% reason goes back instead, and both ends of the loopback pair are given up
%% here rather than left open (macula#158).
started_bridge({ok, _Bridge}, DistSock, _BridgeSock) ->
    {ok, DistSock, DistSock};
started_bridge({error, Reason}, DistSock, BridgeSock) ->
    _ = gen_tcp:close(DistSock),
    _ = gen_tcp:close(BridgeSock),
    ?LOG_ERROR("[dist_pool] Tunnel bridge did not start: ~p", [Reason]),
    {error, {bridge, Reason}}.

%%%===================================================================
%%% Internal — Supervised Bridge Startup
%%%===================================================================

start_supervised_bridge(Pool, BridgeSock, SendTopic, RecvTopic, TunnelId, Key, Source) ->
    Metrics = init_metrics(TunnelId),
    BridgeArgs = #{
        pool => Pool,
        bridge_sock => BridgeSock,
        send_topic => SendTopic,
        recv_topic => RecvTopic,
        tunnel_id => TunnelId,
        key => Key,
        source => Source,
        metrics => Metrics
    },
    case macula_dist_bridge_sup:start_bridge(BridgeArgs) of
        {ok, Pid} ->
            gen_tcp:controlling_process(BridgeSock, Pid),
            Pid ! socket_ready,
            ?LOG_INFO("[dist_pool] Supervised bridge ~p for ~s", [Pid, TunnelId]),
            {ok, Pid};
        {error, Reason} ->
            ?LOG_ERROR("[dist_pool] Failed to start bridge for ~s: ~p",
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
