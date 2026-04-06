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
-export([is_relay_mode/0, get_mesh_client/0]).
-export([register_mesh_client/1]).
-export([advertise_dist_accept/0]).
-export([extract_payload/1]).
-export([get_tunnel_metrics/0, get_tunnel_metrics/1]).

-define(DIST_TIMEOUT, 25000).
-define(CONTROLLER_TIMEOUT, 30000).

-define(METRIC_BYTES_OUT, 1).
-define(METRIC_BYTES_IN, 2).
-define(METRIC_MSGS_OUT, 3).
-define(METRIC_MSGS_IN, 4).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Register a mesh relay client for distribution tunneling.
-spec register_mesh_client(pid()) -> ok.
register_mesh_client(Pid) when is_pid(Pid) ->
    persistent_term:put(macula_dist_mesh_client, Pid),
    ?LOG_INFO("[dist_relay] Mesh client registered: ~p", [Pid]),
    ok.

%% @doc Check if relay distribution mode is enabled.
-spec is_relay_mode() -> boolean().
is_relay_mode() ->
    os:getenv("MACULA_DIST_MODE") =:= "relay".

%% @doc Connect to a remote node via relay mesh.
-spec connect(string(), string(), integer()) -> {ok, port(), port()} | {error, term()}.
connect(NodeStr, _Host, _Port) ->
    ?LOG_INFO("[dist_relay] Connecting to ~s via mesh", [NodeStr]),
    case get_mesh_client() of
        undefined ->
            {error, no_mesh_connection};
        MeshClient ->
            request_tunnel(MeshClient, NodeStr)
    end.

%% @doc Advertise this node as accepting distribution connections via relay.
-spec advertise_dist_accept() -> ok.
advertise_dist_accept() ->
    case get_mesh_client() of
        undefined ->
            ?LOG_WARNING("[dist_relay] Cannot advertise — no mesh client registered"),
            ok;
        Client ->
            NodeName = atom_to_binary(node()),
            Procedure = <<"_dist.tunnel.", NodeName/binary>>,
            Handler = fun(Args) -> handle_tunnel_request(Args) end,
            macula_relay_client:advertise(Client, Procedure, Handler),
            ?LOG_INFO("[dist_relay] Advertised distribution accept: ~s", [Procedure]),
            ok
    end.

%% @doc Get metrics for all active tunnels.
-spec get_tunnel_metrics() -> [{binary(), map()}].
get_tunnel_metrics() ->
    case persistent_term:get(macula_dist_tunnels, undefined) of
        undefined -> [];
        Tunnels ->
            maps:fold(fun(TunnelId, Ref, Acc) ->
                [{TunnelId, read_metrics(Ref)} | Acc]
            end, [], Tunnels)
    end.

%% @doc Get metrics for a specific tunnel.
-spec get_tunnel_metrics(binary()) -> map() | undefined.
get_tunnel_metrics(TunnelId) ->
    case persistent_term:get(macula_dist_tunnels, undefined) of
        undefined -> undefined;
        Tunnels ->
            case maps:get(TunnelId, Tunnels, undefined) of
                undefined -> undefined;
                Ref -> read_metrics(Ref)
            end
    end.

%%%===================================================================
%%% Internal — Mesh Client Lookup
%%%===================================================================

-spec get_mesh_client() -> pid() | undefined.
get_mesh_client() ->
    case persistent_term:get(macula_dist_mesh_client, undefined) of
        undefined -> undefined;
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> Pid;
                false -> undefined
            end
    end.

%%%===================================================================
%%% Internal — Tunnel Negotiation (connecting side)
%%%===================================================================

request_tunnel(MeshClient, NodeStr) ->
    Procedure = <<"_dist.tunnel.", (list_to_binary(NodeStr))/binary>>,
    Args = #{<<"from_node">> => atom_to_binary(node()),
             <<"target_node">> => list_to_binary(NodeStr)},
    %% Gap 7: try all connected relays when using multi_relay.
    %% If the target is on a different relay, call_any tries each in sequence.
    Result = tunnel_rpc(MeshClient, Procedure, Args),
    case Result of
        {ok, #{<<"tunnel_id">> := TunnelId}} ->
            ?LOG_INFO("[dist_relay] Tunnel established: ~s", [TunnelId]),
            create_dist_socket(MeshClient, TunnelId);
        {ok, #{<<"error">> := ErrorInfo}} ->
            ?LOG_WARNING("[dist_relay] Tunnel error: ~p", [ErrorInfo]),
            {error, {tunnel_error, ErrorInfo}};
        {error, Reason} ->
            ?LOG_WARNING("[dist_relay] Tunnel request failed: ~p", [Reason]),
            {error, {tunnel_failed, Reason}}
    end.

%% Use call_any for multi_relay (tries each connected relay).
%% Fall back to direct call for single relay_client.
tunnel_rpc(MeshClient, Procedure, Args) ->
    case is_multi_relay(MeshClient) of
        true ->
            macula_multi_relay:call_any(MeshClient, Procedure, Args, ?DIST_TIMEOUT);
        false ->
            macula_relay_client:call(MeshClient, Procedure, Args, ?DIST_TIMEOUT)
    end.

%% Check if the PID is a macula_multi_relay gen_server by inspecting
%% the registered name or the initial call in process_info.
is_multi_relay(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dict} ->
            case proplists:get_value('$initial_call', Dict) of
                {macula_multi_relay, init, 1} -> true;
                _ -> false
            end;
        _ ->
            false
    end.

%%%===================================================================
%%% Internal — Tunnel Negotiation (accepting side)
%%%===================================================================

handle_tunnel_request(Args) ->
    FromNode = maps:get(<<"from_node">>, Args, <<>>),
    TunnelId = base64:encode(crypto:strong_rand_bytes(16)),
    ?LOG_INFO("[dist_relay] Tunnel request from ~s, id: ~s", [FromNode, TunnelId]),

    SendTopic = <<"_dist.data.", TunnelId/binary, ".in">>,
    RecvTopic = <<"_dist.data.", TunnelId/binary, ".out">>,

    case get_mesh_client() of
        undefined ->
            {error, <<"no_mesh_client">>};
        Client ->
            spawn_accept_bridge(Client, TunnelId, SendTopic, RecvTopic),
            {ok, #{<<"tunnel_id">> => TunnelId,
                   <<"send_topic">> => SendTopic,
                   <<"recv_topic">> => RecvTopic}}
    end.

%% The accept side has a setup phase (kernel negotiation) before the
%% supervised bridge can start. This setup process is short-lived —
%% it creates the loopback pair, negotiates with net_kernel, then
%% hands off to the supervised bridge.
spawn_accept_bridge(Client, TunnelId, SendTopic, RecvTopic) ->
    {Pid, _Ref} = spawn_monitor(fun() ->
        dist_accept_setup(Client, TunnelId, SendTopic, RecvTopic)
    end),
    ?LOG_INFO("[dist_relay] Accept setup ~p for ~s", [Pid, TunnelId]),
    Pid.

%%%===================================================================
%%% Internal — Accept-side Setup (short-lived, then hands to bridge)
%%%===================================================================

dist_accept_setup(Client, TunnelId, SendTopic, RecvTopic) ->
    Self = self(),
    Key = tunnel_key(),

    %% Subscribe for buffering before kernel knows about us
    {ok, BufferSubRef} = macula_relay_client:subscribe(Client, RecvTopic,
        fun(Msg) -> Self ! {dist_data_buffered, extract_payload(Msg)} end),

    {DistSock, BridgeSock} = create_loopback_pair(),

    case whereis(net_kernel) of
        undefined ->
            ?LOG_WARNING("[dist_bridge] net_kernel not found"),
            macula_relay_client:unsubscribe(Client, BufferSubRef);
        KernelPid ->
            negotiate_with_kernel(Self, KernelPid, Client, DistSock, BridgeSock,
                                  BufferSubRef, SendTopic, RecvTopic, TunnelId, Key)
    end.

negotiate_with_kernel(Self, KernelPid, Client, DistSock, BridgeSock,
                      BufferSubRef, SendTopic, RecvTopic, TunnelId, Key) ->
    KernelPid ! {accept, Self, DistSock, inet, macula_dist},
    receive
        {KernelPid, controller, DistCtrl} ->
            DistCtrl ! {Self, controller, ok},
            macula_relay_client:unsubscribe(Client, BufferSubRef),
            flush_buffered_to_socket(BridgeSock, Key),
            %% Hand off to supervised bridge
            start_supervised_bridge(Client, BridgeSock, SendTopic, RecvTopic,
                                     TunnelId, Key);
        {KernelPid, unsupported_protocol} ->
            ?LOG_WARNING("[dist_bridge] Unsupported protocol"),
            macula_relay_client:unsubscribe(Client, BufferSubRef)
    after ?CONTROLLER_TIMEOUT ->
        ?LOG_WARNING("[dist_bridge] Controller timeout"),
        macula_relay_client:unsubscribe(Client, BufferSubRef)
    end.

%%%===================================================================
%%% Internal — Connect-side Bridge Creation
%%%===================================================================

create_dist_socket(MeshClient, TunnelId) ->
    SendTopic = <<"_dist.data.", TunnelId/binary, ".out">>,
    RecvTopic = <<"_dist.data.", TunnelId/binary, ".in">>,

    {DistSock, BridgeSock} = create_loopback_pair(),

    Key = tunnel_key(),
    start_supervised_bridge(MeshClient, BridgeSock, SendTopic, RecvTopic,
                             TunnelId, Key),

    {ok, DistSock, DistSock}.

%%%===================================================================
%%% Internal — Supervised Bridge Startup
%%%===================================================================

start_supervised_bridge(Client, BridgeSock, SendTopic, RecvTopic, TunnelId, Key) ->
    Metrics = init_metrics(TunnelId),
    BridgeArgs = #{
        client => Client,
        bridge_sock => BridgeSock,
        send_topic => SendTopic,
        recv_topic => RecvTopic,
        tunnel_id => TunnelId,
        key => Key,
        metrics => Metrics
    },
    case macula_dist_bridge_sup:start_bridge(BridgeArgs) of
        {ok, Pid} ->
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

flush_buffered_to_socket(BridgeSock, Key) ->
    receive
        {dist_data_buffered, EncData} ->
            case decrypt(Key, EncData) of
                {ok, Data} -> gen_tcp:send(BridgeSock, Data);
                {error, _} -> gen_tcp:send(BridgeSock, EncData)
            end,
            flush_buffered_to_socket(BridgeSock, Key)
    after 0 ->
        ok
    end.

decrypt(Key, <<Nonce:12/binary, Tag:16/binary, Ciphertext/binary>>) ->
    case crypto:crypto_one_time_aead(
            aes_256_gcm, Key, Nonce, Ciphertext, <<>>, Tag, false) of
        error -> {error, decrypt_failed};
        Plaintext -> {ok, Plaintext}
    end;
decrypt(_Key, _Data) ->
    {error, decrypt_failed}.

extract_payload(#{payload := P}) -> P;
extract_payload(P) when is_binary(P) -> P.
