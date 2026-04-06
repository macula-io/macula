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
%%% This is EXPERIMENTAL. Supports Pid ! Msg, gen_server:call,
%%% pg groups, process monitoring. Does NOT guarantee Mnesia or
%%% global module compatibility over WAN latency.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay).

-include_lib("kernel/include/logger.hrl").

-export([connect/3, accept_dist/2]).
-export([is_relay_mode/0, get_mesh_client/0]).
-export([register_mesh_client/1]).
-export([advertise_dist_accept/0]).

-define(DIST_TIMEOUT, 25000).
-define(BRIDGE_RECV_TIMEOUT, 60000).
-define(CONTROLLER_TIMEOUT, 30000).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Register a mesh relay client for distribution tunneling.
%% Called by the application that owns the relay client (e.g., hecate_mesh).
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
%% Returns {ok, DistSock, DistSock} where DistSock is a gen_tcp loopback
%% socket bridged to a relay tunnel.
-spec connect(string(), string(), integer()) -> {ok, port(), port()} | {error, term()}.
connect(NodeStr, _Host, _Port) ->
    ?LOG_INFO("[dist_relay] Connecting to ~s via mesh", [NodeStr]),
    case get_mesh_client() of
        undefined ->
            {error, no_mesh_connection};
        MeshClient ->
            request_tunnel(MeshClient, NodeStr)
    end.

%% @doc Accept an incoming distribution connection via relay mesh.
-spec accept_dist(binary(), map()) -> {ok, binary()}.
accept_dist(TunnelId, _Opts) ->
    ?LOG_INFO("[dist_relay] Accepting dist tunnel: ~s", [TunnelId]),
    {ok, TunnelId}.

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

%%%===================================================================
%%% Internal — Mesh Client Lookup
%%%===================================================================

%% @doc Find the mesh relay client from persistent_term.
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
    case macula_relay_client:call(MeshClient, Procedure, Args, ?DIST_TIMEOUT) of
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
            spawn_monitor_bridge(Client, TunnelId, SendTopic, RecvTopic, FromNode),
            {ok, #{<<"tunnel_id">> => TunnelId,
                   <<"send_topic">> => SendTopic,
                   <<"recv_topic">> => RecvTopic}}
    end.

spawn_monitor_bridge(Client, TunnelId, SendTopic, RecvTopic, FromNode) ->
    {Pid, _Ref} = spawn_monitor(fun() ->
        dist_accept_bridge(Client, TunnelId, SendTopic, RecvTopic, FromNode)
    end),
    ?LOG_INFO("[dist_relay] Tunnel bridge ~p for ~s", [Pid, TunnelId]),
    Pid.

%%%===================================================================
%%% Internal — Accept-side Bridge
%%%===================================================================

%% Creates a gen_tcp loopback pair so OTP's dist_util gets a real fd.
%% One end goes to dist_util, the other is bridged to the relay tunnel.
dist_accept_bridge(Client, TunnelId, SendTopic, RecvTopic, _FromNode) ->
    Self = self(),

    %% Subscribe BEFORE notifying kernel — buffer data until bridge socket ready.
    {ok, BufferSubRef} = macula_relay_client:subscribe(Client, RecvTopic,
        fun(Msg) -> Self ! {dist_data_buffered, extract_payload(Msg)} end),

    {DistSock, BridgeSock} = create_loopback_pair(),

    case whereis(net_kernel) of
        undefined ->
            ?LOG_WARNING("[dist_bridge] net_kernel not found");
        KernelPid ->
            setup_accept_bridge(Self, KernelPid, Client, DistSock, BridgeSock,
                                BufferSubRef, SendTopic, RecvTopic, TunnelId)
    end.

setup_accept_bridge(Self, KernelPid, Client, DistSock, BridgeSock,
                    BufferSubRef, SendTopic, RecvTopic, TunnelId) ->
    KernelPid ! {accept, Self, DistSock, inet, macula_dist},
    receive
        {KernelPid, controller, DistCtrl} ->
            DistCtrl ! {Self, controller, ok},
            macula_relay_client:unsubscribe(Client, BufferSubRef),
            flush_buffered_to_socket(BridgeSock),
            start_bridge_io(Self, Client, BridgeSock, SendTopic, RecvTopic, TunnelId);
        {KernelPid, unsupported_protocol} ->
            ?LOG_WARNING("[dist_bridge] Unsupported protocol")
    after ?CONTROLLER_TIMEOUT ->
        ?LOG_WARNING("[dist_bridge] Controller timeout")
    end.

start_bridge_io(Self, Client, BridgeSock, SendTopic, RecvTopic, TunnelId) ->
    macula_relay_client:subscribe(Client, RecvTopic,
        fun(Msg) -> Self ! {tunnel_in, extract_payload(Msg)} end),
    spawn_link(fun() -> bridge_reader_loop(Client, BridgeSock, SendTopic, TunnelId) end),
    bridge_writer_loop(BridgeSock, TunnelId).

%%%===================================================================
%%% Internal — Connect-side Bridge
%%%===================================================================

create_dist_socket(MeshClient, TunnelId) ->
    SendTopic = <<"_dist.data.", TunnelId/binary, ".out">>,
    RecvTopic = <<"_dist.data.", TunnelId/binary, ".in">>,

    {DistSock, BridgeSock} = create_loopback_pair(),

    spawn_link(fun() ->
        tunnel_io_bridge(MeshClient, BridgeSock, SendTopic, RecvTopic, TunnelId)
    end),

    {ok, DistSock, DistSock}.

tunnel_io_bridge(MeshClient, BridgeSock, SendTopic, RecvTopic, TunnelId) ->
    Self = self(),
    {ok, _SubRef} = macula_relay_client:subscribe(MeshClient, RecvTopic,
        fun(Msg) -> Self ! {tunnel_in, extract_payload(Msg)} end),

    spawn_link(fun() -> bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId) end),
    bridge_writer_loop(BridgeSock, TunnelId).

%%%===================================================================
%%% Internal — Bridge I/O Loops
%%%===================================================================

bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId) ->
    case gen_tcp:recv(BridgeSock, 0, ?BRIDGE_RECV_TIMEOUT) of
        {ok, Data} ->
            macula_relay_client:publish(MeshClient, SendTopic, Data),
            bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId);
        {error, closed} ->
            ?LOG_INFO("[io_bridge] Reader closed for ~s", [TunnelId]);
        {error, Reason} ->
            ?LOG_WARNING("[io_bridge] Reader error ~p for ~s", [Reason, TunnelId])
    end.

bridge_writer_loop(BridgeSock, TunnelId) ->
    receive
        {tunnel_in, Data} when is_binary(Data) ->
            case gen_tcp:send(BridgeSock, Data) of
                ok -> bridge_writer_loop(BridgeSock, TunnelId);
                {error, Reason} ->
                    ?LOG_WARNING("[io_bridge] Writer error ~p for ~s", [Reason, TunnelId])
            end;
        stop ->
            gen_tcp:close(BridgeSock)
    end.

%%%===================================================================
%%% Internal — Loopback Pair + Helpers
%%%===================================================================

create_loopback_pair() ->
    %% DistSock (CSock) gets {packet, 2} for handshake framing.
    %% BridgeSock (ASock) gets {packet, raw} — transparent byte pipe.
    %% When dist_util switches DistSock to {packet, 4} post-handshake,
    %% the bridge doesn't care — it forwards raw bytes including headers.
    ListenOpts = [binary, {active, false}, {reuseaddr, true}, {ip, {127,0,0,1}}],
    {ok, LSock} = gen_tcp:listen(0, ListenOpts),
    {ok, Port} = inet:port(LSock),
    {ok, CSock} = gen_tcp:connect({127,0,0,1}, Port,
                                  [binary, {active, false}, {packet, 2}, {nodelay, true}]),
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    inet:setopts(ASock, [{packet, raw}, {nodelay, true}]),
    {CSock, ASock}.

flush_buffered_to_socket(BridgeSock) ->
    receive
        {dist_data_buffered, Data} ->
            gen_tcp:send(BridgeSock, Data),
            flush_buffered_to_socket(BridgeSock)
    after 0 ->
        ok
    end.

extract_payload(#{payload := P}) -> P;
extract_payload(P) when is_binary(P) -> P.
