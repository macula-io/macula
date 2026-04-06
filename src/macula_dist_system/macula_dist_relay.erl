%%%-------------------------------------------------------------------
%%% @doc Relay-routed Erlang distribution.
%%%
%%% When MACULA_DIST_MODE=relay, Erlang distribution connections
%%% are tunneled through the Macula relay mesh instead of direct QUIC.
%%% This enables distribution across firewalls and NATs — nodes only
%%% need outbound connectivity to a relay.
%%%
%%% The tunnel works by:
%%% 1. Node A publishes a DIST_CONNECT request to the mesh
%%% 2. The relay forwards it to Node B's handler
%%% 3. Node B opens a distribution stream back through the relay
%%% 4. Raw ETF bytes flow through the relay tunnel
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

-define(DIST_TOPIC, <<"_dist.connect">>).
-define(DIST_TIMEOUT, 25000).

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
    case os:getenv("MACULA_DIST_MODE") of
        "relay" -> true;
        _ -> false
    end.

%% @doc Connect to a remote node via relay mesh.
%% Returns {ok, Conn, Stream} where the stream carries ETF bytes
%% through the relay tunnel.
%% @doc Connect to a remote node via relay mesh.
%% NodeStr is the full node name (e.g., "hecate@beam00.lab" or "4433@host").
-spec connect(string(), string(), integer()) -> {ok, reference(), reference()} | {error, term()}.
connect(NodeStr, _Host, _Port) ->
    ?LOG_INFO("[dist_relay] Connecting to ~s via mesh", [NodeStr]),

    %% Step 1: Find which relay the target node is connected to
    %% For now, use the same relay we're connected to (they share the mesh)
    case get_mesh_client() of
        undefined ->
            {error, no_mesh_connection};
        MeshClient ->
            %% Step 2: Request a distribution tunnel via mesh RPC
            Procedure = <<"_dist.tunnel.", (list_to_binary(NodeStr))/binary>>,
            ?LOG_INFO("[dist_relay] Calling ~s via mesh RPC", [Procedure]),
            case macula_relay_client:call(MeshClient, Procedure, #{
                <<"from_node">> => atom_to_binary(node()),
                <<"target_node">> => list_to_binary(NodeStr)
            }, ?DIST_TIMEOUT) of
                {ok, #{<<"tunnel_id">> := TunnelId} = TunnelInfo} ->
                    error_logger:info_msg("[dist_relay] Tunnel established: ~s (~p)~n", [TunnelId, maps:keys(TunnelInfo)]),
                    %% Step 3: The tunnel_id identifies a relay-side bridge.
                    %% We use the mesh QUIC connection's stream for ETF bytes.
                    %% For the experimental version, we reuse the existing
                    %% relay_client connection and multiplex dist traffic
                    %% as pub/sub messages on a private topic.
                    create_dist_socket(MeshClient, TunnelId);
                {ok, #{<<"error">> := ErrorInfo}} ->
                    ?LOG_WARNING("[dist_relay] Tunnel request returned error: ~p", [ErrorInfo]),
                    {error, {tunnel_error, ErrorInfo}};
                {error, Reason} ->
                    ?LOG_WARNING("[dist_relay] Tunnel request failed: ~p", [Reason]),
                    {error, {tunnel_failed, Reason}}
            end
    end.

%% @doc Accept an incoming distribution connection via relay mesh.
%% Called when a remote node requests a tunnel to us.
-spec accept_dist(binary(), map()) -> {ok, pid()} | {error, term()}.
accept_dist(TunnelId, Opts) ->
    ?LOG_INFO("[dist_relay] Accepting dist tunnel: ~s", [TunnelId]),
    %% The accepting side creates a process that bridges
    %% mesh pub/sub messages to the dist_util handshake
    FromNode = maps:get(<<"from_node">>, Opts, <<>>),
    spawn_link(fun() -> dist_tunnel_process(TunnelId, FromNode) end),
    {ok, TunnelId}.

%% @doc Advertise this node as accepting distribution connections via relay.
%% Registers a `_dist.tunnel.{nodename}` RPC procedure on the mesh.
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

%%====================================================================
%% Internal — tunnel negotiation
%%====================================================================

handle_tunnel_request(Args) ->
    FromNode = maps:get(<<"from_node">>, Args, <<>>),
    TunnelId = base64:encode(crypto:strong_rand_bytes(16)),
    error_logger:info_msg("[dist_relay] Tunnel request from ~s, id: ~s~n", [FromNode, TunnelId]),

    %% The accepting side needs a PERSISTENT process that:
    %% - Subscribes to the connecting node's data topic
    %% - Forwards mesh messages to the Erlang dist handshake
    %% - Lives for the duration of the distribution connection
    %%
    %% We can't use self() here because this handler function runs in
    %% a temporary spawn inside relay_client — it dies after returning.
    SendTopic = <<"_dist.data.", TunnelId/binary, ".in">>,
    RecvTopic = <<"_dist.data.", TunnelId/binary, ".out">>,

    case get_mesh_client() of
        undefined ->
            {error, <<"no_mesh_client">>};
        Client ->
            %% Spawn a persistent bridge process for this tunnel
            BridgePid = spawn(fun() ->
                dist_accept_bridge(Client, TunnelId, SendTopic, RecvTopic, FromNode)
            end),
            error_logger:info_msg("[dist_relay] Tunnel bridge ~p for ~s~n", [BridgePid, TunnelId]),
            {ok, #{<<"tunnel_id">> => TunnelId,
                   <<"send_topic">> => SendTopic,
                   <<"recv_topic">> => RecvTopic}}
    end.

%% Persistent bridge process for the accepting side of a distribution tunnel.
%% Creates a gen_tcp loopback pair so OTP's dist_util gets a real fd.
%% One end goes to dist_util, the other is bridged to the relay tunnel.
dist_accept_bridge(Client, TunnelId, SendTopic, RecvTopic, _FromNode) ->
    error_logger:info_msg("[dist_bridge] Starting for tunnel ~s~n", [TunnelId]),

    Self = self(),

    error_logger:info_msg("[dist_bridge] Subscribing FIRST, then notifying kernel~n"),

    %% Subscribe BEFORE notifying kernel — buffer data until bridge socket ready.
    %% This prevents a race where alpha sends handshake data before beta subscribes.
    {ok, BufferSubRef} = macula_relay_client:subscribe(Client, RecvTopic,
        fun(Msg) ->
            Self ! {dist_data_buffered, extract_payload(Msg)}
        end),

    %% Create gen_tcp loopback pair — OTP needs a real fd
    {DistSock, BridgeSock} = create_loopback_pair(),
    error_logger:info_msg("[dist_bridge] Loopback pair created for tunnel ~s~n", [TunnelId]),

    KernelPid = whereis(net_kernel),

    case KernelPid of
        undefined ->
            error_logger:warning_msg("[dist_bridge] net_kernel not found~n");
        _ ->
            %% Tell kernel about the dist-facing socket (real gen_tcp port)
            KernelPid ! {accept, Self, DistSock, inet, macula_dist},
            receive
                {KernelPid, controller, DistCtrl} ->
                    error_logger:info_msg("[dist_bridge] Controller ~p~n", [DistCtrl]),
                    DistCtrl ! {Self, controller, ok},

                    %% Unsubscribe buffer callback, flush buffered data to bridge socket
                    macula_relay_client:unsubscribe(Client, BufferSubRef),
                    flush_buffered_to_socket(BridgeSock),

                    %% Now set up persistent relay↔socket bridge
                    %% Relay → BridgeSock (write end)
                    macula_relay_client:subscribe(Client, RecvTopic,
                        fun(Msg) ->
                            Self ! {tunnel_in, extract_payload(Msg)}
                        end),

                    %% BridgeSock → Relay (read end)
                    spawn(fun() ->
                        bridge_reader_loop(Client, BridgeSock, SendTopic, TunnelId)
                    end),

                    error_logger:info_msg("[dist_bridge] Data forwarding active~n"),
                    %% Writer loop: relay data → bridge socket → dist_util
                    bridge_writer_loop(BridgeSock, TunnelId);
                {KernelPid, unsupported_protocol} ->
                    error_logger:warning_msg("[dist_bridge] Unsupported protocol~n")
            after 30000 ->
                error_logger:warning_msg("[dist_bridge] Controller timeout~n")
            end
    end.

flush_buffered_to_socket(BridgeSock) ->
    receive
        {dist_data_buffered, Data} ->
            error_logger:info_msg("[dist_bridge] Flushing ~b buffered bytes~n", [byte_size(Data)]),
            gen_tcp:send(BridgeSock, Data),
            flush_buffered_to_socket(BridgeSock)
    after 0 ->
        ok
    end.

extract_payload(#{payload := P}) -> P;
extract_payload(P) when is_binary(P) -> P;
extract_payload(Msg) -> term_to_binary(Msg).

%%====================================================================
%% Internal — mesh client lookup
%%====================================================================

%% Find the mesh relay client. Uses a registered name convention —
%% any application that provides a relay client registers it as
%% macula_dist_mesh_client. No dependency on hecate or any specific app.
get_mesh_client() ->
    case persistent_term:get(macula_dist_mesh_client, undefined) of
        undefined -> undefined;
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> Pid;
                false -> undefined
            end
    end.

create_dist_socket(MeshClient, TunnelId) ->
    %% Create a gen_tcp loopback pair. OTP's dist_util needs a real fd.
    %% One end goes to dist_util. The other end is bridged to the relay tunnel.
    SendTopic = <<"_dist.data.", TunnelId/binary, ".out">>,
    RecvTopic = <<"_dist.data.", TunnelId/binary, ".in">>,

    {DistSock, BridgeSock} = create_loopback_pair(),
    error_logger:info_msg("[dist_relay] Loopback pair created for tunnel ~s~n", [TunnelId]),

    %% Spawn bridge: reads from BridgeSock → publishes to relay
    %%               subscribes to relay → writes to BridgeSock
    spawn(fun() ->
        tunnel_io_bridge(MeshClient, BridgeSock, SendTopic, RecvTopic, TunnelId)
    end),

    %% Return the dist-facing socket as a {gen_tcp, Socket} for dist_util
    {ok, DistSock, DistSock}.

create_loopback_pair() ->
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true},
                                     {ip, {127,0,0,1}}]),
    {ok, Port} = inet:port(LSock),
    {ok, CSock} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active, false}]),
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    {CSock, ASock}.

tunnel_io_bridge(MeshClient, BridgeSock, SendTopic, RecvTopic, TunnelId) ->
    error_logger:info_msg("[io_bridge] Starting for tunnel ~s~n", [TunnelId]),

    %% Subscribe to incoming relay data → write to bridge socket
    Self = self(),
    {ok, _SubRef} = macula_relay_client:subscribe(MeshClient, RecvTopic,
        fun(Msg) ->
            Data = extract_payload(Msg),
            Self ! {tunnel_in, Data}
        end),

    %% Set bridge socket to passive mode for controlled reading
    inet:setopts(BridgeSock, [{active, false}]),

    %% Spawn reader: reads from bridge socket → publishes to relay
    spawn(fun() -> bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId) end),

    %% Writer loop: receives relay data → writes to bridge socket
    bridge_writer_loop(BridgeSock, TunnelId).

bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId) ->
    case gen_tcp:recv(BridgeSock, 0, 30000) of
        {ok, Data} ->
            macula_relay_client:publish(MeshClient, SendTopic, Data),
            bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId);
        {error, closed} ->
            error_logger:info_msg("[io_bridge] Reader closed for ~s~n", [TunnelId]);
        {error, Reason} ->
            error_logger:warning_msg("[io_bridge] Reader error ~p for ~s~n", [Reason, TunnelId])
    end.

bridge_writer_loop(BridgeSock, TunnelId) ->
    receive
        {tunnel_in, Data} when is_binary(Data) ->
            case gen_tcp:send(BridgeSock, Data) of
                ok -> bridge_writer_loop(BridgeSock, TunnelId);
                {error, Reason} ->
                    error_logger:warning_msg("[io_bridge] Writer error ~p for ~s~n",
                                            [Reason, TunnelId])
            end;
        stop ->
            gen_tcp:close(BridgeSock)
    end.

dist_tunnel_process(_TunnelId, _FromNode) ->
    %% This process lives for the duration of the distribution connection
    receive
        stop -> ok
    end.
