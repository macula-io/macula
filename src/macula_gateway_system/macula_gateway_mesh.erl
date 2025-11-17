%%%-------------------------------------------------------------------
%%% @doc
%%% Mesh Connection Manager GenServer - manages peer-to-peer QUIC connections.
%%%
%%% Responsibilities:
%%% - Pool QUIC connections to remote peers by node_id (BOUNDED POOL)
%%% - Enforce max_connections limit with LRU eviction
%%% - Check connection liveness before reuse
%%% - Open new streams on pooled connections
%%% - Monitor connection processes for automatic cleanup
%%% - Cache connection metadata with timestamps
%%%
%%% Pattern: Bounded connection pooling with LRU eviction
%%% - Cache connections by node_id (max: max_mesh_connections, default 1000)
%%% - Evict least recently used when pool is full
%%% - Verify liveness before reuse (open new stream)
%%% - Remove dead connections and recreate on demand
%%%
%%% Configuration:
%%% - max_mesh_connections: Maximum pooled connections (default: 1000)
%%%
%%% Extracted from macula_gateway.erl (Phase 9)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_mesh).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    stop/1,
    get_or_create_connection/3,
    remove_connection/2,
    get_connection_info/2,
    list_connections/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Types
-type connection_info() :: #{
    connection => pid() | reference() | undefined,
    address => {inet:ip_address(), inet:port_number()},
    last_used => integer()
}.

-record(state, {
    opts :: map(),
    max_connections :: integer(),  % Maximum pooled connections
    mesh_connections :: #{binary() => connection_info()},
    monitors :: #{reference() => binary()}  % monitor_ref => node_id
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the mesh connection manager with options.
%% Options:
%%   - cert_file: Path to TLS certificate
%%   - key_file: Path to TLS private key
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Stop the mesh connection manager.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Get existing connection or create new one.
%% Returns opened stream ready for use.
-spec get_or_create_connection(pid(), binary(), {inet:ip_address(), inet:port_number()}) ->
    {ok, pid()} | {error, term()}.
get_or_create_connection(Pid, NodeId, Address) ->
    gen_server:call(Pid, {get_or_create_connection, NodeId, Address}, 10000).

%% @doc Explicitly remove connection from cache.
-spec remove_connection(pid(), binary()) -> ok.
remove_connection(Pid, NodeId) ->
    gen_server:call(Pid, {remove_connection, NodeId}).

%% @doc Get connection metadata for a node.
-spec get_connection_info(pid(), binary()) -> {ok, connection_info()} | not_found.
get_connection_info(Pid, NodeId) ->
    gen_server:call(Pid, {get_connection_info, NodeId}).

%% @doc List all cached connections.
-spec list_connections(pid()) -> {ok, [{binary(), connection_info()}]}.
list_connections(Pid) ->
    gen_server:call(Pid, list_connections).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    io:format("[Mesh] Initializing mesh connection manager~n"),
    %% Get max connections from opts or use default (1000)
    MaxConnections = maps:get(max_mesh_connections, Opts, 1000),
    io:format("[Mesh] Max connections: ~p~n", [MaxConnections]),

    State = #state{
        opts = Opts,
        max_connections = MaxConnections,
        mesh_connections = #{},
        monitors = #{}
    },
    io:format("[Mesh] Mesh connection manager initialized~n"),
    {ok, State}.

%%%===================================================================
%%% get_or_create_connection - Main connection pooling logic
%%%===================================================================

%% Pattern A: No cached connection - check pool size and create new
handle_call({get_or_create_connection, NodeId, Address}, _From,
            #state{mesh_connections = MeshConns, max_connections = MaxConns} = State)
    when not is_map_key(NodeId, MeshConns) ->

    %% Check if pool is full
    case maps:size(MeshConns) >= MaxConns of
        true ->
            %% Pool full - evict LRU entry
            io:format("[Mesh] Pool full (~p connections), evicting LRU~n", [MaxConns]),
            NewState = evict_lru_connection(State),
            io:format("[Mesh] Creating new connection to ~p at ~p~n",
                     [binary:encode_hex(NodeId), Address]),
            create_new_connection(NodeId, Address, NewState);
        false ->
            %% Pool has space
            io:format("[Mesh] Creating new connection to ~p at ~p~n",
                     [binary:encode_hex(NodeId), Address]),
            create_new_connection(NodeId, Address, State)
    end;

%% Pattern B: Cached connection exists with valid conn PID - check liveness
handle_call({get_or_create_connection, NodeId, _Address}, _From,
            #state{mesh_connections = MeshConns} = State) ->
    #{NodeId := #{connection := Conn} = ConnInfo} = MeshConns,

    case is_connection_alive(Conn) of
        true ->
            reuse_connection(NodeId, ConnInfo, State);
        false ->
            remove_and_retry(NodeId, State)
    end;

%%%===================================================================
%%% remove_connection - Explicit removal
%%%===================================================================

handle_call({remove_connection, NodeId}, _From, State) ->
    NewState = do_remove_connection(NodeId, State),
    {reply, ok, NewState};

%%%===================================================================
%%% Query operations
%%%===================================================================

handle_call({get_connection_info, NodeId}, _From,
            #state{mesh_connections = MeshConns} = State) ->
    case maps:get(NodeId, MeshConns, undefined) of
        undefined ->
            {reply, not_found, State};
        ConnInfo ->
            {reply, {ok, ConnInfo}, State}
    end;

handle_call(list_connections, _From,
            #state{mesh_connections = MeshConns} = State) ->
    Connections = maps:to_list(MeshConns),
    {reply, {ok, Connections}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%%===================================================================
%%% Cast handlers (currently unused)
%%%===================================================================

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%===================================================================
%%% Monitor cleanup - Auto-remove dead connections
%%%===================================================================

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    #state{monitors = Monitors, mesh_connections = MeshConns} = State,

    case maps:get(MonitorRef, Monitors, undefined) of
        undefined ->
            {noreply, State};
        NodeId ->
            io:format("[Mesh] Connection to ~p died, removing from cache~n",
                     [binary:encode_hex(NodeId)]),
            NewMonitors = maps:remove(MonitorRef, Monitors),
            NewMeshConns = maps:remove(NodeId, MeshConns),
            NewState = State#state{
                monitors = NewMonitors,
                mesh_connections = NewMeshConns
            },
            {noreply, NewState}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%%===================================================================
%%% Termination
%%%===================================================================

terminate(_Reason, #state{mesh_connections = MeshConns}) ->
    %% Close all connections on shutdown
    lists:foreach(fun({_NodeId, #{connection := Conn}}) ->
        case is_connection_alive(Conn) of
            true -> macula_quic:close(Conn);
            false -> ok
        end
    end, maps:to_list(MeshConns)),
    ok.

%%%===================================================================
%%% Internal functions - Connection creation
%%%===================================================================

%% @private
%% Create new QUIC connection and open stream
create_new_connection(NodeId, Address, State) ->
    case create_mesh_connection(Address, State) of
        {ok, Conn, Stream} ->
            %% Monitor the connection
            MonitorRef = erlang:monitor(process, Conn),

            %% Cache connection info
            ConnectionInfo = #{
                connection => Conn,
                address => Address,
                last_used => erlang:system_time(second)
            },

            NewMeshConns = maps:put(NodeId,
                ConnectionInfo, State#state.mesh_connections),
            NewMonitors = maps:put(MonitorRef, NodeId, State#state.monitors),

            NewState = State#state{
                mesh_connections = NewMeshConns,
                monitors = NewMonitors
            },

            {reply, {ok, Stream}, NewState};

        {error, Reason} ->
            io:format("[Mesh] Failed to create connection: ~p~n", [Reason]),
            {reply, {error, Reason}, State}
    end.

%% @private
%% Reuse existing alive connection (open new stream)
reuse_connection(NodeId, ConnInfo, State) ->
    #{connection := Conn, address := Address} = ConnInfo,

    io:format("[Mesh] Reusing connection to ~p, opening new stream~n",
             [binary:encode_hex(NodeId)]),

    case macula_quic:open_stream(Conn) of
        {ok, Stream} ->
            %% Update last_used timestamp
            UpdatedConnInfo = ConnInfo#{last_used => erlang:system_time(second)},
            NewMeshConns = maps:put(NodeId,
                UpdatedConnInfo, State#state.mesh_connections),
            NewState = State#state{mesh_connections = NewMeshConns},
            {reply, {ok, Stream}, NewState};

        {error, _StreamReason} ->
            %% Stream opening failed - connection likely dead
            io:format("[Mesh] Failed to open stream, removing connection and retrying~n"),
            NewState = do_remove_connection(NodeId, State),
            %% Retry recursively (will create new connection)
            handle_call({get_or_create_connection, NodeId, Address}, undefined, NewState)
    end.

%% @private
%% Remove dead connection and retry
remove_and_retry(NodeId, State) ->
    io:format("[Mesh] Connection to ~p is dead, removing and recreating~n",
             [binary:encode_hex(NodeId)]),
    NewState = do_remove_connection(NodeId, State),

    %% Retry - need to get address from removed connection info
    #{NodeId := #{address := Address}} = State#state.mesh_connections,
    handle_call({get_or_create_connection, NodeId, Address}, undefined, NewState).

%% @private
%% Evict least recently used connection from pool
-spec evict_lru_connection(#state{}) -> #state{}.
evict_lru_connection(#state{mesh_connections = MeshConns} = State)
    when map_size(MeshConns) == 0 ->
    %% No connections to evict
    State;
evict_lru_connection(#state{mesh_connections = MeshConns} = State) ->
    %% Find connection with oldest last_used timestamp
    ConnectionsList = maps:to_list(MeshConns),

    %% Sort by last_used (oldest first)
    SortedConns = lists:sort(
        fun({_, #{last_used := T1}}, {_, #{last_used := T2}}) ->
            T1 =< T2  % Oldest first
        end,
        ConnectionsList
    ),

    %% Get oldest connection's NodeId
    [{OldestNodeId, _} | _] = SortedConns,

    io:format("[Mesh] Evicting LRU connection: ~p~n", [binary:encode_hex(OldestNodeId)]),

    %% Remove oldest connection
    do_remove_connection(OldestNodeId, State).

%% @private
%% Remove connection and its monitor
do_remove_connection(NodeId, State) ->
    #state{mesh_connections = MeshConns, monitors = Monitors} = State,

    %% Find and remove monitor
    NewMonitors = case find_monitor_ref(NodeId, Monitors) of
        undefined ->
            Monitors;
        MonitorRef ->
            erlang:demonitor(MonitorRef, [flush]),
            maps:remove(MonitorRef, Monitors)
    end,

    %% Remove connection
    NewMeshConns = maps:remove(NodeId, MeshConns),

    State#state{
        mesh_connections = NewMeshConns,
        monitors = NewMonitors
    }.

%% @private
%% Find monitor reference for a node_id
find_monitor_ref(NodeId, Monitors) ->
    find_monitor_in_list(maps:to_list(Monitors), NodeId).

%% @doc Find monitor in monitor list.
find_monitor_in_list([], _NodeId) ->
    undefined;
find_monitor_in_list(MonitorsList, NodeId) ->
    extract_monitor_ref(lists:keyfind(NodeId, 2, MonitorsList)).

%% @doc Extract monitor reference from keyfind result.
extract_monitor_ref({MonitorRef, _NodeId}) -> MonitorRef;
extract_monitor_ref(false) -> undefined.

%%%===================================================================
%%% QUIC connection creation
%%%===================================================================

%% @private
%% @doc Create QUIC connection to peer.
create_mesh_connection({Host, Port}, State) ->
    %% Get TLS certificates from opts or environment
    {CertFile, KeyFile} = get_tls_certificates(State#state.opts),

    %% Connection options
    ConnOpts = [
        {cert, CertFile},
        {key, KeyFile},
        {alpn, ["macula"]},
        {verify, none},  % For now, accept any certificate
        {peer_unidi_stream_count, 3}
    ],

    %% Format address
    Address = format_address(Host),

    io:format("[Mesh] Connecting to peer at ~s:~p~n", [Address, Port]),

    %% Connect to peer
    case macula_quic:connect(Address, Port, ConnOpts, 5000) of
        {ok, Conn} ->
            io:format("[Mesh] Connected, opening stream~n"),
            case macula_quic:open_stream(Conn) of
                {ok, Stream} ->
                    {ok, Conn, Stream};
                {error, _StreamReason} = StreamError ->
                    macula_quic:close(Conn),
                    {error, {stream_failed, StreamError}}
            end;
        ErrorResult ->
            %% Handle any error format from quicer
            {error, {connect_failed, ErrorResult}}
    end.

%% @private
%% Format address for macula_quic:connect/4
format_address({_, _, _, _} = IPv4) ->
    inet:ntoa(IPv4);  % IPv4 tuple
format_address({_, _, _, _, _, _, _, _} = IPv6) ->
    inet:ntoa(IPv6);  % IPv6 tuple
format_address(HostStr) when is_list(HostStr) ->
    HostStr;
format_address(HostBin) when is_binary(HostBin) ->
    binary_to_list(HostBin).

%%%===================================================================
%%% TLS certificate management
%%%===================================================================

%% @private
%% Get TLS certificate paths from opts or environment
get_tls_certificates(Opts) when is_map(Opts) ->
    %% Check opts first
    case {maps:get(cert_file, Opts, undefined), maps:get(key_file, Opts, undefined)} of
        {undefined, undefined} ->
            %% Fall back to environment variables
            get_tls_certificates_from_env();
        {CertFile, KeyFile} when CertFile =/= undefined, KeyFile =/= undefined ->
            {CertFile, KeyFile};
        _ ->
            %% Partial config in opts, use defaults
            io:format("[Mesh] WARNING: Partial TLS config in opts, using defaults~n"),
            {"/opt/macula/certs/cert.pem", "/opt/macula/certs/key.pem"}
    end.

%% @private
get_tls_certificates_from_env() ->
    case {os:getenv("TLS_CERT_FILE"), os:getenv("TLS_KEY_FILE")} of
        {false, false} ->
            %% No env vars, use pre-generated certs
            {"/opt/macula/certs/cert.pem", "/opt/macula/certs/key.pem"};
        {CertEnv, KeyEnv} when CertEnv =/= false, KeyEnv =/= false ->
            %% Use mounted certificates (production)
            {CertEnv, KeyEnv};
        _ ->
            %% Partial configuration, use defaults
            {"/opt/macula/certs/cert.pem", "/opt/macula/certs/key.pem"}
    end.

%%%===================================================================
%%% Connection liveness checking
%%%===================================================================

%% @private
%% Check if connection process is alive
is_connection_alive(undefined) ->
    false;
is_connection_alive(Conn) when is_pid(Conn) orelse is_reference(Conn) ->
    try
        erlang:is_process_alive(Conn)
    catch
        _:_ -> false
    end;
is_connection_alive(_Other) ->
    false.
