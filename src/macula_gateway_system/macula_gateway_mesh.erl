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

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    stop/1,
    get_or_create_connection/3,
    send_async/4,
    remove_connection/2,
    get_connection_info/2,
    list_connections/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Types
-type connection_info() :: #{
    connection => pid() | reference() | undefined,
    stream => pid() | reference() | undefined,  % Persistent stream for reuse
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
%% Registers as 'macula_gateway_mesh' for discovery by pubsub module.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

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

%% @doc Send message asynchronously (fire-and-forget).
%% Connection creation and sending happens in a spawned process.
%% Does NOT block the caller - returns immediately with 'ok'.
%% Use this for pubsub_route and other non-critical messages.
-spec send_async(pid(), binary(), binary() | {inet:ip_address(), inet:port_number()}, binary()) -> ok.
send_async(Pid, NodeId, Address, EncodedMessage) ->
    gen_server:cast(Pid, {send_async, NodeId, Address, EncodedMessage}),
    ok.

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
    %% Get max connections from opts or use default (1000)
    MaxConnections = maps:get(max_mesh_connections, Opts, 1000),
    State = #state{
        opts = Opts,
        max_connections = MaxConnections,
        mesh_connections = #{},
        monitors = #{}
    },
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
            NewState = evict_lru_connection(State),
            create_new_connection(NodeId, Address, NewState);
        false ->
            %% Pool has space
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
%%% Cast handlers - Async message sending
%%%===================================================================

%% @doc Handle async send request - spawns process for connection + send.
%% Fire-and-forget pattern - does not block the gen_server.
handle_cast({send_async, NodeId, Address, EncodedMessage}, State) ->
    %% Spawn a process to handle connection and send
    %% This way the gen_server is not blocked by slow QUIC connections
    MeshPid = self(),
    spawn(fun() ->
        async_send_worker(MeshPid, NodeId, Address, EncodedMessage, State#state.opts)
    end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%===================================================================
%%% Monitor cleanup - Auto-remove dead connections
%%%===================================================================

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    NewState = handle_monitor_down(MonitorRef, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%%%===================================================================
%%% Termination
%%%===================================================================

terminate(_Reason, #state{mesh_connections = MeshConns}) ->
    close_all_connections(maps:to_list(MeshConns)),
    ok.

close_all_connections([]) ->
    ok;
close_all_connections([{_NodeId, #{connection := Conn}} | Rest]) ->
    close_connection_if_alive(Conn),
    close_all_connections(Rest).

close_connection_if_alive(Conn) ->
    case is_connection_alive(Conn) of
        true -> macula_quic:close(Conn);
        false -> ok
    end.

%% @private
%% @doc Handle monitor DOWN message - remove connection for the monitored process.
handle_monitor_down(MonitorRef, #state{monitors = Monitors} = State) ->
    do_handle_monitor_down(maps:get(MonitorRef, Monitors, undefined), MonitorRef, State).

do_handle_monitor_down(undefined, _MonitorRef, State) ->
    State;
do_handle_monitor_down(NodeId, MonitorRef, #state{monitors = Monitors, mesh_connections = MeshConns} = State) ->
    State#state{
        monitors = maps:remove(MonitorRef, Monitors),
        mesh_connections = maps:remove(NodeId, MeshConns)
    }.

%%%===================================================================
%%% Internal functions - Connection creation
%%%===================================================================

%% @private
%% Create new QUIC connection and open stream
create_new_connection(NodeId, Address, State) ->
    case create_mesh_connection(Address, State) of
        {ok, Conn, Stream} ->
            %% Note: Cannot monitor QUIC connections (they are NIF references, not processes)
            %% Connection failures will be detected on send errors

            %% Cache connection info with stream
            ConnectionInfo = #{
                connection => Conn,
                stream => Stream,
                address => Address,
                last_used => erlang:system_time(second)
            },

            NewMeshConns = maps:put(NodeId,
                ConnectionInfo, State#state.mesh_connections),

            NewState = State#state{
                mesh_connections = NewMeshConns
            },

            {reply, {ok, Stream}, NewState};

        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

%% @private
%% Reuse existing alive connection and its stream (create stream only once per connection)
reuse_connection(NodeId, ConnInfo, State) ->
    #{connection := Conn, address := Address} = ConnInfo,

    %% Check if we already have a cached stream for this connection
    case maps:get(stream, ConnInfo, undefined) of
        undefined ->
            %% No cached stream - create one and cache it
            open_and_cache_stream(NodeId, Conn, ConnInfo, Address, State);
        Stream ->
            %% Have a cached stream - reuse it
            UpdatedConnInfo = ConnInfo#{last_used => erlang:system_time(second)},
            NewMeshConns = maps:put(NodeId, UpdatedConnInfo, State#state.mesh_connections),
            NewState = State#state{mesh_connections = NewMeshConns},
            {reply, {ok, Stream}, NewState}
    end.

%% @private
%% Open a new stream and cache it in connection info
open_and_cache_stream(NodeId, Conn, ConnInfo, Address, State) ->
    case macula_quic:open_stream(Conn) of
        {ok, Stream} ->
            %% Update connection info with the new stream
            UpdatedConnInfo = ConnInfo#{
                stream => Stream,
                last_used => erlang:system_time(second)
            },
            NewMeshConns = maps:put(NodeId, UpdatedConnInfo, State#state.mesh_connections),
            NewState = State#state{mesh_connections = NewMeshConns},
            {reply, {ok, Stream}, NewState};

        {error, _StreamReason} ->
            %% Stream opening failed - connection likely dead
            NewState = do_remove_connection(NodeId, State),
            %% Retry recursively (will create new connection)
            handle_call({get_or_create_connection, NodeId, Address}, undefined, NewState)
    end.

%% @private
%% Remove dead connection and retry
remove_and_retry(NodeId, State) ->
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
%% Handles address in various formats:
%%   - `{Host, Port}' tuple (expected format)
%%   - Binary string like `host:port'
%%   - List string like `"host:port"'
create_mesh_connection(Address, State) ->
    {Host, Port} = parse_address(Address),
    %% Get TLS certificates from opts or environment
    {CertFile, KeyFile} = get_tls_certificates(State#state.opts),

    %% Connection options with proper keep-alive settings
    ConnOpts = [
        {cert, CertFile},
        {key, KeyFile},
        {alpn, ["macula"]},
        {verify, none},  % For now, accept any certificate
        {peer_unidi_stream_count, 3},
        {idle_timeout_ms, 60000},
        {keep_alive_interval_ms, 20000},
        {handshake_idle_timeout_ms, 30000}
    ],

    %% Format address for QUIC connection
    FormattedAddress = format_address(Host),

    %% Connect to peer
    case macula_quic:connect(FormattedAddress, Port, ConnOpts, 5000) of
        {ok, Conn} ->
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
%%% Address parsing - Handle various formats
%%%===================================================================

%% @private
%% @doc Parse address from various formats to {Host, Port} tuple.
%% Handles:
%%   - `{Host, Port}' tuple - passthrough
%%   - Binary `host:port' - parse and convert
%%   - String `"host:port"' - parse
-spec parse_address(term()) -> {string(), inet:port_number()}.
parse_address({Host, Port}) when is_integer(Port) ->
    %% Already a tuple - just ensure Host is a string
    {format_host(Host), Port};
parse_address(Address) when is_binary(Address) ->
    parse_address(binary_to_list(Address));
parse_address(Address) when is_list(Address) ->
    parse_host_port_string(Address).

%% @private
%% @doc Parse "host:port" string into {Host, Port} tuple.
-spec parse_host_port_string(string()) -> {string(), inet:port_number()}.
parse_host_port_string(Address) ->
    case string:rchr(Address, $:) of
        0 ->
            %% No port specified, use default QUIC port
            {Address, 4433};
        Pos ->
            Host = string:substr(Address, 1, Pos - 1),
            PortStr = string:substr(Address, Pos + 1),
            Port = list_to_integer(PortStr),
            {Host, Port}
    end.

%% @private
%% @doc Format host to string for QUIC connection.
-spec format_host(term()) -> string().
format_host(Host) when is_binary(Host) ->
    binary_to_list(Host);
format_host(Host) when is_list(Host) ->
    Host;
format_host({_, _, _, _} = IPv4) ->
    inet:ntoa(IPv4);
format_host({_, _, _, _, _, _, _, _} = IPv6) ->
    inet:ntoa(IPv6).

%%%===================================================================
%%% TLS certificate management
%%%===================================================================

%% @private
%% Get TLS certificate paths from opts or environment
get_tls_certificates(Opts) when is_map(Opts) ->
    %% Check opts first
    case {maps:get(cert_file, Opts, undefined), maps:get(key_file, Opts, undefined)} of
        {undefined, undefined} ->
            %% Fall back to environment variables, then macula_tls defaults
            get_tls_certificates_from_env();
        {CertFile, KeyFile} when CertFile =/= undefined, KeyFile =/= undefined ->
            {CertFile, KeyFile};
        _ ->
            %% Partial config in opts, use macula_tls defaults
            macula_tls:get_cert_paths()
    end.

%% @private
get_tls_certificates_from_env() ->
    case {os:getenv("TLS_CERT_FILE"), os:getenv("TLS_KEY_FILE")} of
        {false, false} ->
            %% No env vars, use macula_tls defaults (auto-generated if missing)
            macula_tls:get_cert_paths();
        {CertEnv, KeyEnv} when CertEnv =/= false, KeyEnv =/= false ->
            %% Use mounted certificates (production)
            {CertEnv, KeyEnv};
        _ ->
            %% Partial configuration, use macula_tls defaults
            macula_tls:get_cert_paths()
    end.

%%%===================================================================
%%% Connection liveness checking
%%%===================================================================

%% @private
%% @doc Check if QUIC connection is still valid.
%% QUIC connections are NIF references, NOT processes - cannot use is_process_alive.
%% We use quicer:sockname/1 as a liveness probe - if it returns {ok, _} the connection is alive.
is_connection_alive(undefined) ->
    false;
is_connection_alive(Conn) when is_reference(Conn) ->
    %% QUIC connection reference - use sockname as liveness probe
    try quicer:sockname(Conn) of
        {ok, _} -> true;
        {error, _} -> false
    catch
        _:_ -> false
    end;
is_connection_alive(Conn) when is_pid(Conn) ->
    %% Erlang process - use is_process_alive
    erlang:is_process_alive(Conn);
is_connection_alive(_Other) ->
    false.

%%%===================================================================
%%% Async send worker (runs in spawned process)
%%%===================================================================

%% @private
%% @doc Worker process for async message sending.
%% Creates connection, opens stream, sends message, closes stream.
%% Does NOT use the connection pool to avoid coordination overhead.
%% For fire-and-forget messages where delivery is best-effort.
-spec async_send_worker(pid(), binary(), term(), binary(), map()) -> ok | {error, term()}.
async_send_worker(_MeshPid, NodeId, Address, EncodedMessage, Opts) ->
    ?LOG_DEBUG("Starting async send to ~p", [binary:encode_hex(NodeId)]),

    %% Parse address and create connection
    {Host, Port} = parse_address(Address),
    {CertFile, KeyFile} = get_tls_certificates(Opts),

    ConnOpts = [
        {cert, CertFile},
        {key, KeyFile},
        {alpn, ["macula"]},
        {verify, none},
        {peer_unidi_stream_count, 3},
        {idle_timeout_ms, 60000},
        {keep_alive_interval_ms, 20000},
        {handshake_idle_timeout_ms, 30000}
    ],

    FormattedAddress = format_address(Host),
    ?LOG_DEBUG("Connecting to ~s:~p", [FormattedAddress, Port]),

    %% Try to connect with a shorter timeout for async sends
    case macula_quic:connect(FormattedAddress, Port, ConnOpts, 5000) of
        {ok, Conn} ->
            case macula_quic:open_stream(Conn) of
                {ok, Stream} ->
                    case macula_quic:send(Stream, EncodedMessage) of
                        ok ->
                            ?LOG_DEBUG("Message sent successfully to ~p",
                                     [binary:encode_hex(NodeId)]),
                            %% Brief delay for data transmission
                            timer:sleep(50),
                            %% Close stream and connection
                            catch macula_quic:close(Stream),
                            catch macula_quic:close(Conn),
                            ok;
                        {error, SendErr} ->
                            ?LOG_WARNING("Send failed: ~p", [SendErr]),
                            catch macula_quic:close(Stream),
                            catch macula_quic:close(Conn),
                            {error, {send_failed, SendErr}}
                    end;
                {error, StreamErr} ->
                    ?LOG_WARNING("Stream open failed: ~p", [StreamErr]),
                    catch macula_quic:close(Conn),
                    {error, {stream_failed, StreamErr}}
            end;
        {error, ConnErr} ->
            ?LOG_WARNING("Connection failed to ~s:~p: ~p",
                     [FormattedAddress, Port, ConnErr]),
            {error, {connect_failed, ConnErr}};
        {error, Type, Details} ->
            ?LOG_WARNING("Connection failed to ~s:~p: ~p ~p",
                     [FormattedAddress, Port, Type, Details]),
            {error, {connect_failed, {Type, Details}}}
    end.
