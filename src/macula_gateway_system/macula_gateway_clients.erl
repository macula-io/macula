%%%-------------------------------------------------------------------
%%% @doc
%%% Clients Worker GenServer - tracks connected clients.
%%%
%%% Responsibilities:
%%% - Track connected clients with metadata (BOUNDED POOL)
%%% - Enforce max_clients limit with backpressure
%%% - Monitor client processes for automatic cleanup
%%% - Store bidirectional streams for client communication
%%% - Provide client query APIs
%%%
%%% Pattern: Bounded client pool with backpressure
%%% - Tracks clients with max_clients limit (default: 10,000)
%%% - Rejects new clients when pool is full (backpressure)
%%% - Allows updates to existing clients even when pool is full
%%%
%%% Configuration:
%%% - max_clients: Maximum concurrent clients (default: 10,000)
%%%
%%% Extracted from macula_gateway.erl (Phase 2)
%%% Renamed from macula_gateway_client_manager (Phase 2 QUIC refactoring)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_clients).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    stop/1,
    client_connected/3,
    client_disconnected/2,
    get_client_info/2,
    get_all_clients/1,
    is_client_alive/2,
    store_client_stream/3,
    store_client_stream/4,
    get_client_stream/2,
    get_stream_by_endpoint/2,
    get_all_node_ids/1,
    broadcast/2,
    remove_stale_stream/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type client_info() :: #{
    realm := binary(),
    node_id := binary(),
    endpoint => binary(),
    capabilities => [atom()]
}.

-record(state, {
    opts :: map(),
    max_clients :: integer(),                       % Maximum clients allowed
    clients :: #{pid() => client_info()},           % client_pid => client_info
    monitors :: #{reference() => pid()},            % monitor_ref => client_pid
    client_streams :: #{binary() => pid()},         % node_id => stream_pid
    endpoint_to_stream :: #{binary() => pid()}      % endpoint => stream_pid
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the client manager with options.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Stop the client manager.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Register a connected client with metadata.
%% Monitors the client process for automatic cleanup on death.
-spec client_connected(pid(), pid(), client_info()) -> ok.
client_connected(Pid, ClientPid, ClientInfo) ->
    gen_server:call(Pid, {client_connected, ClientPid, ClientInfo}).

%% @doc Unregister a disconnected client.
-spec client_disconnected(pid(), pid()) -> ok.
client_disconnected(Pid, ClientPid) ->
    gen_server:call(Pid, {client_disconnected, ClientPid}).

%% @doc Get information about a specific client.
-spec get_client_info(pid(), pid()) -> {ok, client_info()} | not_found.
get_client_info(Pid, ClientPid) ->
    gen_server:call(Pid, {get_client_info, ClientPid}).

%% @doc Get all connected clients.
-spec get_all_clients(pid()) -> {ok, [{pid(), client_info()}]}.
get_all_clients(Pid) ->
    gen_server:call(Pid, get_all_clients).

%% @doc Check if a client is alive (process still running).
-spec is_client_alive(pid(), pid()) -> boolean().
is_client_alive(_Pid, ClientPid) ->
    erlang:is_process_alive(ClientPid).

%% @doc Store a bidirectional stream for a client node (legacy 3-arg version).
-spec store_client_stream(pid(), binary(), pid()) -> ok.
store_client_stream(Pid, NodeId, StreamPid) ->
    store_client_stream(Pid, NodeId, StreamPid, <<>>).

%% @doc Store a bidirectional stream for a client node with endpoint tracking.
-spec store_client_stream(pid(), binary(), pid(), binary()) -> ok.
store_client_stream(Pid, NodeId, StreamPid, Endpoint) when is_binary(Endpoint) ->
    gen_server:call(Pid, {store_client_stream, NodeId, StreamPid, Endpoint}).

%% @doc Get the stored stream for a client node.
-spec get_client_stream(pid(), binary()) -> {ok, pid()} | not_found.
get_client_stream(Pid, NodeId) ->
    gen_server:call(Pid, {get_client_stream, NodeId}).

%% @doc Get the stream PID for a given endpoint URL.
%% Used for routing pub/sub messages to remote subscribers.
-spec get_stream_by_endpoint(pid(), binary()) -> {ok, pid()} | {error, not_found}.
get_stream_by_endpoint(Pid, Endpoint) ->
    gen_server:call(Pid, {get_stream_by_endpoint, Endpoint}).

%% @doc Get all node IDs with stored client streams (for debugging).
-spec get_all_node_ids(pid()) -> [binary()].
get_all_node_ids(Pid) ->
    gen_server:call(Pid, get_all_node_ids).

%% @doc Broadcast a message to all connected clients.
-spec broadcast(pid(), binary()) -> ok.
broadcast(Pid, EncodedMsg) ->
    gen_server:cast(Pid, {broadcast, EncodedMsg}).

%% @doc Remove a stale stream for a node when send fails with 'closed'.
%% This is called by the gateway when quicer:send returns {error, closed}
%% to clean up the invalid stream reference from our maps.
%% Uses async cast to avoid blocking the gateway during send operations.
-spec remove_stale_stream(pid(), binary()) -> ok.
remove_stale_stream(Pid, NodeId) ->
    gen_server:cast(Pid, {remove_stale_stream, NodeId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    ?LOG_INFO("Initializing client manager"),
    %% Get max clients from opts or use default (10,000)
    MaxClients = maps:get(max_clients, Opts, 10000),
    ?LOG_INFO("Max clients: ~p", [MaxClients]),

    State = #state{
        opts = Opts,
        max_clients = MaxClients,
        clients = #{},
        monitors = #{},
        client_streams = #{},
        endpoint_to_stream = #{}
    },
    ?LOG_INFO("Client manager initialized"),
    {ok, State}.

handle_call({client_connected, ClientPid, ClientInfo}, _From, State) ->
    IsExisting = maps:is_key(ClientPid, State#state.clients),
    handle_client_connected(IsExisting, ClientPid, ClientInfo, State);

handle_call({client_disconnected, ClientPid}, _From, State) ->
    NewState = remove_client(ClientPid, State),
    {reply, ok, NewState};

handle_call({get_client_info, ClientPid}, _From, State) ->
    Result = case maps:get(ClientPid, State#state.clients, undefined) of
        undefined -> not_found;
        Info -> {ok, Info}
    end,
    {reply, Result, State};

handle_call(get_all_clients, _From, State) ->
    Clients = maps:to_list(State#state.clients),
    {reply, {ok, Clients}, State};

%% Legacy 3-arg version (no endpoint)
handle_call({store_client_stream, NodeId, StreamPid, <<>>}, _From, State) ->
    ClientStreams = maps:put(NodeId, StreamPid, State#state.client_streams),
    NewState = State#state{client_streams = ClientStreams},
    {reply, ok, NewState};

%% New 4-arg version with endpoint tracking
handle_call({store_client_stream, NodeId, StreamPid, Endpoint}, _From, State) when is_binary(Endpoint), byte_size(Endpoint) > 0 ->
    %% Store both node_id → stream and endpoint → stream mappings
    ClientStreams = maps:put(NodeId, StreamPid, State#state.client_streams),
    EndpointToStream = maps:put(Endpoint, StreamPid, State#state.endpoint_to_stream),
    NewState = State#state{
        client_streams = ClientStreams,
        endpoint_to_stream = EndpointToStream
    },
    ?LOG_DEBUG("Tracking endpoint → stream: ~s → ~p", [Endpoint, StreamPid]),
    {reply, ok, NewState};

handle_call({get_client_stream, NodeId}, _From, State) ->
    Result = case maps:get(NodeId, State#state.client_streams, undefined) of
        undefined -> not_found;
        StreamPid -> {ok, StreamPid}
    end,
    {reply, Result, State};

handle_call({get_stream_by_endpoint, Endpoint}, _From, State) ->
    Result = case maps:get(Endpoint, State#state.endpoint_to_stream, undefined) of
        undefined -> {error, not_found};
        StreamPid -> {ok, StreamPid}
    end,
    {reply, Result, State};

handle_call(get_all_node_ids, _From, State) ->
    NodeIds = maps:keys(State#state.client_streams),
    {reply, NodeIds, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({broadcast, EncodedMsg}, State) ->
    %% Broadcast message to all connected clients
    Streams = State#state.client_streams,
    Count = maps:fold(fun(_NodeId, Stream, Acc) ->
        case macula_quic:send(Stream, EncodedMsg) of
            ok -> Acc + 1;
            {error, Reason} ->
                ?LOG_WARNING("Broadcast send failed: ~p", [Reason]),
                Acc
        end
    end, 0, Streams),
    ?LOG_INFO("Broadcast sent to ~p client(s)", [Count]),
    {noreply, State};

%% @doc Remove a stale stream from client_streams map when send fails.
%% This prevents repeated attempts to send to closed streams and enables
%% clients to reconnect with fresh streams.
handle_cast({remove_stale_stream, NodeId}, State) ->
    case maps:is_key(NodeId, State#state.client_streams) of
        true ->
            ?LOG_WARNING("[ClientManager] Removing stale stream for node ~s",
                        [binary:encode_hex(NodeId)]),
            NewClientStreams = maps:remove(NodeId, State#state.client_streams),
            %% Also try to remove from endpoint_to_stream if we can find it
            %% (reverse lookup by stream PID)
            OldStreamPid = maps:get(NodeId, State#state.client_streams),
            NewEndpointToStream = remove_stream_from_endpoint_map(
                OldStreamPid, State#state.endpoint_to_stream),
            {noreply, State#state{
                client_streams = NewClientStreams,
                endpoint_to_stream = NewEndpointToStream
            }};
        false ->
            %% Stream already removed (possibly by reconnection)
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle client process death - automatic cleanup.
handle_info({'DOWN', MonitorRef, process, ClientPid, _Reason}, State) ->
    %% Remove monitor reference
    Monitors = maps:remove(MonitorRef, State#state.monitors),

    %% Remove client from registry
    NewState = remove_client(ClientPid, State#state{monitors = Monitors}),

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Remove a client from the registry.
%% Does not demonitor - that's handled separately in handle_info.
%% Also removes associated client stream from client_streams and endpoint_to_stream maps.
-spec remove_client(pid(), #state{}) -> #state{}.
remove_client(ClientPid, State) ->
    ClientInfo = maps:get(ClientPid, State#state.clients, undefined),
    do_remove_client(ClientInfo, ClientPid, State).

%%%===================================================================
%%% Client connected helpers
%%%===================================================================

%% @private Update existing client info (allowed even when pool is full)
handle_client_connected(true, ClientPid, ClientInfo, State) ->
    NewClients = maps:put(ClientPid, ClientInfo, State#state.clients),
    NewState = State#state{clients = NewClients},
    {reply, ok, NewState};
%% @private New client - check pool capacity
handle_client_connected(false, ClientPid, ClientInfo, #state{clients = Clients, max_clients = MaxClients} = State) ->
    IsFull = maps:size(Clients) >= MaxClients,
    handle_new_client(IsFull, ClientPid, ClientInfo, State).

%% @private Pool full - reject new client (backpressure)
handle_new_client(true, _ClientPid, _ClientInfo, #state{max_clients = MaxClients} = State) ->
    ?LOG_WARNING("Client pool full (~p clients), rejecting new client", [MaxClients]),
    {reply, {error, max_clients_reached}, State};
%% @private Pool has space - monitor and store new client
handle_new_client(false, ClientPid, ClientInfo, State) ->
    MonitorRef = erlang:monitor(process, ClientPid),
    NewClients = maps:put(ClientPid, ClientInfo, State#state.clients),
    NewMonitors = maps:put(MonitorRef, ClientPid, State#state.monitors),
    NewState = State#state{clients = NewClients, monitors = NewMonitors},
    {reply, ok, NewState}.

%%%===================================================================
%%% Client removal helpers
%%%===================================================================

%% @private Client not found - return state unchanged
do_remove_client(undefined, _ClientPid, State) ->
    State;
%% @private Remove client and associated streams
do_remove_client(ClientInfo, ClientPid, State) ->
    NodeId = maps:get(node_id, ClientInfo),
    Endpoint = maps:get(endpoint, ClientInfo, undefined),
    NewClientStreams = maps:remove(NodeId, State#state.client_streams),
    NewEndpointToStream = cleanup_endpoint_mapping(Endpoint, State#state.endpoint_to_stream),
    NewClients = maps:remove(ClientPid, State#state.clients),
    State#state{
        clients = NewClients,
        client_streams = NewClientStreams,
        endpoint_to_stream = NewEndpointToStream
    }.

cleanup_endpoint_mapping(undefined, EndpointToStream) ->
    EndpointToStream;
cleanup_endpoint_mapping(<<>>, EndpointToStream) ->
    EndpointToStream;
cleanup_endpoint_mapping(Endpoint, EndpointToStream) ->
    ?LOG_DEBUG("Cleaning up endpoint mapping: ~s", [Endpoint]),
    maps:remove(Endpoint, EndpointToStream).

%% @private Remove stream from endpoint_to_stream map by stream PID (reverse lookup).
%% Used when we know the stream is stale but don't have the endpoint key.
remove_stream_from_endpoint_map(StreamPid, EndpointToStream) ->
    %% Find endpoints that map to this stream PID and remove them
    maps:filter(fun(_Endpoint, Pid) -> Pid =/= StreamPid end, EndpointToStream).
