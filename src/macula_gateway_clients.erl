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
    get_client_stream/2,
    get_stream_by_endpoint/2
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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    io:format("[Clients] Initializing client manager~n"),
    %% Get max clients from opts or use default (10,000)
    MaxClients = maps:get(max_clients, Opts, 10000),
    io:format("[Clients] Max clients: ~p~n", [MaxClients]),

    State = #state{
        opts = Opts,
        max_clients = MaxClients,
        clients = #{},
        monitors = #{},
        client_streams = #{},
        endpoint_to_stream = #{}
    },
    io:format("[Clients] Client manager initialized~n"),
    {ok, State}.

handle_call({client_connected, ClientPid, ClientInfo}, _From,
            #state{clients = Clients, max_clients = MaxClients} = State) ->
    %% Check if client already connected (update case)
    case maps:is_key(ClientPid, Clients) of
        true ->
            %% Update existing client info (allowed even when pool is full)
            NewClients = maps:put(ClientPid, ClientInfo, Clients),
            NewState = State#state{clients = NewClients},
            {reply, ok, NewState};
        false ->
            %% New client - check if pool is full
            case maps:size(Clients) >= MaxClients of
                true ->
                    %% Pool full - reject new client (backpressure)
                    io:format("[ClientManager] Client pool full (~p clients), rejecting new client~n",
                             [MaxClients]),
                    {reply, {error, max_clients_reached}, State};
                false ->
                    %% Pool has space - monitor and store new client
                    MonitorRef = erlang:monitor(process, ClientPid),
                    NewClients = maps:put(ClientPid, ClientInfo, Clients),
                    NewMonitors = maps:put(MonitorRef, ClientPid, State#state.monitors),
                    NewState = State#state{
                        clients = NewClients,
                        monitors = NewMonitors
                    },
                    {reply, ok, NewState}
            end
    end;

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
    io:format("[ClientManager] Tracking endpoint → stream: ~s → ~p~n", [Endpoint, StreamPid]),
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

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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
    %% Get client info to extract node_id and endpoint before removing
    case maps:get(ClientPid, State#state.clients, undefined) of
        undefined ->
            %% Client not found, just return state
            State;
        ClientInfo ->
            %% Extract node_id and remove from client_streams
            NodeId = maps:get(node_id, ClientInfo),
            NewClientStreams = maps:remove(NodeId, State#state.client_streams),

            %% Extract endpoint (if present) and remove from endpoint_to_stream
            Endpoint = maps:get(endpoint, ClientInfo, undefined),
            NewEndpointToStream = case Endpoint of
                undefined -> State#state.endpoint_to_stream;
                <<>> -> State#state.endpoint_to_stream;
                _ ->
                    io:format("[ClientManager] Cleaning up endpoint mapping: ~s~n", [Endpoint]),
                    maps:remove(Endpoint, State#state.endpoint_to_stream)
            end,

            %% Remove from clients map
            NewClients = maps:remove(ClientPid, State#state.clients),

            State#state{
                clients = NewClients,
                client_streams = NewClientStreams,
                endpoint_to_stream = NewEndpointToStream
            }
    end.
