%%%-------------------------------------------------------------------
%%% @doc
%%% Client Manager GenServer - manages client lifecycle and tracking.
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
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_client_manager).

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
    get_client_stream/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type client_info() :: #{
    realm := binary(),
    node_id := binary(),
    capabilities => [atom()]
}.

-record(state, {
    opts :: map(),
    max_clients :: integer(),                       % Maximum clients allowed
    clients :: #{pid() => client_info()},           % client_pid => client_info
    monitors :: #{reference() => pid()},            % monitor_ref => client_pid
    client_streams :: #{binary() => pid()}          % node_id => stream_pid
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

%% @doc Store a bidirectional stream for a client node.
-spec store_client_stream(pid(), binary(), pid()) -> ok.
store_client_stream(Pid, NodeId, StreamPid) ->
    gen_server:call(Pid, {store_client_stream, NodeId, StreamPid}).

%% @doc Get the stored stream for a client node.
-spec get_client_stream(pid(), binary()) -> {ok, pid()} | not_found.
get_client_stream(Pid, NodeId) ->
    gen_server:call(Pid, {get_client_stream, NodeId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    %% Get max clients from opts or use default (10,000)
    MaxClients = maps:get(max_clients, Opts, 10000),

    State = #state{
        opts = Opts,
        max_clients = MaxClients,
        clients = #{},
        monitors = #{},
        client_streams = #{}
    },
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

handle_call({store_client_stream, NodeId, StreamPid}, _From, State) ->
    ClientStreams = maps:put(NodeId, StreamPid, State#state.client_streams),
    NewState = State#state{client_streams = ClientStreams},
    {reply, ok, NewState};

handle_call({get_client_stream, NodeId}, _From, State) ->
    Result = case maps:get(NodeId, State#state.client_streams, undefined) of
        undefined -> not_found;
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
%% Also removes associated client stream from client_streams map.
-spec remove_client(pid(), #state{}) -> #state{}.
remove_client(ClientPid, State) ->
    %% Get client info to extract node_id before removing
    case maps:get(ClientPid, State#state.clients, undefined) of
        undefined ->
            %% Client not found, just return state
            State;
        ClientInfo ->
            %% Extract node_id and remove from client_streams
            NodeId = maps:get(node_id, ClientInfo),
            NewClientStreams = maps:remove(NodeId, State#state.client_streams),

            %% Remove from clients map
            NewClients = maps:remove(ClientPid, State#state.clients),

            State#state{
                clients = NewClients,
                client_streams = NewClientStreams
            }
    end.
