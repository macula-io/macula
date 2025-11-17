%%%-------------------------------------------------------------------
%%% @doc
%%% Bootstrap Registry - Thin wrapper around macula_routing_server DHT.
%%%
%%% Delegates all storage/lookup operations to the DHT routing server.
%%% Bootstrap nodes use the DHT's built-in key-value storage for:
%%% 1. RPC Services - Advertised RPC endpoints
%%% 2. Pub/Sub Topics - Topic subscriptions
%%%
%%% The DHT routing_server handles storage, TTL, and cleanup internally.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bootstrap_registry).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    store_service/2, lookup_service/1,
    store_topic/2, lookup_topic/1,
    store/2, lookup/1  % Generic API
]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    routing_server :: pid()    % DHT routing server PID
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Store RPC service registration (delegates to DHT)
-spec store_service(Key :: binary(), Value :: term()) -> ok.
store_service(Key, Value) ->
    gen_server:call(?MODULE, {store, Key, Value}).

%% @doc Lookup RPC service by key (delegates to DHT)
-spec lookup_service(Key :: binary()) -> {ok, term()} | {error, not_found}.
lookup_service(Key) ->
    gen_server:call(?MODULE, {lookup, Key}).

%% @doc Store pub/sub topic subscription (delegates to DHT)
-spec store_topic(Topic :: binary(), Subscriber :: map()) -> ok.
store_topic(Topic, Subscriber) ->
    gen_server:call(?MODULE, {store, Topic, Subscriber}).

%% @doc Lookup subscribers for a topic (delegates to DHT)
-spec lookup_topic(Topic :: binary()) -> {ok, [map()]} | {error, not_found}.
lookup_topic(Topic) ->
    gen_server:call(?MODULE, {lookup, Topic}).

%% @doc Generic store (delegates to DHT)
-spec store(Key :: binary(), Value :: term()) -> ok.
store(Key, Value) ->
    gen_server:call(?MODULE, {store, Key, Value}).

%% @doc Generic lookup (delegates to DHT)
-spec lookup(Key :: binary()) -> {ok, term()} | {error, not_found}.
lookup(Key) ->
    gen_server:call(?MODULE, {lookup, Key}).

%%%===================================================================
%%% GenServer Callbacks
%%%===================================================================

init(_Config) ->
    %% Find DHT routing server
    case whereis(macula_routing_server) of
        undefined ->
            ?LOG_ERROR("[BootstrapRegistry] DHT routing server not running!"),
            {stop, no_routing_server};
        RoutingServerPid ->
            ?LOG_INFO("[BootstrapRegistry] Connected to DHT routing server: ~p", [RoutingServerPid]),
            State = #state{
                routing_server = RoutingServerPid
            },
            {ok, State}
    end.

%% Store key-value pair in DHT
handle_call({store, Key, Value}, _From, State) ->
    RoutingServerPid = State#state.routing_server,
    ok = macula_routing_server:store_local(RoutingServerPid, Key, Value),
    ?LOG_DEBUG("[BootstrapRegistry] Stored in DHT: ~p", [Key]),
    {reply, ok, State};

%% Lookup key-value pair in DHT
handle_call({lookup, Key}, _From, State) ->
    RoutingServerPid = State#state.routing_server,
    case macula_routing_server:get_local(RoutingServerPid, Key) of
        {ok, Value} ->
            {reply, {ok, Value}, State};
        not_found ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% No cleanup needed - DHT routing server handles storage
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
