%%%-------------------------------------------------------------------
%%% @doc
%%% Bootstrap Server - Main GenServer for bootstrap node operations.
%%%
%%% Handles:
%%% - DHT queries (FIND_NODE, FIND_VALUE, STORE)
%%% - Peer registration and discovery
%%% - Statistics tracking
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bootstrap_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_dht_query/2, get_stats/0]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    realm :: binary(),
    node_id :: binary(),
    config :: map(),
    stats :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Handle DHT query from remote peer
-spec handle_dht_query(QueryType :: atom(), QueryData :: term()) ->
    {ok, term()} | {error, term()}.
handle_dht_query(QueryType, QueryData) ->
    gen_server:call(?MODULE, {dht_query, QueryType, QueryData}, 10000).

%% @doc Get bootstrap server statistics
-spec get_stats() -> {ok, map()}.
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%%===================================================================
%%% GenServer Callbacks
%%%===================================================================

init(Config) ->
    Realm = maps:get(realm, Config, <<"macula.bootstrap">>),
    %% Generate unique node ID for this bootstrap server
    NodeId = crypto:hash(sha256, term_to_binary({Realm, erlang:system_time(), self()})),

    ?LOG_INFO("[BootstrapServer] Starting for realm ~s", [Realm]),

    State = #state{
        realm = Realm,
        node_id = NodeId,
        config = Config,
        stats = #{
            queries_handled => 0,
            services_registered => 0,
            peers_discovered => 0,
            uptime_start => erlang:system_time(second)
        }
    },

    {ok, State}.

%% DHT FIND_NODE query - find K closest nodes to target ID
handle_call({dht_query, find_node, TargetId}, _From, State) when is_binary(TargetId) ->
    ?LOG_DEBUG("[BootstrapServer] FIND_NODE query for ~p", [binary:encode_hex(TargetId)]),

    %% Delegate to routing_server to find K closest nodes
    case whereis(macula_routing_server) of
        undefined ->
            {reply, {error, no_routing_server}, State};
        RoutingPid ->
            Result = macula_routing_server:find_closest(RoutingPid, TargetId, 20),
            NewStats = maps:update_with(queries_handled, fun(N) -> N + 1 end, State#state.stats),
            {reply, {ok, Result}, State#state{stats = NewStats}}
    end;

%% DHT FIND_VALUE query - lookup service by key
handle_call({dht_query, find_value, Key}, _From, State) when is_binary(Key) ->
    ?LOG_DEBUG("[BootstrapServer] FIND_VALUE query for ~p", [Key]),

    %% Delegate to service registry
    Result = macula_bootstrap_registry:lookup(Key),

    NewStats = maps:update_with(queries_handled, fun(N) -> N + 1 end, State#state.stats),
    {reply, Result, State#state{stats = NewStats}};

%% DHT STORE operation - store service registration
handle_call({dht_query, store, {Key, Value}}, _From, State) when is_binary(Key) ->
    ?LOG_DEBUG("[BootstrapServer] STORE operation for ~p", [Key]),

    %% Store in service registry
    ok = macula_bootstrap_registry:store(Key, Value),

    NewStats = maps:update_with(services_registered, fun(N) -> N + 1 end, State#state.stats),
    {reply, ok, State#state{stats = NewStats}};

%% Get statistics
handle_call(get_stats, _From, State) ->
    Uptime = erlang:system_time(second) - maps:get(uptime_start, State#state.stats),
    Stats = maps:put(uptime_seconds, Uptime, State#state.stats),
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG_INFO("[BootstrapServer] Shutting down"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
