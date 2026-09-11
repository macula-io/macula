%%%-------------------------------------------------------------------
%%% @doc Node discovery for Erlang distribution over mDNS.
%%%
%%% A registry of distribution nodes that subscribers can query and watch,
%%% fed by this node's own registration and by mDNS announcements.
%%%
%%% == Availability ==
%%%
%%% Only mDNS discovery runs, in a server started with
%%% `discovery_type => mdns'; announcing this node needs the `macula_mdns'
%%% application to be running. DHT discovery is not available: it needs a
%%% DHT module that no macula release provides, so a server started with
%%% `discovery_type => dht', or with `both' (the default), does not start,
%%% and `start_link/1' returns `{error, {strategy_unavailable, dht}}'. The
%%% macula application does not start this server, and this module is
%%% removed in 11.0.0.
%%%
%%% == Usage ==
%%%
%%% Start the server:
%%%   {ok, _} = macula_dist_discovery:start_link(#{discovery_type => mdns}).
%%%
%%% Register this node:
%%%   ok = macula_dist_discovery:register_node('4433@192.168.1.100', 4433).
%%%
%%% Look up a node:
%%%   {ok, #{host := Host, port := Port}} = macula_dist_discovery:lookup_node('4433@192.168.1.100').
%%%
%%% Subscribe to node events:
%%%   ok = macula_dist_discovery:subscribe(self()).
%%%   %% Receive: {node_discovered, Node, IP, Port}
%%%   %% Receive: {node_lost, Node}
%%%
%%% @copyright 2025 Macula.io Apache-2.0
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_discovery).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    register_node/2,
    unregister_node/1,
    lookup_node/1,
    lookup_node/2,
    list_nodes/0,
    subscribe/1,
    unsubscribe/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TTL, 300).  % 5 minutes
-define(REFRESH_INTERVAL, 60000).  % 1 minute
-define(CLEANUP_INTERVAL, 120000).  % 2 minutes

-record(state, {
    %% Local node registration
    local_node :: atom() | undefined,
    local_port :: integer() | undefined,
    local_info :: map() | undefined,

    %% Known nodes cache
    nodes :: #{atom() => map()},

    %% Subscribers for node events
    subscribers :: [pid()],

    %% Timers
    refresh_timer :: reference() | undefined,
    cleanup_timer :: reference() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the discovery server with default options. The default
%% discovery type is `both', which includes DHT discovery, so this returns
%% `{error, {strategy_unavailable, dht}}'. Use `start_link/1' with
%% `discovery_type => mdns'.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the discovery server with options. `discovery_type' is
%% `mdns', `dht' or `both' (the default); only `mdns' starts, and the other
%% two return `{error, {strategy_unavailable, dht}}'.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Register this node in the distributed registry.
%% This replaces EPMD registration.
-spec register_node(atom(), integer()) -> ok | {error, term()}.
register_node(NodeName, Port) ->
    gen_server:call(?SERVER, {register_node, NodeName, Port}).

%% @doc Unregister this node from the distributed registry.
-spec unregister_node(atom()) -> ok.
unregister_node(NodeName) ->
    gen_server:call(?SERVER, {unregister_node, NodeName}).

%% @doc Look up a node by name.
%% This replaces EPMD lookup.
-spec lookup_node(atom()) -> {ok, map()} | {error, not_found}.
lookup_node(NodeName) ->
    lookup_node(NodeName, 5000).

%% @doc Look up a node by name with timeout.
-spec lookup_node(atom(), timeout()) -> {ok, map()} | {error, not_found}.
lookup_node(NodeName, Timeout) ->
    gen_server:call(?SERVER, {lookup_node, NodeName}, Timeout).

%% @doc List all known nodes.
-spec list_nodes() -> [atom()].
list_nodes() ->
    gen_server:call(?SERVER, list_nodes).

%% @doc Subscribe to node discovery events.
%% Subscriber will receive:
%%   {node_discovered, NodeName, IP, Port}
%%   {node_lost, NodeName}
-spec subscribe(pid()) -> ok.
subscribe(Pid) ->
    gen_server:call(?SERVER, {subscribe, Pid}).

%% @doc Unsubscribe from node discovery events.
-spec unsubscribe(pid()) -> ok.
unsubscribe(Pid) ->
    gen_server:call(?SERVER, {unsubscribe, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Opts) ->
    process_flag(trap_exit, true),
    started_as(maps:get(discovery_type, Opts, both)).

%% @private DHT discovery needs a DHT module that no macula release
%% provides, so a server asked for it, alone or together with mDNS, does
%% not start. Returning an error stops the server normally, so a linked
%% caller is not taken down with it.
started_as(mdns) ->
    {ok, #state{
        nodes = #{},
        subscribers = [],
        refresh_timer = erlang:send_after(?REFRESH_INTERVAL, self(), refresh_registration),
        cleanup_timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired)
    }};
started_as(dht) ->
    {error, {strategy_unavailable, dht}};
started_as(both) ->
    {error, {strategy_unavailable, dht}}.

%% @private
handle_call({register_node, NodeName, Port}, _From, State) ->
    {ok, Host} = inet:gethostname(),
    {ok, Addrs} = inet:getaddrs(Host, inet),
    IP = hd(Addrs),

    NodeInfo = #{
        name => NodeName,
        port => Port,
        host => Host,
        ip => IP,
        protocol => 'macula-dist',
        registered_at => erlang:system_time(second),
        ttl => ?DEFAULT_TTL
    },

    %% Store in the local cache
    ok = store_in_local_cache(NodeName, NodeInfo),

    %% Also announce via mDNS when it runs
    maybe_announce_mdns(NodeName, Port),

    {reply, ok, State#state{
        local_node = NodeName,
        local_port = Port,
        local_info = NodeInfo
    }};

handle_call({unregister_node, NodeName}, _From, State) ->
    %% Remove from the local cache
    ok = remove_from_local_cache(NodeName),

    %% Remove from mDNS when it runs
    maybe_unannounce_mdns(NodeName),

    NewState = case State#state.local_node of
        NodeName ->
            State#state{
                local_node = undefined,
                local_port = undefined,
                local_info = undefined
            };
        _ ->
            State
    end,

    {reply, ok, NewState};

handle_call({lookup_node, NodeName}, _From, State) ->
    %% First check the known nodes
    lookup_cached(maps:get(NodeName, State#state.nodes, undefined), NodeName, State);

handle_call(list_nodes, _From, State) ->
    Nodes = maps:keys(State#state.nodes),
    {reply, Nodes, State};

handle_call({subscribe, Pid}, _From, State) ->
    %% Monitor the subscriber
    erlang:monitor(process, Pid),
    Subscribers = [Pid | State#state.subscribers],
    {reply, ok, State#state{subscribers = Subscribers}};

handle_call({unsubscribe, Pid}, _From, State) ->
    Subscribers = lists:delete(Pid, State#state.subscribers),
    {reply, ok, State#state{subscribers = Subscribers}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private Unknown node -> look in the local cache; known node -> validate
lookup_cached(undefined, NodeName, State) ->
    lookup_miss_result(lookup_in_local_cache(NodeName), NodeName, State);
lookup_cached(NodeInfo, NodeName, State) ->
    %% Check if the known entry is still valid
    lookup_valid(is_entry_valid(NodeInfo), NodeInfo, NodeName, State).

lookup_miss_result({ok, NodeInfo}, NodeName, State) ->
    %% Remember the result
    NewNodes = maps:put(NodeName, NodeInfo, State#state.nodes),
    {reply, {ok, NodeInfo}, State#state{nodes = NewNodes}};
lookup_miss_result({error, Reason}, _NodeName, State) ->
    {reply, {error, Reason}, State}.

lookup_valid(true, NodeInfo, _NodeName, State) ->
    {reply, {ok, NodeInfo}, State};
lookup_valid(false, _NodeInfo, NodeName, State) ->
    %% Expired, refresh from the local cache
    lookup_refresh_result(lookup_in_local_cache(NodeName), NodeName, State).

lookup_refresh_result({ok, FreshInfo}, NodeName, State) ->
    NewNodes = maps:put(NodeName, FreshInfo, State#state.nodes),
    {reply, {ok, FreshInfo}, State#state{nodes = NewNodes}};
lookup_refresh_result({error, Reason}, NodeName, State) ->
    NewNodes = maps:remove(NodeName, State#state.nodes),
    {reply, {error, Reason}, State#state{nodes = NewNodes}}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(refresh_registration, State) ->
    %% Refresh our registration in the local cache
    NewState = case State#state.local_info of
        undefined ->
            State;
        NodeInfo ->
            ok = store_in_local_cache(State#state.local_node, NodeInfo),
            State
    end,

    %% Reschedule
    Timer = erlang:send_after(?REFRESH_INTERVAL, self(), refresh_registration),
    {noreply, NewState#state{refresh_timer = Timer}};

handle_info(cleanup_expired, State) ->
    %% Remove expired entries from the known nodes
    Now = erlang:system_time(second),
    NewNodes = maps:filter(
        fun(_NodeName, NodeInfo) ->
            is_entry_valid(NodeInfo, Now)
        end,
        State#state.nodes
    ),

    %% Notify subscribers about lost nodes
    LostNodes = maps:keys(State#state.nodes) -- maps:keys(NewNodes),
    lists:foreach(
        fun(NodeName) ->
            notify_subscribers({node_lost, NodeName}, State#state.subscribers)
        end,
        LostNodes
    ),

    %% Reschedule
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),
    {noreply, State#state{nodes = NewNodes, cleanup_timer = Timer}};

%% Handle mDNS discovery events
handle_info({mdns_node_discovered, NodeName, IP, Port}, State) ->
    NodeInfo = #{
        name => NodeName,
        port => Port,
        ip => IP,
        protocol => 'macula-dist',
        registered_at => erlang:system_time(second),
        ttl => ?DEFAULT_TTL,
        source => mdns
    },

    NewNodes = maps:put(NodeName, NodeInfo, State#state.nodes),
    notify_subscribers({node_discovered, NodeName, IP, Port}, State#state.subscribers),

    {noreply, State#state{nodes = NewNodes}};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Subscriber died, remove from list
    Subscribers = lists:delete(Pid, State#state.subscribers),
    {noreply, State#state{subscribers = Subscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    %% Unregister from the local cache
    case State#state.local_node of
        undefined -> ok;
        NodeName -> remove_from_local_cache(NodeName)
    end,

    %% Cancel timers
    cancel_timer(State#state.refresh_timer),
    cancel_timer(State#state.cleanup_timer),

    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions - Local Cache
%%%===================================================================

%% @private Store in local ETS cache
store_in_local_cache(NodeName, NodeInfo) ->
    ensure_local_cache(),
    ets:insert(macula_dist_discovery_cache, {NodeName, NodeInfo}),
    ok.

%% @private Remove from local ETS cache
remove_from_local_cache(NodeName) ->
    ensure_local_cache(),
    ets:delete(macula_dist_discovery_cache, NodeName),
    ok.

%% @private Look up in local ETS cache
lookup_in_local_cache(NodeName) ->
    ensure_local_cache(),
    case ets:lookup(macula_dist_discovery_cache, NodeName) of
        [{_, NodeInfo}] -> {ok, NodeInfo};
        [] -> {error, not_found}
    end.

%% @private Ensure local cache ETS table exists
ensure_local_cache() ->
    case ets:info(macula_dist_discovery_cache) of
        undefined ->
            ets:new(macula_dist_discovery_cache, [
                named_table,
                public,
                set,
                {read_concurrency, true}
            ]);
        _ ->
            ok
    end.

%%%===================================================================
%%% Internal Functions - mDNS
%%%===================================================================

%% @private Announce via mDNS when the mDNS application runs
maybe_announce_mdns(NodeName, Port) ->
    case whereis(mdns_advertise_sup) of
        undefined ->
            ok;
        _Pid ->
            macula_dist_mdns_advertiser:register(NodeName, Port),
            mdns_advertise_sup:start_child(macula_dist_mdns_advertiser),
            ok
    end.

%% @private Stop announcing via mDNS when the mDNS application runs
maybe_unannounce_mdns(_NodeName) ->
    case whereis(mdns_advertise_sup) of
        undefined ->
            ok;
        _Pid ->
            mdns_advertise:stop(macula_dist_mdns_advertiser),
            macula_dist_mdns_advertiser:unregister(),
            ok
    end.

%%%===================================================================
%%% Internal Functions - Utilities
%%%===================================================================

%% @private Check if entry is still valid
is_entry_valid(NodeInfo) ->
    is_entry_valid(NodeInfo, erlang:system_time(second)).

is_entry_valid(NodeInfo, Now) ->
    RegisteredAt = maps:get(registered_at, NodeInfo, 0),
    TTL = maps:get(ttl, NodeInfo, ?DEFAULT_TTL),
    (RegisteredAt + TTL * 2) > Now.  % Grace period of 2x TTL

%% @private Notify all subscribers of an event
notify_subscribers(Event, Subscribers) ->
    lists:foreach(
        fun(Pid) ->
            Pid ! Event
        end,
        Subscribers
    ).

%% @private Cancel timer if defined
cancel_timer(undefined) -> ok;
cancel_timer(Timer) -> erlang:cancel_timer(Timer).
