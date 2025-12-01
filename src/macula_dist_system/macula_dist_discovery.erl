%%%-------------------------------------------------------------------
%%% @doc EPMD Replacement using Macula DHT Discovery.
%%%
%%% This module provides decentralized node discovery for Erlang distribution,
%%% replacing the centralized EPMD (Erlang Port Mapper Daemon).
%%%
%%% == How it Works ==
%%%
%%% Instead of registering with a local EPMD daemon on port 4369,
%%% nodes announce themselves via Macula's DHT (Distributed Hash Table):
%%%
%%% 1. On startup, nodes call register_node/2 to announce themselves
%%% 2. Other nodes find peers via lookup_node/1 which queries the DHT
%%% 3. Subscribers get notified of node join/leave events
%%%
%%% == Discovery Mechanisms ==
%%%
%%% - **mDNS**: For local network discovery (no bootstrap required)
%%% - **DHT**: For internet-scale discovery (requires bootstrap nodes)
%%% - **Bootstrap**: Initial DHT seeds from known nodes
%%%
%%% == Usage ==
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
-define(DHT_PREFIX, <<"_dist.node.">>).
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

    %% Discovery type
    discovery_type :: mdns | dht | both,

    %% Timers
    refresh_timer :: reference() | undefined,
    cleanup_timer :: reference() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the discovery server with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the discovery server with options.
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

    DiscoveryType = maps:get(discovery_type, Opts, both),

    State = #state{
        nodes = #{},
        subscribers = [],
        discovery_type = DiscoveryType
    },

    %% Start refresh timer
    RefreshTimer = erlang:send_after(?REFRESH_INTERVAL, self(), refresh_registration),

    %% Start cleanup timer
    CleanupTimer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),

    %% Subscribe to Macula DHT events if available
    maybe_subscribe_to_dht(),

    {ok, State#state{
        refresh_timer = RefreshTimer,
        cleanup_timer = CleanupTimer
    }}.

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

    %% Store in DHT
    ok = store_in_dht(NodeName, NodeInfo),

    %% Also announce via mDNS if enabled
    maybe_announce_mdns(NodeName, Port, State),

    {reply, ok, State#state{
        local_node = NodeName,
        local_port = Port,
        local_info = NodeInfo
    }};

handle_call({unregister_node, NodeName}, _From, State) ->
    %% Remove from DHT
    ok = remove_from_dht(NodeName),

    %% Remove from mDNS if enabled
    maybe_unannounce_mdns(NodeName, State),

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
    %% First check local cache
    case maps:get(NodeName, State#state.nodes, undefined) of
        undefined ->
            %% Query DHT
            case lookup_in_dht(NodeName) of
                {ok, NodeInfo} ->
                    %% Cache the result
                    NewNodes = maps:put(NodeName, NodeInfo, State#state.nodes),
                    {reply, {ok, NodeInfo}, State#state{nodes = NewNodes}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        NodeInfo ->
            %% Check if cached entry is still valid
            case is_entry_valid(NodeInfo) of
                true ->
                    {reply, {ok, NodeInfo}, State};
                false ->
                    %% Expired, refresh from DHT
                    case lookup_in_dht(NodeName) of
                        {ok, FreshInfo} ->
                            NewNodes = maps:put(NodeName, FreshInfo, State#state.nodes),
                            {reply, {ok, FreshInfo}, State#state{nodes = NewNodes}};
                        {error, Reason} ->
                            NewNodes = maps:remove(NodeName, State#state.nodes),
                            {reply, {error, Reason}, State#state{nodes = NewNodes}}
                    end
            end
    end;

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

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(refresh_registration, State) ->
    %% Refresh our registration in DHT
    NewState = case State#state.local_info of
        undefined ->
            State;
        NodeInfo ->
            ok = store_in_dht(State#state.local_node, NodeInfo),
            State
    end,

    %% Reschedule
    Timer = erlang:send_after(?REFRESH_INTERVAL, self(), refresh_registration),
    {noreply, NewState#state{refresh_timer = Timer}};

handle_info(cleanup_expired, State) ->
    %% Remove expired entries from cache
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

%% Handle DHT discovery events
handle_info({dht_node_discovered, NodeName, NodeInfo}, State) ->
    %% Update cache
    NewNodes = maps:put(NodeName, NodeInfo, State#state.nodes),

    %% Notify subscribers
    #{ip := IP, port := Port} = NodeInfo,
    notify_subscribers({node_discovered, NodeName, IP, Port}, State#state.subscribers),

    {noreply, State#state{nodes = NewNodes}};

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
    %% Unregister from DHT
    case State#state.local_node of
        undefined -> ok;
        NodeName -> remove_from_dht(NodeName)
    end,

    %% Cancel timers
    cancel_timer(State#state.refresh_timer),
    cancel_timer(State#state.cleanup_timer),

    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions - DHT Operations
%%%===================================================================

%% @private Store node info in DHT
store_in_dht(NodeName, NodeInfo) ->
    Key = make_dht_key(NodeName),

    %% Try to use Macula's DHT if available
    case whereis(macula_routing_dht) of
        undefined ->
            %% Fallback: store in local ETS for testing
            store_in_local_cache(NodeName, NodeInfo);
        _Pid ->
            %% Use Macula DHT
            try
                macula_routing_dht:store(Key, term_to_binary(NodeInfo)),
                ok
            catch
                _:_ ->
                    store_in_local_cache(NodeName, NodeInfo)
            end
    end.

%% @private Remove node info from DHT
remove_from_dht(NodeName) ->
    Key = make_dht_key(NodeName),

    case whereis(macula_routing_dht) of
        undefined ->
            remove_from_local_cache(NodeName);
        _Pid ->
            try
                macula_routing_dht:delete(Key),
                ok
            catch
                _:_ ->
                    remove_from_local_cache(NodeName)
            end
    end.

%% @private Look up node info in DHT
lookup_in_dht(NodeName) ->
    Key = make_dht_key(NodeName),

    case whereis(macula_routing_dht) of
        undefined ->
            lookup_in_local_cache(NodeName);
        _Pid ->
            try
                case macula_routing_dht:find(Key) of
                    {ok, BinInfo} ->
                        {ok, binary_to_term(BinInfo)};
                    {error, not_found} ->
                        lookup_in_local_cache(NodeName)
                end
            catch
                _:_ ->
                    lookup_in_local_cache(NodeName)
            end
    end.

%% @private Make DHT key for node
make_dht_key(NodeName) when is_atom(NodeName) ->
    <<?DHT_PREFIX/binary, (atom_to_binary(NodeName, utf8))/binary>>;
make_dht_key(NodeName) when is_list(NodeName) ->
    <<?DHT_PREFIX/binary, (list_to_binary(NodeName))/binary>>.

%%%===================================================================
%%% Internal Functions - Local Cache (Fallback)
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

%% @private Maybe announce via mDNS
maybe_announce_mdns(_NodeName, _Port, #state{discovery_type = dht}) ->
    ok;
maybe_announce_mdns(NodeName, Port, _State) ->
    %% Try to use mDNS if available (shortishly/mdns library)
    case whereis(mdns_advertise_sup) of
        undefined ->
            ok;
        _Pid ->
            try
                %% Register instance info for the advertiser
                macula_dist_mdns_advertiser:register(NodeName, Port),
                %% Start the advertiser child
                mdns_advertise_sup:start_child(macula_dist_mdns_advertiser),
                ok
            catch
                _:_ -> ok
            end
    end.

%% @private Maybe unannounce via mDNS
maybe_unannounce_mdns(_NodeName, #state{discovery_type = dht}) ->
    ok;
maybe_unannounce_mdns(_NodeName, _State) ->
    case whereis(mdns_advertise_sup) of
        undefined ->
            ok;
        _Pid ->
            try
                %% Stop the advertiser (sends TTL=0 announcement)
                mdns_advertise:stop(macula_dist_mdns_advertiser),
                %% Unregister instance info
                macula_dist_mdns_advertiser:unregister(),
                ok
            catch
                _:_ -> ok
            end
    end.

%%%===================================================================
%%% Internal Functions - DHT Subscription
%%%===================================================================

%% @private Subscribe to DHT events
maybe_subscribe_to_dht() ->
    case whereis(macula_routing_dht) of
        undefined ->
            ok;
        _Pid ->
            try
                %% Subscribe to nodes with _dist.node. prefix
                macula_routing_dht:subscribe(?DHT_PREFIX, self())
            catch
                _:_ -> ok
            end
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
