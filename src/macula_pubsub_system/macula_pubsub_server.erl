%%%-------------------------------------------------------------------
%%% @doc
%%% Pub/Sub GenServer - manages subscriptions and message delivery.
%%% Ties together registry, cache, discovery, and delivery layers.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_server).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    subscribe/4,
    unsubscribe/3,
    publish/2,
    list_subscriptions/1,
    list_patterns/1,
    subscription_count/1,
    cache_stats/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% Types
-record(state, {
    registry :: macula_pubsub_registry:registry(),
    cache :: macula_pubsub_cache:cache(),
    cache_ttl :: pos_integer(),
    discovery_fun :: macula_pubsub_discovery:dht_lookup_fun() | undefined,
    send_fun :: macula_pubsub_delivery:send_fun() | undefined
}).

-type options() :: #{
    cache_size => pos_integer(),
    cache_ttl => pos_integer(),
    discovery_fun => macula_pubsub_discovery:dht_lookup_fun(),
    send_fun => macula_pubsub_delivery:send_fun()
}.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start server with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start server with options.
-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% @doc Stop server.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Subscribe to a pattern.
-spec subscribe(pid(), binary(), binary(), pid()) -> ok.
subscribe(Pid, SubscriberId, Pattern, Callback) ->
    gen_server:call(Pid, {subscribe, SubscriberId, Pattern, Callback}).

%% @doc Unsubscribe from a pattern.
-spec unsubscribe(pid(), binary(), binary()) -> ok.
unsubscribe(Pid, SubscriberId, Pattern) ->
    gen_server:call(Pid, {unsubscribe, SubscriberId, Pattern}).

%% @doc Publish message to all matching subscribers.
-spec publish(pid(), macula_pubsub_delivery:message()) -> ok.
publish(Pid, Message) ->
    gen_server:cast(Pid, {publish, Message}).

%% @doc List all subscriptions.
-spec list_subscriptions(pid()) -> [macula_pubsub_registry:subscription()].
list_subscriptions(Pid) ->
    gen_server:call(Pid, list_subscriptions).

%% @doc List all unique patterns.
-spec list_patterns(pid()) -> [binary()].
list_patterns(Pid) ->
    gen_server:call(Pid, list_patterns).

%% @doc Get subscription count.
-spec subscription_count(pid()) -> non_neg_integer().
subscription_count(Pid) ->
    gen_server:call(Pid, subscription_count).

%% @doc Get cache statistics.
-spec cache_stats(pid()) -> #{size := non_neg_integer(), max_size := pos_integer()}.
cache_stats(Pid) ->
    gen_server:call(Pid, cache_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Options) ->
    %% Extract options
    CacheSize = maps:get(cache_size, Options, 1000),
    CacheTTL = maps:get(cache_ttl, Options, 300),
    DiscoveryFun = maps:get(discovery_fun, Options, undefined),
    SendFun = maps:get(send_fun, Options, undefined),

    %% Initialize state
    State = #state{
        registry = macula_pubsub_registry:new(),
        cache = macula_pubsub_cache:new(CacheSize),
        cache_ttl = CacheTTL,
        discovery_fun = DiscoveryFun,
        send_fun = SendFun
    },

    {ok, State}.

%% @private
handle_call({subscribe, SubscriberId, Pattern, Callback}, _From, State) ->
    #state{registry = Registry} = State,

    NewRegistry = macula_pubsub_registry:subscribe(Registry, SubscriberId, Pattern, Callback),

    {reply, ok, State#state{registry = NewRegistry}};

handle_call({unsubscribe, SubscriberId, Pattern}, _From, State) ->
    #state{registry = Registry} = State,

    NewRegistry = macula_pubsub_registry:unsubscribe(Registry, SubscriberId, Pattern),

    {reply, ok, State#state{registry = NewRegistry}};

handle_call(list_subscriptions, _From, State) ->
    #state{registry = Registry} = State,

    %% Get all subscriptions from registry
    Subscriptions = get_all_subscriptions(Registry),

    {reply, Subscriptions, State};

handle_call(list_patterns, _From, State) ->
    #state{registry = Registry} = State,

    Patterns = macula_pubsub_registry:list_patterns(Registry),

    {reply, Patterns, State};

handle_call(subscription_count, _From, State) ->
    #state{registry = Registry} = State,

    Count = macula_pubsub_registry:size(Registry),

    {reply, Count, State};

handle_call(cache_stats, _From, State) ->
    #state{cache = Cache} = State,

    Stats = #{
        size => macula_pubsub_cache:size(Cache),
        max_size => macula_pubsub_cache:max_size(Cache)
    },

    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({publish, Message}, State) ->
    #state{
        registry = Registry,
        discovery_fun = DiscoveryFun,
        send_fun = SendFun
    } = State,

    %% Create discovery function (use cache if available)
    DiscoveryFn = get_discovery_fn(DiscoveryFun),

    %% Create send function (default: no-op)
    SendFn = get_send_fn(SendFun),

    %% Publish to local and remote
    _Results = macula_pubsub_delivery:publish(Message, Registry, DiscoveryFn, SendFn),

    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Get all subscriptions from registry.
%% NOTE: This extracts subscriptions from the registry internal structure.
-spec get_all_subscriptions(macula_pubsub_registry:registry()) ->
    [macula_pubsub_registry:subscription()].
get_all_subscriptions(#{subscriptions := Subs}) ->
    Subs.

%% @doc Get discovery function or default.
get_discovery_fn(undefined) -> fun(_Pattern) -> {ok, []} end;
get_discovery_fn(Fun) -> Fun.

%% @doc Get send function or default.
get_send_fn(undefined) -> fun(_Msg, _Addr) -> ok end;
get_send_fn(Fn) -> Fn.
