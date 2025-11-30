%%%-------------------------------------------------------------------
%%% @doc
%%% RPC server managing registrations and calls.
%%% GenServer that integrates registry, cache, discovery, router, and executor.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_server).
-behaviour(gen_server).

%% API
-export([
    start_link/2,
    stop/1,
    register/4,
    unregister/3,
    call/4,
    list_registrations/1
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
-type config() :: #{
    routing_strategy => macula_rpc_router:strategy(),
    cache_enabled => boolean(),
    dht_lookup_fun => macula_rpc_dht:dht_lookup_fun(),
    send_fun => macula_rpc_executor:send_fun()
}.

-type state() :: #{
    local_node_id := binary(),
    registry := macula_rpc_registry:registry(),
    cache := macula_rpc_cache:cache(),
    router_state := macula_rpc_router:router_state(),
    config := config()
}.

-export_type([config/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start RPC server.
-spec start_link(binary(), config()) -> {ok, pid()} | {error, term()}.
start_link(LocalNodeId, Config) ->
    gen_server:start_link(?MODULE, {LocalNodeId, Config}, []).

%% @doc Stop RPC server.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Register procedure.
-spec register(pid(), binary(), macula_rpc_registry:handler_fn(), map()) -> ok.
register(Pid, Uri, Handler, Metadata) ->
    gen_server:call(Pid, {register, Uri, Handler, Metadata}).

%% @doc Unregister procedure.
-spec unregister(pid(), binary(), macula_rpc_registry:handler_fn()) -> ok.
unregister(Pid, Uri, Handler) ->
    gen_server:call(Pid, {unregister, Uri, Handler}).

%% @doc Synchronous call to procedure.
-spec call(pid(), binary(), map(), pos_integer()) -> {ok, term()} | {error, term()}.
call(Pid, Uri, Args, Timeout) ->
    gen_server:call(Pid, {call, Uri, Args, Timeout}, Timeout + 1000).

%% @doc List local registrations.
-spec list_registrations(pid()) -> [macula_rpc_registry:registration()].
list_registrations(Pid) ->
    gen_server:call(Pid, list_registrations).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

%% @doc Initialize server state.
init({LocalNodeId, Config}) ->
    %% Create initial state
    Registry = macula_rpc_registry:new(),
    Cache = macula_rpc_cache:new(1000),
    Strategy = maps:get(routing_strategy, Config, local_first),
    RouterState = macula_rpc_router:new_state(Strategy),

    State = #{
        local_node_id => LocalNodeId,
        registry => Registry,
        cache => Cache,
        router_state => RouterState,
        config => Config
    },

    {ok, State}.

%% @doc Handle synchronous calls.
handle_call({register, Uri, Handler, Metadata}, _From, State) ->
    #{registry := Registry} = State,

    %% Validate URI
    case macula_rpc_names:validate(Uri) of
        ok ->
            %% Add to registry
            NewRegistry = macula_rpc_registry:register(Registry, Uri, Handler, Metadata),
            NewState = State#{registry => NewRegistry},
            {reply, ok, NewState};

        {error, _Reason} = Error ->
            {reply, Error, State}
    end;

handle_call({unregister, Uri, Handler}, _From, State) ->
    #{registry := Registry} = State,

    %% Remove from registry
    NewRegistry = macula_rpc_registry:unregister(Registry, Uri, Handler),
    NewState = State#{registry => NewRegistry},

    {reply, ok, NewState};

handle_call({call, Uri, Args, Timeout}, _From, State) ->
    #{
        local_node_id := LocalNodeId,
        registry := Registry,
        cache := Cache,
        router_state := RouterState,
        config := Config
    } = State,

    %% Validate URI
    case macula_rpc_names:validate(Uri) of
        ok ->
            %% Execute call
            {Result, NewState} = execute_call(
                Uri, Args, Timeout, LocalNodeId, Registry, Cache, RouterState, Config
            ),
            {reply, Result, NewState};

        {error, _Reason} = Error ->
            {reply, Error, State}
    end;

handle_call(list_registrations, _From, State) ->
    #{registry := Registry} = State,
    Registrations = macula_rpc_registry:list_registrations(Registry),
    {reply, Registrations, State}.

%% @doc Handle asynchronous casts (none implemented).
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages (none expected).
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination.
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Execute RPC call (local or remote).
-spec execute_call(
    binary(), map(), pos_integer(), binary(),
    macula_rpc_registry:registry(),
    macula_rpc_cache:cache(),
    macula_rpc_router:router_state(),
    config()
) -> {{ok, term()} | {error, term()}, state()}.
execute_call(Uri, Args, Timeout, LocalNodeId, Registry, Cache, RouterState, Config) ->
    %% Check cache first (if enabled)
    CacheEnabled = maps:get(cache_enabled, Config, false),

    case try_cache_get(Uri, Args, Cache, CacheEnabled) of
        {ok, Result, NewCache} ->
            %% Cache hit
            NewState = #{
                local_node_id => LocalNodeId,
                registry => Registry,
                cache => NewCache,
                router_state => RouterState,
                config => Config
            },
            {{ok, Result}, NewState};

        not_found ->
            %% Cache miss, execute call
            execute_call_no_cache(
                Uri, Args, Timeout, LocalNodeId, Registry, Cache, RouterState, Config
            )
    end.

%% @doc Try to get result from cache.
-spec try_cache_get(binary(), map(), macula_rpc_cache:cache(), boolean()) ->
    {ok, term(), macula_rpc_cache:cache()} | not_found.
try_cache_get(_Uri, _Args, _Cache, false) ->
    %% Cache disabled
    not_found;
try_cache_get(Uri, Args, Cache, true) ->
    %% Try cache
    macula_rpc_cache:get(Cache, Uri, Args).

%% @doc Execute call without cache.
-spec execute_call_no_cache(
    binary(), map(), pos_integer(), binary(),
    macula_rpc_registry:registry(),
    macula_rpc_cache:cache(),
    macula_rpc_router:router_state(),
    config()
) -> {{ok, term()} | {error, term()}, state()}.
execute_call_no_cache(Uri, Args, Timeout, LocalNodeId, Registry, Cache, RouterState, Config) ->
    %% Find local handlers
    LocalHandlers = macula_rpc_registry:find(Registry, Uri),

    %% Find remote providers
    DhtLookupFun = maps:get(dht_lookup_fun, Config, fun default_dht_lookup/1),
    RemoteProviders = unwrap_providers(DhtLookupFun(Uri)),

    %% Route call
    Strategy = maps:get(routing_strategy, Config, local_first),
    {ProviderResult, NewRouterState} = route_call(
        Strategy, LocalHandlers, RemoteProviders, RouterState
    ),

    %% Execute based on routing result
    case ProviderResult of
        {local, Registration} ->
            %% Execute local handler
            Handler = maps:get(handler, Registration),
            Result = macula_rpc_executor:execute_local(Handler, Args, Timeout),

            %% Cache result if enabled and successful
            NewCache = maybe_cache_result(Uri, Args, Result, Registration, Cache, Config),

            NewState = #{
                local_node_id => LocalNodeId,
                registry => Registry,
                cache => NewCache,
                router_state => NewRouterState,
                config => Config
            },
            {Result, NewState};

        {remote, Provider} ->
            %% Execute remote call
            SendFun = maps:get(send_fun, Config, fun default_send/4),
            Result = macula_rpc_executor:execute_remote(Uri, Args, Provider, SendFun, Timeout),

            %% Don't cache remote results (for now)
            NewState = #{
                local_node_id => LocalNodeId,
                registry => Registry,
                cache => Cache,
                router_state => NewRouterState,
                config => Config
            },
            {Result, NewState};

        {error, no_provider} ->
            %% No provider available
            NewState = #{
                local_node_id => LocalNodeId,
                registry => Registry,
                cache => Cache,
                router_state => NewRouterState,
                config => Config
            },
            {{error, no_provider}, NewState}
    end.

%% @doc Route call to provider.
-spec route_call(
    macula_rpc_router:strategy(),
    [macula_rpc_registry:registration()],
    [macula_rpc_dht:provider_info()],
    macula_rpc_router:router_state()
) -> {
    {local, macula_rpc_registry:registration()} |
    {remote, macula_rpc_dht:provider_info()} |
    {error, no_provider},
    macula_rpc_router:router_state()
}.
route_call(round_robin, LocalHandlers, RemoteProviders, RouterState) ->
    %% Round robin requires stateful API
    macula_rpc_router:select_provider_stateful(RouterState, LocalHandlers, RemoteProviders);

route_call(Strategy, LocalHandlers, RemoteProviders, RouterState) ->
    %% Other strategies are stateless
    Result = macula_rpc_router:select_provider(Strategy, LocalHandlers, RemoteProviders),
    {Result, RouterState}.

%% @doc Cache result if caching enabled and result is successful.
-spec maybe_cache_result(
    binary(), map(),
    {ok, term()} | {error, term()},
    macula_rpc_registry:registration(),
    macula_rpc_cache:cache(),
    config()
) -> macula_rpc_cache:cache().
maybe_cache_result(Uri, Args, {ok, Result}, Registration, Cache, Config) ->
    CacheEnabled = maps:get(cache_enabled, Config, false),
    Metadata = maps:get(metadata, Registration),
    CacheTTL = maps:get(cache_ttl, Metadata, 0),
    cache_if_enabled(CacheEnabled andalso CacheTTL > 0, Cache, Uri, Args, Result, CacheTTL);

maybe_cache_result(_Uri, _Args, {error, _Reason}, _Registration, Cache, _Config) ->
    %% Don't cache errors
    Cache.

%% @doc Cache result if caching is enabled.
cache_if_enabled(true, Cache, Uri, Args, Result, CacheTTL) ->
    %% Cache result with TTL
    macula_rpc_cache:put(Cache, Uri, Args, Result, CacheTTL);
cache_if_enabled(false, Cache, _Uri, _Args, _Result, _CacheTTL) ->
    %% Don't cache
    Cache.

%% @doc Unwrap provider list from ok/error result.
unwrap_providers({ok, Providers}) -> Providers;
unwrap_providers({error, _}) -> [].

%% @doc Default DHT lookup (returns empty list - for testing).
-spec default_dht_lookup(binary()) -> {ok, [macula_rpc_dht:provider_info()]}.
default_dht_lookup(_Uri) ->
    {ok, []}.

%% @doc Default send function (returns error - for testing).
-spec default_send(binary(), map(), macula_rpc_dht:address(), pos_integer()) ->
    {ok, term()} | {error, term()}.
default_send(_Uri, _Args, _Address, _Timeout) ->
    {error, no_transport}.
