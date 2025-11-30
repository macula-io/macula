%%%-------------------------------------------------------------------
%%% @doc
%%% RPC call routing strategies.
%%% Selects which provider to use for a call (local or remote).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_router).

%% API
-export([
    select_provider/3,
    select_provider_stateful/3,
    select_provider_closest/3,
    select_local/1,
    select_remote_random/1,
    new_state/1
]).

%% Types
-type strategy() :: local_first | round_robin | random | closest.
-type registration() :: macula_rpc_registry:registration().
-type provider_info() :: macula_rpc_dht:provider_info().

-type router_state() :: #{
    strategy := strategy(),
    round_robin_index := non_neg_integer()
}.

-export_type([strategy/0, router_state/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create new router state.
-spec new_state(strategy()) -> router_state().
new_state(Strategy) ->
    #{
        strategy => Strategy,
        round_robin_index => 0
    }.

%% @doc Select provider using stateless strategy.
%% For local_first, random, and closest strategies.
-spec select_provider(strategy(), [registration()], [provider_info()]) ->
    {local, registration()} | {remote, provider_info()} | {error, no_provider}.
select_provider(local_first, LocalHandlers, RemoteProviders) ->
    LocalResult = select_local(LocalHandlers),
    do_local_first(LocalResult, RemoteProviders);

select_provider(random, _LocalHandlers, RemoteProviders) ->
    RemoteResult = select_remote_random(RemoteProviders),
    wrap_remote_result(RemoteResult);

select_provider(round_robin, _LocalHandlers, _RemoteProviders) ->
    %% Round robin requires state, use select_provider_stateful instead
    {error, use_stateful_api};

select_provider(closest, _LocalHandlers, _RemoteProviders) ->
    %% Closest requires local node ID, use select_provider_closest instead
    {error, use_closest_api}.

%% @doc Select provider using stateful strategy (for round_robin).
-spec select_provider_stateful(router_state(), [registration()], [provider_info()]) ->
    {{local, registration()} | {remote, provider_info()} | {error, no_provider}, router_state()}.
select_provider_stateful(#{strategy := round_robin, round_robin_index := Index} = State, _LocalHandlers, RemoteProviders) ->
    case RemoteProviders of
        [] ->
            {{error, no_provider}, State};

        _ ->
            %% Select provider at current index
            ProviderIndex = Index rem length(RemoteProviders),
            Provider = lists:nth(ProviderIndex + 1, RemoteProviders),

            %% Increment index for next call
            NewState = State#{round_robin_index => Index + 1},

            {{remote, Provider}, NewState}
    end;

select_provider_stateful(#{strategy := Strategy} = State, LocalHandlers, RemoteProviders) ->
    %% For non-stateful strategies, use stateless version
    Result = select_provider(Strategy, LocalHandlers, RemoteProviders),
    {Result, State}.

%% @doc Select provider using closest strategy (requires local node ID).
-spec select_provider_closest(binary(), [registration()], [provider_info()]) ->
    {local, registration()} | {remote, provider_info()} | {error, no_provider}.
select_provider_closest(LocalNodeId, LocalHandlers, RemoteProviders) ->
    LocalResult = select_local(LocalHandlers),
    do_closest(LocalResult, LocalNodeId, RemoteProviders).

%% @private Local handler found
do_local_first({ok, Handler}, _RemoteProviders) ->
    {local, Handler};
%% @private No local handler - try remote
do_local_first(not_found, RemoteProviders) ->
    RemoteResult = select_remote_random(RemoteProviders),
    wrap_remote_result(RemoteResult).

%% @private Local handler found
do_closest({ok, Handler}, _LocalNodeId, _RemoteProviders) ->
    {local, Handler};
%% @private No local handler - try closest remote
do_closest(not_found, LocalNodeId, RemoteProviders) ->
    ClosestResult = find_closest_provider(LocalNodeId, RemoteProviders),
    wrap_remote_result(ClosestResult).

%% @private Wrap remote provider result
wrap_remote_result({ok, Provider}) -> {remote, Provider};
wrap_remote_result(not_found) -> {error, no_provider}.

%% @doc Select local handler (returns first one).
-spec select_local([registration()]) -> {ok, registration()} | not_found.
select_local([]) ->
    not_found;
select_local([Handler | _Rest]) ->
    {ok, Handler}.

%% @doc Select random remote provider.
-spec select_remote_random([provider_info()]) -> {ok, provider_info()} | not_found.
select_remote_random([]) ->
    not_found;
select_remote_random(Providers) ->
    Index = rand:uniform(length(Providers)),
    Provider = lists:nth(Index, Providers),
    {ok, Provider}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Find provider with smallest XOR distance to local node.
-spec find_closest_provider(binary(), [provider_info()]) -> {ok, provider_info()} | not_found.
find_closest_provider(_LocalNodeId, []) ->
    not_found;
find_closest_provider(LocalNodeId, Providers) ->
    %% Calculate XOR distances
    ProvidersWithDistance = lists:map(
        fun(Provider) ->
            RemoteNodeId = maps:get(node_id, Provider),
            Distance = xor_distance(LocalNodeId, RemoteNodeId),
            {Distance, Provider}
        end,
        Providers
    ),

    %% Sort by distance (ascending)
    Sorted = lists:sort(ProvidersWithDistance),

    %% Return closest
    {_Distance, Closest} = hd(Sorted),
    {ok, Closest}.

%% @doc Calculate XOR distance between two node IDs.
-spec xor_distance(binary(), binary()) -> non_neg_integer().
xor_distance(A, B) ->
    <<AInt:256>> = A,
    <<BInt:256>> = B,
    AInt bxor BInt.
