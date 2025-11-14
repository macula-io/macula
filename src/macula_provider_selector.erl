%%%-------------------------------------------------------------------
%%% @doc
%%% Provider selection strategies for multi-provider RPC load balancing.
%%%
%%% Supports multiple strategies for choosing which provider to use
%%% when multiple providers advertise the same service.
%%%
%%% Strategies:
%%% - round_robin: Distribute calls evenly across providers
%%% - random: Random provider selection
%%% - first: Always use first provider (default/simple)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_provider_selector).

%% API exports
-export([
    select_provider/2,
    select_provider/3
]).

-export_type([
    strategy/0,
    provider_info/0,
    selection_state/0
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type strategy() :: round_robin | random | first.
%% Selection strategy for choosing a provider from multiple options.

-type provider_info() :: #{
    node_id := binary(),
    endpoint := binary(),
    metadata := map(),
    advertised_at => integer(),
    ttl => pos_integer()
}.
%% Provider information returned from DHT.

-type selection_state() :: #{
    strategy => strategy(),
    counters => #{binary() => non_neg_integer()}  % Service ID -> call counter for round-robin
}.
%% State maintained by the selector for stateful strategies.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Select a provider from a list using the default strategy (random).
%%
%% Returns the selected provider or error if list is empty.
-spec select_provider([provider_info()], selection_state()) ->
    {ok, provider_info(), selection_state()} | {error, no_providers}.
select_provider(Providers, State) ->
    select_provider(Providers, random, State).

%% @doc Select a provider from a list using a specific strategy.
%%
%% Strategies:
%% - first: Always select the first provider (simple, no state)
%% - random: Randomly select a provider
%% - round_robin: Distribute calls evenly using a counter
%%
%% Examples:
%% ```
%% %% Random selection
%% State = new_state(random),
%% {ok, Provider, State2} = select_provider(Providers, random, State).
%%
%% %% Round-robin selection
%% State = new_state(round_robin),
%% {ok, P1, State2} = select_provider(Providers, round_robin, State),
%% {ok, P2, State3} = select_provider(Providers, round_robin, State2),
%% %% P1 and P2 will be different providers (if multiple available)
%% '''
-spec select_provider([provider_info()], strategy(), selection_state()) ->
    {ok, provider_info(), selection_state()} | {error, no_providers}.
select_provider([], _Strategy, _State) ->
    {error, no_providers};

select_provider(Providers, first, State) when is_list(Providers) ->
    %% Simple: always pick first provider
    [Provider | _] = Providers,
    {ok, Provider, State};

select_provider(Providers, random, State) when is_list(Providers) ->
    %% Random selection
    Index = rand:uniform(length(Providers)),
    Provider = lists:nth(Index, Providers),
    {ok, Provider, State};

select_provider(Providers, round_robin, State) when is_list(Providers) ->
    %% Round-robin: need service_id to track counter
    %% Extract service_id from state or use a default key
    ServiceId = maps:get(current_service_id, State, <<"default">>),

    %% Get current counter for this service
    Counters = maps:get(counters, State, #{}),
    Counter = maps:get(ServiceId, Counters, 0),

    %% Select provider based on counter
    Index = (Counter rem length(Providers)) + 1,
    Provider = lists:nth(Index, Providers),

    %% Update counter
    NewCounter = Counter + 1,
    NewCounters = Counters#{ServiceId => NewCounter},
    NewState = State#{counters => NewCounters},

    {ok, Provider, NewState}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Note: State management for round-robin is handled by macula_connection.erl
%% which maintains the counters in its #state.provider_selector_state field.
