%%%-------------------------------------------------------------------
%%% @doc
%%% RPC Failover Strategy Module
%%%
%%% Encapsulates failover logic for RPC calls:
%%% - Determines if a call should be retried after failure
%%% - Manages provider exclusion list (failed providers)
%%% - Tracks attempt counts against max attempts
%%% - Provides retry decisions based on failure type
%%%
%%% Extracted from macula_rpc_handler.erl (Dec 2025) to improve testability
%%% and separation of concerns.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_failover).

%% API
-export([
    new_context/4,
    can_retry/1,
    mark_provider_failed/2,
    get_available_providers/1,
    increment_attempt/1,
    get_attempt_count/1,
    should_failover/2
]).

%% Types
-export_type([failover_context/0, failure_reason/0]).

-type failure_reason() :: timeout | connection_error | provider_error | gateway_timeout.

-type failover_context() :: #{
    procedure := binary(),
    args := term(),
    opts := map(),
    providers := [map()],
    excluded := [binary()],
    attempt := pos_integer(),
    max_attempts := pos_integer(),
    tried_node_id => binary() | undefined
}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create a new failover context for an RPC call.
%% Used when initiating a call that might need failover.
%% max_attempts is capped at provider count (can't try more providers than exist).
-spec new_context(binary(), term(), map(), [map()]) -> failover_context().
new_context(Procedure, Args, Opts, Providers) ->
    ProviderCount = length(Providers),
    RequestedMax = maps:get(max_attempts, Opts, 3),
    MaxAttempts = min(RequestedMax, ProviderCount),
    #{
        procedure => Procedure,
        args => Args,
        opts => Opts,
        providers => Providers,
        excluded => [],
        attempt => 1,
        max_attempts => MaxAttempts
    }.

%% @doc Check if the call can be retried based on current context.
%% Returns true if there are available providers and max attempts not exceeded.
-spec can_retry(failover_context()) -> boolean().
can_retry(#{attempt := Attempt, max_attempts := MaxAttempts}) when Attempt > MaxAttempts ->
    false;
can_retry(Context) ->
    case get_available_providers(Context) of
        [] -> false;
        _ -> true
    end.

%% @doc Mark a provider as failed and add to exclusion list.
%% Returns updated context with provider added to excluded list.
-spec mark_provider_failed(binary() | undefined, failover_context()) -> failover_context().
mark_provider_failed(undefined, Context) ->
    Context;
mark_provider_failed(NodeId, #{excluded := Excluded} = Context) ->
    Context#{excluded := [NodeId | Excluded]}.

%% @doc Get list of providers that haven't been excluded.
%% Filters out all previously failed providers.
-spec get_available_providers(failover_context()) -> [map()].
get_available_providers(#{providers := Providers, excluded := []}) ->
    Providers;
get_available_providers(#{providers := Providers, excluded := Excluded}) ->
    lists:filter(fun(#{node_id := NodeId}) ->
        not lists:member(NodeId, Excluded)
    end, Providers).

%% @doc Increment the attempt counter.
%% Call this before each retry attempt.
-spec increment_attempt(failover_context()) -> failover_context().
increment_attempt(#{attempt := Attempt} = Context) ->
    Context#{attempt := Attempt + 1}.

%% @doc Get current attempt count.
-spec get_attempt_count(failover_context()) -> pos_integer().
get_attempt_count(#{attempt := Attempt}) ->
    Attempt.

%% @doc Determine if failover should be attempted for a given failure reason.
%% Some failures are not retryable (e.g., invalid procedure).
-spec should_failover(failure_reason(), failover_context()) -> boolean().
should_failover(timeout, Context) ->
    can_retry(Context);
should_failover(connection_error, Context) ->
    can_retry(Context);
should_failover(provider_error, Context) ->
    can_retry(Context);
should_failover(gateway_timeout, _Context) ->
    %% Gateway-direct calls have no failover
    false.
