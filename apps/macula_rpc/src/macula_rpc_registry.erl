%%%-------------------------------------------------------------------
%%% @doc
%%% Local RPC procedure registration registry.
%%% Maps URIs to handler functions.
%%% Supports multiple handlers per URI (for load balancing).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_registry).

%% API
-export([
    new/0,
    new/1,
    register/4,
    unregister/3,
    find_handlers/2,
    list_uris/1,
    list_registrations/1,
    size/1
]).

%% Types
-type handler_fn() :: fun((map()) -> {ok, term()} | {error, term()}).

-type registration() :: #{
    uri := binary(),
    handler := handler_fn(),
    metadata := map()
}.

-type invocation_strategy() :: round_robin | random | local_first.

-type registry() :: #{
    registrations := [registration()],
    strategy := invocation_strategy()
}.

-export_type([
    handler_fn/0,
    registration/0,
    registry/0,
    invocation_strategy/0
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create new empty registry with default strategy (round_robin).
-spec new() -> registry().
new() ->
    new(round_robin).

%% @doc Create new empty registry with custom strategy.
-spec new(invocation_strategy()) -> registry().
new(Strategy) ->
    #{
        registrations => [],
        strategy => Strategy
    }.

%% @doc Register a procedure handler.
%% Allows multiple handlers for the same URI (for load balancing).
-spec register(registry(), binary(), handler_fn(), map()) -> registry().
register(#{registrations := Regs} = Registry, Uri, Handler, Metadata) ->
    Registration = #{
        uri => Uri,
        handler => Handler,
        metadata => Metadata
    },

    NewRegs = [Registration | Regs],
    Registry#{registrations => NewRegs}.

%% @doc Unregister a specific procedure handler.
%% Only removes the exact handler function.
-spec unregister(registry(), binary(), handler_fn()) -> registry().
unregister(#{registrations := Regs} = Registry, Uri, Handler) ->
    NewRegs = lists:filter(
        fun(Reg) ->
            not (maps:get(uri, Reg) =:= Uri andalso
                 maps:get(handler, Reg) =:= Handler)
        end,
        Regs
    ),
    Registry#{registrations => NewRegs}.

%% @doc Find all handlers for a URI.
-spec find_handlers(registry(), binary()) -> {ok, [registration()]} | not_found.
find_handlers(#{registrations := Regs}, Uri) ->
    Matches = lists:filter(
        fun(Reg) -> maps:get(uri, Reg) =:= Uri end,
        Regs
    ),

    case Matches of
        [] -> not_found;
        _ -> {ok, Matches}
    end.

%% @doc List all unique URIs in registry.
-spec list_uris(registry()) -> [binary()].
list_uris(#{registrations := Regs}) ->
    Uris = lists:map(
        fun(Reg) -> maps:get(uri, Reg) end,
        Regs
    ),
    lists:usort(Uris).

%% @doc List all registrations.
-spec list_registrations(registry()) -> [registration()].
list_registrations(#{registrations := Regs}) ->
    Regs.

%% @doc Get total number of registrations.
-spec size(registry()) -> non_neg_integer().
size(#{registrations := Regs}) ->
    length(Regs).
