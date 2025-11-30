%%%-------------------------------------------------------------------
%%% @doc
%%% RPC Service Interests Module (Pull-based Discovery)
%%%
%%% Handles pull-based service discovery configuration:
%%% - Normalizes service interest lists (atoms, binaries, strings)
%%% - Validates service interest inputs
%%% - Prepares DHT lookup messages for prefetching
%%%
%%% Extracted from macula_rpc_handler.erl (Dec 2025) to improve testability
%%% and separation of concerns.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_service_interests).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    normalize/1,
    normalize_single/1,
    create_find_value_message/1,
    log_interests/1,
    merge_interests/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Normalize service interests to list of binaries.
%% Accepts various input formats: list of binaries/atoms/strings, or single value.
-spec normalize(term()) -> [binary()].
normalize(Interests) when is_list(Interests) ->
    lists:filtermap(fun normalize_single/1, Interests);
normalize(Interest) when is_binary(Interest), byte_size(Interest) > 0 ->
    [Interest];
normalize(Interest) when is_atom(Interest), Interest =/= undefined ->
    [atom_to_binary(Interest, utf8)];
normalize(Interest) when is_list(Interest) ->
    %% Could be a string
    case io_lib:printable_list(Interest) of
        true -> [list_to_binary(Interest)];
        false -> []
    end;
normalize(_) ->
    [].

%% @doc Normalize a single service interest to binary.
%% Returns {true, Binary} for valid inputs, false for invalid.
-spec normalize_single(term()) -> {true, binary()} | false.
normalize_single(Interest) when is_binary(Interest), byte_size(Interest) > 0 ->
    {true, Interest};
normalize_single(Interest) when is_atom(Interest), Interest =/= undefined ->
    {true, atom_to_binary(Interest, utf8)};
normalize_single(Interest) when is_list(Interest), Interest =/= [] ->
    case io_lib:printable_list(Interest) of
        true -> {true, list_to_binary(Interest)};
        false -> false
    end;
normalize_single(_) ->
    false.

%% @doc Create a FIND_VALUE message for a service.
%% Returns the service key and encoded message ready to send.
-spec create_find_value_message(binary()) -> {binary(), map()}.
create_find_value_message(ServiceName) ->
    ServiceKey = crypto:hash(sha256, ServiceName),
    FindValueMsg = macula_routing_protocol:encode_find_value(ServiceKey),
    {ServiceKey, FindValueMsg}.

%% @doc Log configured service interests during init.
-spec log_interests([binary()]) -> ok.
log_interests([]) ->
    ok;
log_interests(Interests) ->
    ?LOG_INFO("RPC handler configured with ~p service interest(s) for pull-based discovery: ~p",
              [length(Interests), Interests]).

%% @doc Merge new interests with existing ones (removes duplicates).
-spec merge_interests([binary()], [binary()]) -> [binary()].
merge_interests(Existing, New) ->
    lists:usort(Existing ++ New).
