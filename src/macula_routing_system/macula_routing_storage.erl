%%%-------------------------------------------------------------------
%%% @doc
%%% Pure functional helpers for DHT provider storage operations.
%%% Extracted from macula_routing_server to reduce module size.
%%% Handles provider lists, deduplication, expiry, and ETS mirroring.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_storage).

%% Provider list operations
-export([
    ensure_provider_list/1,
    upsert_provider/3,
    deduplicate_providers/1,
    get_node_id_from_value/1,
    find_provider_index/2,
    format_storage_value/1,
    delete_provider_from_storage/3,
    is_provider_expired/2
]).

%% ETS mirroring
-export([
    mirror_to_ets/3,
    mirror_delete_to_ets/3
]).

%% Formatting
-export([
    format_node_id/1,
    get_node_endpoint/1
]).

%% DHT value expiry: values with TTL older than this are expired (default 5 minutes + 60s grace)
-define(VALUE_MAX_AGE_MS, 360000).

%%%===================================================================
%%% Provider List Operations
%%%===================================================================

%% @doc Ensure value is a list for multi-provider storage.
-spec ensure_provider_list(term()) -> [term()].
ensure_provider_list(List) when is_list(List) -> List;
ensure_provider_list(Value) -> [Value].

%% @doc Insert or update a provider in the list.
%% Stamps every entry with stored_at for expiry tracking.
-spec upsert_provider(binary() | undefined, map(), [map()]) -> [map()].
upsert_provider(undefined, Value, ProviderList) ->
    [Value#{stored_at => erlang:system_time(millisecond)} | ProviderList];
upsert_provider(NodeId, Value, ProviderList) ->
    Stamped = Value#{stored_at => erlang:system_time(millisecond)},
    upsert_by_index(find_provider_index(NodeId, ProviderList), Stamped, ProviderList).

%% @doc Deduplicate providers by node_id -- keeps first occurrence.
-spec deduplicate_providers([map()]) -> [map()].
deduplicate_providers(Providers) ->
    lists:foldl(fun(P, Acc) ->
        NId = get_node_id_from_value(P),
        case lists:any(fun(A) -> get_node_id_from_value(A) =:= NId end, Acc) of
            true -> Acc;
            false -> [P | Acc]
        end
    end, [], Providers).

%% @doc Extract node_id from value map, handling both atom and binary keys.
-spec get_node_id_from_value(map()) -> binary() | undefined.
get_node_id_from_value(#{node_id := NodeId}) -> NodeId;
get_node_id_from_value(#{<<"node_id">> := NodeId}) -> NodeId;
get_node_id_from_value(_Value) -> undefined.

%% @doc Find index of provider with matching node_id in provider list.
-spec find_provider_index(binary(), [map()]) -> pos_integer() | not_found.
find_provider_index(NodeId, ProviderList) ->
    find_provider_index(NodeId, ProviderList, 1).

%% @doc Format storage value for get_local response.
-spec format_storage_value(undefined | term()) -> not_found | {ok, [term()]}.
format_storage_value(undefined) -> not_found;
format_storage_value(Value) when is_list(Value) -> {ok, Value};
format_storage_value(Value) -> {ok, [Value]}.

%% @doc Delete a specific provider from storage by node_id.
-spec delete_provider_from_storage(binary(), binary(), map()) -> map().
delete_provider_from_storage(Key, NodeId, Storage) ->
    case maps:get(Key, Storage, undefined) of
        undefined -> Storage;
        Providers when is_list(Providers) ->
            delete_provider_by_node_id(Key, NodeId, Providers, Storage);
        _SingleValue -> maps:remove(Key, Storage)
    end.

%% @doc Check if a provider entry is expired based on its stored_at timestamp.
-spec is_provider_expired(map(), integer()) -> boolean().
is_provider_expired(#{stored_at := StoredAt}, Now) ->
    (Now - StoredAt) > ?VALUE_MAX_AGE_MS;
is_provider_expired(#{<<"stored_at">> := StoredAt}, Now) ->
    (Now - StoredAt) > ?VALUE_MAX_AGE_MS;
is_provider_expired(#{ttl := _TTL}, _Now) ->
    true;
is_provider_expired(#{<<"ttl">> := _TTL}, _Now) ->
    true;
is_provider_expired(_Provider, _Now) ->
    false.

%%%===================================================================
%%% ETS Mirroring
%%%===================================================================

%% @doc Mirror a key-value update to ETS for concurrent reads.
-spec mirror_to_ets(atom() | ets:tid() | undefined, binary(), [term()]) -> ok.
mirror_to_ets(undefined, _Key, _Providers) -> ok;
mirror_to_ets(Ets, _Key, []) -> ets:delete(Ets, _Key), ok;
mirror_to_ets(Ets, Key, Providers) -> ets:insert(Ets, {Key, Providers}), ok.

%% @doc Mirror a delete operation to ETS.
-spec mirror_delete_to_ets(atom() | ets:tid() | undefined, binary(), map()) -> ok.
mirror_delete_to_ets(undefined, _Key, _Storage) -> ok;
mirror_delete_to_ets(Ets, Key, Storage) ->
    case maps:get(Key, Storage, []) of
        [] -> ets:delete(Ets, Key), ok;
        V -> ets:insert(Ets, {Key, V}), ok
    end.

%%%===================================================================
%%% Formatting
%%%===================================================================

%% @doc Format node_id for logging.
-spec format_node_id(binary() | undefined) -> binary().
format_node_id(undefined) -> <<"?">>;
format_node_id(B) when is_binary(B), byte_size(B) =:= 32 -> binary:encode_hex(B);
format_node_id(B) when is_binary(B) -> B;
format_node_id(_) -> <<"?">>.

%% @doc Extract endpoint from node info.
-spec get_node_endpoint(map()) -> binary() | undefined.
get_node_endpoint(#{endpoint := Endpoint}) when is_binary(Endpoint) ->
    Endpoint;
get_node_endpoint(#{address := {Host, Port}}) when is_integer(Port) ->
    iolist_to_binary([format_host(Host), <<":">>, integer_to_binary(Port)]);
get_node_endpoint(#{<<"endpoint">> := Endpoint}) when is_binary(Endpoint) ->
    Endpoint;
get_node_endpoint(#{address := Address}) when is_binary(Address) ->
    Address;
get_node_endpoint(#{<<"address">> := Address}) when is_binary(Address) ->
    Address;
get_node_endpoint(_) ->
    undefined.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

find_provider_index(_NodeId, [], _Index) ->
    not_found;
find_provider_index(NodeId, [Provider | Rest], Index) ->
    ProviderNodeId = get_node_id_from_value(Provider),
    check_provider_match(NodeId, ProviderNodeId, Rest, Index).

check_provider_match(NodeId, NodeId, _Rest, Index) -> Index;
check_provider_match(NodeId, _Other, Rest, Index) ->
    find_provider_index(NodeId, Rest, Index + 1).

upsert_by_index(not_found, Value, ProviderList) ->
    [Value | ProviderList];
upsert_by_index(Index, Value, ProviderList) ->
    lists:sublist(ProviderList, Index - 1) ++ [Value] ++ lists:nthtail(Index, ProviderList).

delete_provider_by_node_id(Key, NodeId, Providers, Storage) ->
    FilterFn = fun(P) -> get_node_id_from_value(P) =/= NodeId end,
    UpdatedProviders = lists:filter(FilterFn, Providers),
    update_or_remove_key(Key, UpdatedProviders, Storage).

update_or_remove_key(Key, [], Storage) -> maps:remove(Key, Storage);
update_or_remove_key(Key, Providers, Storage) -> Storage#{Key => Providers}.

%% @private Format host for URL.
format_host({A, B, C, D}) when is_integer(A) ->
    io_lib:format("~B.~B.~B.~B", [A, B, C, D]);
format_host(Host) when is_list(Host) ->
    Host;
format_host(Host) when is_binary(Host) ->
    binary_to_list(Host).
