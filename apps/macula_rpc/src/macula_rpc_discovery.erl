%%%-------------------------------------------------------------------
%%% @doc
%%% DHT integration for finding RPC service providers.
%%% Uses Kademlia DHT to publish and discover RPC registrations.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_discovery).

%% API
-export([
    find_providers/2,
    announce_registration/5,
    remove_registration/3,
    filter_available/2
]).

%% Types
-type uri() :: binary().
-type node_id() :: binary().
-type address() :: {inet:ip_address(), inet:port_number()}.

-type provider_info() :: #{
    node_id := node_id(),
    address := address(),
    metadata := map(),
    last_seen := integer()
}.

-type dht_lookup_fun() :: fun((uri()) -> {ok, [provider_info()]} | {error, term()}).
-type dht_publish_fun() :: fun((uri(), node_id(), address(), map()) -> ok | {error, term()}).
-type dht_unpublish_fun() :: fun((uri(), node_id()) -> ok | {error, term()}).

-export_type([
    uri/0,
    node_id/0,
    address/0,
    provider_info/0,
    dht_lookup_fun/0,
    dht_publish_fun/0,
    dht_unpublish_fun/0
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Find service providers for a URI via DHT.
-spec find_providers(uri(), dht_lookup_fun()) -> {ok, [provider_info()]} | {error, term()}.
find_providers(Uri, DhtLookupFun) ->
    DhtLookupFun(Uri).

%% @doc Announce local registration to DHT.
-spec announce_registration(uri(), node_id(), address(), map(), dht_publish_fun()) ->
    ok | {error, term()}.
announce_registration(Uri, LocalNodeId, LocalAddress, Metadata, DhtPublishFun) ->
    DhtPublishFun(Uri, LocalNodeId, LocalAddress, Metadata).

%% @doc Remove local registration from DHT.
-spec remove_registration(uri(), node_id(), dht_unpublish_fun()) -> ok | {error, term()}.
remove_registration(Uri, LocalNodeId, DhtUnpublishFun) ->
    DhtUnpublishFun(Uri, LocalNodeId).

%% @doc Filter providers to only available ones (based on last_seen TTL).
-spec filter_available([provider_info()], pos_integer()) -> [provider_info()].
filter_available(Providers, TTL) ->
    Now = erlang:system_time(millisecond),
    MaxAge = TTL * 1000,  % TTL in seconds, convert to milliseconds

    lists:filter(
        fun(Provider) ->
            LastSeen = maps:get(last_seen, Provider),
            Age = Now - LastSeen,
            Age =< MaxAge
        end,
        Providers
    ).
