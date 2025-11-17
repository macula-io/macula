%%%-------------------------------------------------------------------
%%% @doc
%%% DHT integration for finding RPC service providers.
%%% Uses Kademlia DHT to publish and discover RPC registrations.
%%% Wraps macula_discovery with RPC-specific types.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_discovery).

%% API
-export([
    find_providers/2,
    find_with_cache/3,
    find_with_cache/4,
    announce/5,
    unannounce/3,
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
    macula_discovery:find(Uri, DhtLookupFun).

%% @doc Find providers with cache (default TTL: 300 seconds).
-spec find_with_cache(uri(), macula_cache:cache(), dht_lookup_fun()) ->
    {ok, [provider_info()], macula_cache:cache()} | {error, term(), macula_cache:cache()}.
find_with_cache(Uri, Cache, DhtLookupFun) ->
    find_with_cache(Uri, Cache, DhtLookupFun, 300).

%% @doc Find providers with cache and custom TTL.
-spec find_with_cache(uri(), macula_cache:cache(), dht_lookup_fun(), pos_integer()) ->
    {ok, [provider_info()], macula_cache:cache()} | {error, term(), macula_cache:cache()}.
find_with_cache(Uri, Cache, DhtLookupFun, TTL) ->
    macula_discovery:find_with_cache(Uri, Cache, DhtLookupFun, TTL).

%% @doc Announce local registration to DHT.
-spec announce(uri(), node_id(), address(), map(), dht_publish_fun()) ->
    ok | {error, term()}.
announce(Uri, LocalNodeId, LocalAddress, Metadata, DhtPublishFun) ->
    %% Wrap publish function to match generic discovery signature
    PublishFun = fun(Key) ->
        DhtPublishFun(Key, LocalNodeId, LocalAddress, Metadata)
    end,
    macula_discovery:announce(Uri, PublishFun).

%% @doc Remove local registration from DHT.
-spec unannounce(uri(), node_id(), dht_unpublish_fun()) -> ok | {error, term()}.
unannounce(Uri, LocalNodeId, DhtUnpublishFun) ->
    %% Wrap unpublish function to match generic discovery signature
    UnpublishFun = fun(Key) ->
        DhtUnpublishFun(Key, LocalNodeId)
    end,
    macula_discovery:unannounce(Uri, UnpublishFun).

%% @doc Filter providers to only available ones (based on last_seen TTL).
-spec filter_available([provider_info()], pos_integer()) -> [provider_info()].
filter_available(Providers, TTL) ->
    macula_discovery:filter_by_age(Providers, TTL, last_seen).
