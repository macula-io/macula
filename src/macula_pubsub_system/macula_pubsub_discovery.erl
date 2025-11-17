%%%-------------------------------------------------------------------
%%% @doc
%%% DHT integration for finding remote subscribers.
%%% Uses Kademlia DHT to publish and discover subscriptions.
%%% Wraps macula_discovery with pub/sub-specific types.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_discovery).

%% API
-export([
    find_subscribers/2,
    find_with_cache/3,
    find_with_cache/4,
    announce/4,
    unannounce/3
]).

%% Types
-type pattern() :: binary().
-type node_id() :: binary().
-type address() :: {inet:ip_address(), inet:port_number()}.
-type subscriber() :: #{node_id := node_id(), address := address()}.

-type dht_lookup_fun() :: fun((pattern()) -> {ok, [subscriber()]} | {error, term()}).
-type dht_publish_fun() :: fun((pattern(), node_id(), address()) -> ok | {error, term()}).
-type dht_unpublish_fun() :: fun((pattern(), node_id()) -> ok | {error, term()}).

-export_type([
    pattern/0,
    node_id/0,
    address/0,
    subscriber/0,
    dht_lookup_fun/0,
    dht_publish_fun/0,
    dht_unpublish_fun/0
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Find remote subscribers for a pattern via DHT.
-spec find_subscribers(pattern(), dht_lookup_fun()) -> {ok, [subscriber()]} | {error, term()}.
find_subscribers(Pattern, DhtLookupFun) ->
    macula_discovery:find(Pattern, DhtLookupFun).

%% @doc Find subscribers with cache (default TTL: 300 seconds).
-spec find_with_cache(pattern(), macula_cache:cache(), dht_lookup_fun()) ->
    {ok, [subscriber()], macula_cache:cache()} | {error, term(), macula_cache:cache()}.
find_with_cache(Pattern, Cache, DhtLookupFun) ->
    find_with_cache(Pattern, Cache, DhtLookupFun, 300).

%% @doc Find subscribers with cache and custom TTL.
-spec find_with_cache(pattern(), macula_cache:cache(), dht_lookup_fun(), pos_integer()) ->
    {ok, [subscriber()], macula_cache:cache()} | {error, term(), macula_cache:cache()}.
find_with_cache(Pattern, Cache, DhtLookupFun, TTL) ->
    macula_discovery:find_with_cache(Pattern, Cache, DhtLookupFun, TTL).

%% @doc Announce local subscription to DHT.
-spec announce(pattern(), node_id(), address(), dht_publish_fun()) -> ok | {error, term()}.
announce(Pattern, LocalNodeId, LocalAddress, DhtPublishFun) ->
    %% Wrap publish function to match generic discovery signature
    PublishFun = fun(Key) ->
        DhtPublishFun(Key, LocalNodeId, LocalAddress)
    end,
    macula_discovery:announce(Pattern, PublishFun).

%% @doc Remove local subscription from DHT.
-spec unannounce(pattern(), node_id(), dht_unpublish_fun()) -> ok | {error, term()}.
unannounce(Pattern, LocalNodeId, DhtUnpublishFun) ->
    %% Wrap unpublish function to match generic discovery signature
    UnpublishFun = fun(Key) ->
        DhtUnpublishFun(Key, LocalNodeId)
    end,
    macula_discovery:unannounce(Pattern, UnpublishFun).
