%%%-------------------------------------------------------------------
%%% @doc
%%% DHT integration for Macula content-addressed storage.
%%%
%%% Handles content announcement and discovery through the DHT:
%%% - Announce manifest availability to mesh peers
%%% - Discover providers of content by MCID
%%% - TTL-based re-announcement for liveness
%%%
%%% == DHT Key Structure ==
%%% DHT keys are SHA-256 hashes of the MCID. This ensures:
%%% - Uniform distribution in the DHT keyspace
%%% - Same content always maps to same key
%%% - 32-byte keys match DHT node ID size
%%%
%%% == Provider Discovery ==
%%% When content is published, the provider announces to the DHT:
%%% ```
%%% Key: hash(MCID)
%%% Value: #{node_id, endpoint, metadata, advertised_at, ttl}
%%% '''
%%% Multiple providers can announce the same content.
%%%
%%% == Example Usage ==
%%% ```
%%% %% Announce content availability
%%% {Key, Value} = macula_content_dht:create_announcement(MCID, NodeId, Endpoint, Info),
%%% macula_routing_server:store(RoutingPid, Key, Value).
%%%
%%% %% Discover providers
%%% case macula_routing_server:find_value(RoutingPid, Key, 20) of
%%%     {ok, Providers} -> handle_providers(Providers);
%%%     {nodes, _} -> {error, not_found}
%%% end.
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_dht).

-include_lib("kernel/include/logger.hrl").

%% API - Key generation
-export([
    dht_key/1
]).

%% API - Provider info
-export([
    create_provider_info/3,
    format_providers/1
]).

%% API - Announcement
-export([
    create_announcement/4,
    create_removal/1
]).

%% API - TTL
-export([
    default_ttl/0,
    get_ttl/1,
    reannounce_interval/1
]).

%% API - High-level operations (use with routing server)
-export([
    announce_manifest/4,
    announce_manifest/5,
    unannounce_manifest/3,
    locate_providers/1,
    locate_providers/2
]).

%% Constants
-define(DEFAULT_TTL, 300).       % 5 minutes
-define(MIN_REANNOUNCE, 30).     % Minimum 30 seconds before TTL expires

%%%===================================================================
%%% Types
%%%===================================================================

-type mcid() :: <<_:272>>.  %% 34 bytes: version + codec + hash

-type provider_info() :: #{
    node_id := binary(),
    endpoint := binary(),
    metadata := map(),
    advertised_at := integer(),
    ttl => pos_integer()
}.

-export_type([mcid/0, provider_info/0]).

%%%===================================================================
%%% Key Generation
%%%===================================================================

%% @doc Generate DHT key from MCID.
%% Uses SHA-256 to hash the MCID, ensuring uniform distribution
%% in the DHT keyspace.
-spec dht_key(mcid()) -> binary().
dht_key(MCID) when is_binary(MCID) ->
    crypto:hash(sha256, MCID).

%%%===================================================================
%%% Provider Info
%%%===================================================================

%% @doc Create provider info map with all required fields.
-spec create_provider_info(binary(), binary(), map()) -> provider_info().
create_provider_info(NodeId, Endpoint, Metadata) ->
    #{
        node_id => NodeId,
        endpoint => Endpoint,
        metadata => Metadata,
        advertised_at => erlang:system_time(second)
    }.

%% @doc Format raw provider values into standardized list.
%% Handles both single provider and list of providers.
-spec format_providers([provider_info()] | provider_info()) -> [provider_info()].
format_providers([]) ->
    [];
format_providers(Providers) when is_list(Providers) ->
    [format_single_provider(P) || P <- Providers];
format_providers(SingleProvider) when is_map(SingleProvider) ->
    [format_single_provider(SingleProvider)].

%% @private Format single provider, ensuring consistent structure.
format_single_provider(#{node_id := NodeId, endpoint := Endpoint} = Provider) ->
    #{
        node_id => NodeId,
        endpoint => Endpoint,
        metadata => maps:get(metadata, Provider, #{}),
        advertised_at => maps:get(advertised_at, Provider, 0)
    };
format_single_provider(Provider) when is_map(Provider) ->
    %% Handle legacy or incomplete provider info
    #{
        node_id => maps:get(node_id, Provider, <<>>),
        endpoint => maps:get(endpoint, Provider, <<>>),
        metadata => maps:get(metadata, Provider, #{}),
        advertised_at => maps:get(advertised_at, Provider, 0)
    }.

%%%===================================================================
%%% Announcement Functions
%%%===================================================================

%% @doc Create DHT announcement for content availability.
%% Returns {DHTKey, ProviderValue} tuple ready for storage.
-spec create_announcement(mcid(), binary(), binary(), map()) -> {binary(), provider_info()}.
create_announcement(MCID, NodeId, Endpoint, ManifestInfo) ->
    Key = dht_key(MCID),
    Value = #{
        node_id => NodeId,
        endpoint => Endpoint,
        metadata => ManifestInfo,
        advertised_at => erlang:system_time(second),
        ttl => ?DEFAULT_TTL
    },
    {Key, Value}.

%% @doc Create removal marker for DHT.
%% When a provider stops hosting content, they announce removal.
-spec create_removal(binary()) -> map().
create_removal(NodeId) ->
    #{
        node_id => NodeId,
        removed => true,
        removed_at => erlang:system_time(second)
    }.

%%%===================================================================
%%% TTL Functions
%%%===================================================================

%% @doc Get default TTL for content announcements.
-spec default_ttl() -> pos_integer().
default_ttl() ->
    ?DEFAULT_TTL.

%% @doc Get TTL from options or use default.
-spec get_ttl(map()) -> pos_integer().
get_ttl(Opts) when is_map(Opts) ->
    maps:get(ttl, Opts, ?DEFAULT_TTL).

%% @doc Calculate interval for re-announcement.
%% Re-announces slightly before TTL expires to maintain presence.
%% Returns interval in seconds.
-spec reannounce_interval(pos_integer()) -> pos_integer().
reannounce_interval(TTL) when TTL > ?MIN_REANNOUNCE + 60 ->
    TTL - 60;  % 60 seconds before expiry
reannounce_interval(_TTL) ->
    ?MIN_REANNOUNCE.  % Minimum 30 seconds

%%%===================================================================
%%% High-Level DHT Operations
%%%===================================================================

%% @doc Announce manifest availability to DHT.
%% Stores provider info at DHT key derived from MCID.
-spec announce_manifest(mcid(), binary(), binary(), map()) ->
    ok | {error, term()}.
announce_manifest(MCID, NodeId, Endpoint, ManifestInfo) ->
    announce_manifest(MCID, NodeId, Endpoint, ManifestInfo, #{}).

%% @doc Announce manifest with options.
%% Options:
%% - ttl: Custom TTL (default: 300 seconds)
-spec announce_manifest(mcid(), binary(), binary(), map(), map()) ->
    ok | {error, term()}.
announce_manifest(MCID, NodeId, Endpoint, ManifestInfo, Opts) ->
    {Key, BaseValue} = create_announcement(MCID, NodeId, Endpoint, ManifestInfo),
    TTL = get_ttl(Opts),
    Value = BaseValue#{ttl => TTL},

    ?LOG_INFO("[content_dht] Announcing manifest ~s from node ~s",
              [format_mcid(MCID), short_id(NodeId)]),

    case whereis(macula_routing_server) of
        undefined ->
            ?LOG_WARNING("[content_dht] Routing server not available"),
            {error, routing_server_unavailable};
        RoutingPid ->
            case macula_routing_server:store(RoutingPid, Key, Value) of
                ok ->
                    ?LOG_DEBUG("[content_dht] Successfully announced manifest"),
                    ok;
                {error, Reason} = Error ->
                    ?LOG_WARNING("[content_dht] Failed to announce: ~p", [Reason]),
                    Error
            end
    end.

%% @doc Remove manifest announcement from DHT.
-spec unannounce_manifest(mcid(), binary(), map()) -> ok | {error, term()}.
unannounce_manifest(MCID, NodeId, _Opts) ->
    Key = dht_key(MCID),
    RemovalValue = create_removal(NodeId),

    ?LOG_INFO("[content_dht] Unannouncing manifest ~s from node ~s",
              [format_mcid(MCID), short_id(NodeId)]),

    case whereis(macula_routing_server) of
        undefined ->
            {error, routing_server_unavailable};
        RoutingPid ->
            %% Store removal marker (or delete if DHT supports it)
            macula_routing_server:store(RoutingPid, Key, RemovalValue)
    end.

%% @doc Locate providers of content by MCID.
%% Queries DHT and returns list of provider endpoints.
-spec locate_providers(mcid()) -> {ok, [provider_info()]} | {error, term()}.
locate_providers(MCID) ->
    locate_providers(MCID, #{}).

%% @doc Locate providers with options.
%% Options:
%% - k: Number of closest nodes to query (default: 20)
%% - timeout: Query timeout in milliseconds
-spec locate_providers(mcid(), map()) -> {ok, [provider_info()]} | {error, term()}.
locate_providers(MCID, Opts) ->
    Key = dht_key(MCID),
    K = maps:get(k, Opts, 20),

    ?LOG_DEBUG("[content_dht] Locating providers for ~s", [format_mcid(MCID)]),

    case whereis(macula_routing_server) of
        undefined ->
            {error, routing_server_unavailable};
        RoutingPid ->
            case macula_routing_server:find_value(RoutingPid, Key, K) of
                {ok, Value} when is_map(Value) ->
                    %% Single provider
                    filter_active_providers([Value]);
                {ok, Values} when is_list(Values) ->
                    %% Multiple providers
                    filter_active_providers(Values);
                {nodes, _ClosestNodes} ->
                    %% Value not found
                    {ok, []};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Filter out removed providers and format result.
filter_active_providers(Providers) ->
    Active = [P || P <- Providers,
                   is_map(P),
                   maps:get(removed, P, false) =:= false],
    {ok, format_providers(Active)}.

%% @private Format MCID for logging.
format_mcid(<<_Version:8, _Codec:8, Hash:32/binary>>) ->
    HexHash = macula_content_hasher:hex_encode(Hash),
    <<Short:16/binary, _/binary>> = HexHash,
    <<"mcid:", Short/binary, "...">>;
format_mcid(_) ->
    <<"mcid:invalid">>.

%% @private Format node ID for logging (first 8 chars).
short_id(NodeId) when is_binary(NodeId), byte_size(NodeId) >= 4 ->
    HexId = macula_content_hasher:hex_encode(binary:part(NodeId, 0, 4)),
    <<Short:8/binary, _/binary>> = HexId,
    Short;
short_id(_) ->
    <<"unknown">>.
