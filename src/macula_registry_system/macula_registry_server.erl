%%%-------------------------------------------------------------------
%%% @doc Macula Registry Server
%%%
%%% Main API server for package registry operations:
%%% - Package publishing with signature verification
%%% - Package fetching with integrity checks
%%% - Package queries and search
%%% - DHT integration for distributed discovery
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_registry_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([publish_package/5, publish_package/6]).
-export([fetch_package/1, fetch_package/2]).
-export([verify_package/4]).
-export([list_packages/0, search_packages/1]).
-export([get_package_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

-record(state, {
    node_id :: binary(),
    realm :: binary(),
    store_pid :: pid() | undefined,
    routing_pid :: pid() | undefined
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the registry server
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc Publish a package to the registry
-spec publish_package(Name :: binary(), Version :: binary(),
                      Manifest :: map(), BeamArchive :: binary(),
                      PrivateKey :: binary()) ->
    {ok, #{package_name := binary(), version := binary(), checksum := binary()}} |
    {error, term()}.
publish_package(Name, Version, Manifest, BeamArchive, PrivateKey) ->
    gen_server:call(?SERVER, {publish, Name, Version, Manifest, BeamArchive, PrivateKey}, 30000).

%% @doc Publish a package with pre-computed signature
-spec publish_package(Name :: binary(), Version :: binary(),
                      Manifest :: map(), BeamArchive :: binary(),
                      Signature :: binary(), PublicKey :: binary()) ->
    {ok, #{package_name := binary(), version := binary(), checksum := binary()}} |
    {error, term()}.
publish_package(Name, Version, Manifest, BeamArchive, Signature, PublicKey) ->
    gen_server:call(?SERVER, {publish_signed, Name, Version, Manifest, BeamArchive, Signature, PublicKey}, 30000).

%% @doc Fetch the latest version of a package
-spec fetch_package(Name :: binary()) ->
    {ok, map()} | {error, not_found}.
fetch_package(Name) ->
    gen_server:call(?SERVER, {fetch, Name, latest}).

%% @doc Fetch a specific version of a package
-spec fetch_package(Name :: binary(), Version :: binary()) ->
    {ok, map()} | {error, not_found}.
fetch_package(Name, Version) ->
    gen_server:call(?SERVER, {fetch, Name, Version}).

%% @doc Verify package signature
-spec verify_package(ManifestBin :: binary(), BeamArchive :: binary(),
                     Signature :: binary(), PublicKey :: binary()) ->
    ok | {error, term()}.
verify_package(ManifestBin, BeamArchive, Signature, PublicKey) ->
    macula_registry_verify:verify_package(ManifestBin, BeamArchive, Signature, PublicKey).

%% @doc List all packages in the registry
-spec list_packages() -> [map()].
list_packages() ->
    gen_server:call(?SERVER, list_packages).

%% @doc Search packages by pattern
-spec search_packages(Pattern :: binary()) -> [map()].
search_packages(Pattern) ->
    gen_server:call(?SERVER, {search, Pattern}).

%% @doc Get detailed info about a package
-spec get_package_info(Name :: binary()) ->
    {ok, #{versions := [binary()], latest := binary()}} | {error, not_found}.
get_package_info(Name) ->
    gen_server:call(?SERVER, {get_info, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Config) ->
    NodeId = maps:get(node_id, Config, generate_node_id()),
    Realm = maps:get(realm, Config, <<"default">>),

    %% Find store and routing PIDs after startup
    erlang:send_after(100, self(), init_pids),

    {ok, #state{
        node_id = NodeId,
        realm = Realm,
        store_pid = undefined,
        routing_pid = undefined
    }}.

%% @private
handle_call({publish, Name, Version, Manifest, BeamArchive, PrivateKey}, _From, State) ->
    Result = do_publish(Name, Version, Manifest, BeamArchive, PrivateKey, State),
    {reply, Result, State};

handle_call({publish_signed, Name, Version, Manifest, BeamArchive, Signature, PublicKey}, _From, State) ->
    Result = do_publish_signed(Name, Version, Manifest, BeamArchive, Signature, PublicKey, State),
    {reply, Result, State};

handle_call({fetch, Name, Version}, _From, State) ->
    Result = do_fetch(Name, Version, State),
    {reply, Result, State};

handle_call(list_packages, _From, State) ->
    Result = do_list_packages(State),
    {reply, Result, State};

handle_call({search, Pattern}, _From, State) ->
    Result = do_search(Pattern, State),
    {reply, Result, State};

handle_call({get_info, Name}, _From, State) ->
    Result = do_get_info(Name, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(init_pids, State) ->
    StorePid = find_store_pid(),
    RoutingPid = find_routing_pid(),
    {noreply, State#state{store_pid = StorePid, routing_pid = RoutingPid}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Publish with private key (compute signature)
do_publish(Name, Version, Manifest, BeamArchive, PrivateKey, State) ->
    %% Validate manifest
    case macula_registry_manifest:validate(Manifest) of
        ok ->
            %% Serialize manifest for signing
            ManifestBin = macula_registry_manifest:to_binary(Manifest),

            %% Sign the package
            case macula_registry_verify:sign_package(ManifestBin, BeamArchive, PrivateKey) of
                {ok, Signature} ->
                    %% Derive public key from private key
                    %% Note: In Ed25519, the private key contains the public key
                    PublicKey = extract_public_key(PrivateKey),
                    do_store_package(Name, Version, Manifest, BeamArchive, Signature, PublicKey, State);
                {error, Reason} ->
                    {error, {signing_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_manifest, Reason}}
    end.

%% @private Publish with pre-computed signature
do_publish_signed(Name, Version, Manifest, BeamArchive, Signature, PublicKey, State) ->
    %% Validate manifest
    case macula_registry_manifest:validate(Manifest) of
        ok ->
            %% Verify signature
            ManifestBin = macula_registry_manifest:to_binary(Manifest),
            case macula_registry_verify:verify_package(ManifestBin, BeamArchive, Signature, PublicKey) of
                ok ->
                    do_store_package(Name, Version, Manifest, BeamArchive, Signature, PublicKey, State);
                {error, Reason} ->
                    {error, {invalid_signature, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_manifest, Reason}}
    end.

%% @private Store package in store
do_store_package(Name, Version, Manifest, BeamArchive, Signature, PublicKey, State) ->
    case State#state.store_pid of
        undefined ->
            {error, store_not_available};
        StorePid ->
            PackageData = #{
                package_name => Name,
                version => Version,
                manifest => Manifest,
                beam_archive => BeamArchive,
                signature => Signature,
                public_key => PublicKey
            },
            case macula_registry_store:store_package(StorePid, PackageData) of
                {ok, Checksum} ->
                    %% Publish to DHT for discovery
                    publish_to_dht(Name, Version, Checksum, State),
                    {ok, #{
                        package_name => Name,
                        version => Version,
                        checksum => Checksum
                    }};
                {error, Reason} ->
                    {error, {store_failed, Reason}}
            end
    end.

%% @private Fetch package
do_fetch(Name, Version, State) ->
    case State#state.store_pid of
        undefined ->
            {error, store_not_available};
        StorePid ->
            case Version of
                latest -> macula_registry_store:get_package(StorePid, Name);
                _ -> macula_registry_store:get_package(StorePid, Name, Version)
            end
    end.

%% @private List packages
do_list_packages(State) ->
    case State#state.store_pid of
        undefined -> [];
        StorePid -> macula_registry_store:list_packages(StorePid)
    end.

%% @private Search packages
do_search(Pattern, State) ->
    case State#state.store_pid of
        undefined -> [];
        StorePid -> macula_registry_store:search_packages(StorePid, Pattern)
    end.

%% @private Get package info
do_get_info(Name, State) ->
    case State#state.store_pid of
        undefined ->
            {error, store_not_available};
        StorePid ->
            case macula_registry_store:get_latest_version(StorePid, Name) of
                {ok, Latest} ->
                    Versions = macula_registry_store:get_versions(StorePid, Name),
                    {ok, #{versions => Versions, latest => Latest}};
                {error, not_found} ->
                    {error, not_found}
            end
    end.

%% @private Find store PID
find_store_pid() ->
    case macula_registry_system:get_store_pid() of
        {ok, Pid} -> Pid;
        {error, _} -> whereis(macula_registry_store)
    end.

%% @private Find routing PID for DHT
find_routing_pid() ->
    whereis(macula_routing_server).

%% @private Publish package to DHT for discovery
publish_to_dht(Name, Version, Checksum, State) ->
    case State#state.routing_pid of
        undefined ->
            ?LOG_DEBUG("[RegistryServer] No routing server - skipping DHT publish"),
            ok;
        RoutingPid ->
            Key = package_dht_key(Name, Version),
            Value = #{
                package_name => Name,
                version => Version,
                checksum => Checksum,
                node_id => State#state.node_id,
                published_at => erlang:system_time(millisecond)
            },
            try
                macula_routing_server:store_local(RoutingPid, Key, Value)
            catch
                _:_ ->
                    ?LOG_WARNING("[RegistryServer] Failed to publish ~s v~s to DHT", [Name, Version])
            end
    end.

%% @private Generate DHT key for package
package_dht_key(Name, Version) ->
    crypto:hash(sha256, <<"pkg:", Name/binary, ":", Version/binary>>).

%% @private Generate node ID
generate_node_id() ->
    crypto:strong_rand_bytes(32).

%% @private Extract public key from private key
%% In Ed25519, the private key is 64 bytes: 32-byte seed + 32-byte public key
extract_public_key(PrivateKey) when byte_size(PrivateKey) =:= 64 ->
    <<_Seed:32/binary, PublicKey:32/binary>> = PrivateKey,
    PublicKey;
extract_public_key(PrivateKey) when byte_size(PrivateKey) =:= 32 ->
    %% This is just a seed, need to derive public key
    #{public := PublicKey} = crypto:generate_key(eddsa, ed25519, PrivateKey),
    PublicKey.
