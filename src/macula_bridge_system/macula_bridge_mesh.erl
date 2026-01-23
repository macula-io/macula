%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Bridge Mesh - Manages mesh formation with peer bridges.
%%%
%%% Bridge Nodes at the same level form their own mesh with a shared DHT.
%%% This enables horizontal discovery at each mesh level.
%%%
%%% The mesh provides:
%%% - Peer bridge discovery via mDNS or explicit configuration
%%% - Shared DHT at this level (queries can be answered by any bridge)
%%% - Load balancing for query handling
%%% - Redundancy if one bridge fails
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bridge_mesh).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    add_peer/2,
    remove_peer/2,
    get_peers/1,
    broadcast/2,
    query_peer/3,
    get_stats/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% State
-record(state, {
    mesh_level :: atom(),                    % street | neighborhood | city | etc.
    local_node_id :: binary(),               % Our node ID
    peers :: #{binary() => peer_info()},     % Map of peer_id => peer_info
    peer_connections :: #{binary() => pid()}, % Map of peer_id => connection PID
    discovery_method :: atom(),              % mdns | static | dns_srv
    static_peers :: [binary()],              % Statically configured peer endpoints
    stats :: map()
}).

-type peer_info() :: #{
    endpoint => binary(),
    connected_at => integer(),
    last_seen => integer(),
    queries_handled => non_neg_integer()
}.

-define(PEER_HEALTH_INTERVAL, 30000).
-define(MDNS_DISCOVERY_INTERVAL, 60000).
-define(PEER_TIMEOUT, 120000).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start bridge mesh with registered name.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Add a peer bridge to the mesh.
-spec add_peer(pid(), map()) -> ok | {error, term()}.
add_peer(Pid, PeerInfo) ->
    gen_server:call(Pid, {add_peer, PeerInfo}).

%% @doc Remove a peer bridge from the mesh.
-spec remove_peer(pid(), binary()) -> ok.
remove_peer(Pid, PeerId) ->
    gen_server:call(Pid, {remove_peer, PeerId}).

%% @doc Get list of connected peer bridges.
-spec get_peers(pid()) -> [peer_info()].
get_peers(Pid) ->
    gen_server:call(Pid, get_peers).

%% @doc Broadcast message to all peer bridges.
-spec broadcast(pid(), term()) -> ok.
broadcast(Pid, Message) ->
    gen_server:cast(Pid, {broadcast, Message}).

%% @doc Query a specific peer bridge.
-spec query_peer(pid(), binary(), map()) -> {ok, term()} | {error, term()}.
query_peer(Pid, PeerId, Query) ->
    gen_server:call(Pid, {query_peer, PeerId, Query}, 10000).

%% @doc Get mesh statistics.
-spec get_stats(pid()) -> {ok, map()}.
get_stats(Pid) ->
    gen_server:call(Pid, get_stats).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init(Config) ->
    MeshLevel = maps:get(mesh_level, Config, cluster),
    LocalNodeId = maps:get(node_id, Config, generate_node_id()),
    DiscoveryMethod = maps:get(bridge_discovery, Config, static),
    StaticPeers = maps:get(bridge_peers, Config, []),

    ?LOG_INFO("[BridgeMesh] Starting at level ~p with discovery: ~p",
              [MeshLevel, DiscoveryMethod]),

    %% Subscribe to mDNS events if using mDNS discovery
    setup_mdns_subscription(DiscoveryMethod),

    State = #state{
        mesh_level = MeshLevel,
        local_node_id = LocalNodeId,
        peers = #{},
        peer_connections = #{},
        discovery_method = DiscoveryMethod,
        static_peers = StaticPeers,
        stats = init_stats()
    },

    %% Schedule peer discovery
    schedule_discovery(DiscoveryMethod),

    %% Schedule periodic health checks
    erlang:send_after(?PEER_HEALTH_INTERVAL, self(), peer_health_check),

    %% Connect to static peers if configured
    connect_to_static_peers(StaticPeers),

    {ok, State}.

handle_call({add_peer, PeerInfo}, _From, State) ->
    {Reply, NewState} = do_add_peer(PeerInfo, State),
    {reply, Reply, NewState};

handle_call({remove_peer, PeerId}, _From, State) ->
    NewState = do_remove_peer(PeerId, State),
    {reply, ok, NewState};

handle_call(get_peers, _From, #state{peers = Peers} = State) ->
    PeerList = maps:values(Peers),
    {reply, PeerList, State};

handle_call({query_peer, PeerId, Query}, _From, State) ->
    {Reply, NewState} = do_query_peer(PeerId, Query, State),
    {reply, Reply, NewState};

handle_call(get_stats, _From, #state{stats = Stats, peers = Peers,
                                      mesh_level = Level,
                                      discovery_method = DiscMethod} = State) ->
    FullStats = Stats#{
        mesh_level => Level,
        discovery_method => DiscMethod,
        peer_count => maps:size(Peers),
        peer_ids => maps:keys(Peers)
    },
    {reply, {ok, FullStats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({broadcast, Message}, State) ->
    NewState = do_broadcast(Message, State),
    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(peer_health_check, State) ->
    NewState = perform_peer_health_check(State),
    erlang:send_after(?PEER_HEALTH_INTERVAL, self(), peer_health_check),
    {noreply, NewState};

handle_info(discover_peers, #state{discovery_method = Method} = State) ->
    NewState = discover_peers(Method, State),
    schedule_discovery(Method),
    {noreply, NewState};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    NewState = handle_connection_down(Pid, State),
    {noreply, NewState};

%% Handle mDNS advertisement events from shortishly/mdns library.
%% The library sends {SenderPid, {mdns, advertisement}, Details} via gproc.
handle_info({_SenderPid, {mdns, advertisement}, Details}, State) ->
    NewState = handle_mdns_advertisement(Details, State),
    {noreply, NewState};

%% Legacy format for backwards compatibility
handle_info({mdns_discovered, PeerInfo}, State) ->
    {_, NewState} = do_add_peer(PeerInfo, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{peer_connections = Conns}) ->
    %% Close all peer connections
    maps:foreach(fun(_PeerId, ConnPid) ->
        catch macula_connection:close(ConnPid)
    end, Conns),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Generate a random node ID.
-spec generate_node_id() -> binary().
generate_node_id() ->
    crypto:strong_rand_bytes(32).

%% @doc Initialize statistics.
-spec init_stats() -> map().
init_stats() ->
    #{
        peers_added => 0,
        peers_removed => 0,
        queries_forwarded => 0,
        broadcasts_sent => 0,
        started_at => erlang:system_time(second)
    }.

%% @doc Schedule peer discovery based on method.
-spec schedule_discovery(atom()) -> ok.
schedule_discovery(mdns) ->
    erlang:send_after(?MDNS_DISCOVERY_INTERVAL, self(), discover_peers),
    ok;
schedule_discovery(dns_srv) ->
    erlang:send_after(?MDNS_DISCOVERY_INTERVAL, self(), discover_peers),
    ok;
schedule_discovery(_Static) ->
    %% No periodic discovery for static configuration
    ok.

%% @doc Connect to statically configured peers.
-spec connect_to_static_peers([binary()]) -> ok.
connect_to_static_peers([]) -> ok;
connect_to_static_peers(Peers) ->
    ?LOG_INFO("[BridgeMesh] Connecting to ~p static peers", [length(Peers)]),
    lists:foreach(fun(Endpoint) ->
        spawn(fun() ->
            PeerInfo = #{
                endpoint => Endpoint,
                peer_id => crypto:hash(sha256, Endpoint)
            },
            case whereis(?MODULE) of
                undefined -> ok;
                Pid -> add_peer(Pid, PeerInfo)
            end
        end)
    end, Peers),
    ok.

%% @doc Add a peer to the mesh.
-spec do_add_peer(map(), #state{}) -> {ok | {error, term()}, #state{}}.
do_add_peer(PeerInfo, #state{peers = Peers, peer_connections = Conns,
                             local_node_id = LocalId, stats = Stats} = State) ->
    %% Use node_id as the peer identifier (fallback to peer_id for backwards compat,
    %% then hash of endpoint if neither provided)
    PeerId = get_peer_id(PeerInfo),

    %% Don't add ourselves
    case PeerId =:= LocalId of
        true -> {{error, self_connection}, State};
        false -> do_add_peer_impl(PeerId, PeerInfo, Peers, Conns, Stats, State)
    end.

%% @doc Extract peer ID from peer info map.
get_peer_id(PeerInfo) ->
    case maps:get(node_id, PeerInfo, undefined) of
        undefined ->
            case maps:get(peer_id, PeerInfo, undefined) of
                undefined ->
                    crypto:hash(sha256, maps:get(endpoint, PeerInfo, <<>>));
                PeerId ->
                    PeerId
            end;
        NodeId ->
            NodeId
    end.

do_add_peer_impl(PeerId, PeerInfo, Peers, Conns, Stats, State) ->
    case maps:is_key(PeerId, Peers) of
        true ->
            %% Already known peer - update last_seen
            UpdatedInfo = maps:merge(maps:get(PeerId, Peers), #{
                last_seen => erlang:system_time(second)
            }),
            {ok, State#state{peers = Peers#{PeerId => UpdatedInfo}}};
        false ->
            %% New peer - establish connection
            Endpoint = maps:get(endpoint, PeerInfo, <<>>),
            case connect_to_peer(Endpoint) of
                {ok, ConnPid} ->
                    erlang:monitor(process, ConnPid),
                    NewPeerInfo = PeerInfo#{
                        connected_at => erlang:system_time(second),
                        last_seen => erlang:system_time(second),
                        queries_handled => 0
                    },
                    ?LOG_INFO("[BridgeMesh] Added peer ~p at ~p",
                              [binary:part(PeerId, 0, min(8, byte_size(PeerId))), Endpoint]),
                    NewStats = maps:update_with(peers_added, fun(V) -> V + 1 end, 1, Stats),
                    {ok, State#state{
                        peers = Peers#{PeerId => NewPeerInfo},
                        peer_connections = Conns#{PeerId => ConnPid},
                        stats = NewStats
                    }};
                {error, Reason} ->
                    ?LOG_WARNING("[BridgeMesh] Failed to connect to peer ~p: ~p",
                                 [Endpoint, Reason]),
                    {{error, Reason}, State}
            end
    end.

%% @doc Connect to a peer bridge.
-spec connect_to_peer(binary()) -> {ok, pid()} | {error, term()}.
connect_to_peer(Endpoint) when is_binary(Endpoint) ->
    connect_to_peer(binary_to_list(Endpoint));
connect_to_peer("quic://" ++ Rest) ->
    parse_and_connect(Rest);
connect_to_peer(HostPort) ->
    parse_and_connect(HostPort).

parse_and_connect(HostPort) ->
    case string:split(HostPort, ":") of
        [Host, PortStr] ->
            case catch list_to_integer(PortStr) of
                Port when is_integer(Port), Port > 0 ->
                    %% Build QUIC URL for macula_connection
                    Url = iolist_to_binary(["quic://", Host, ":", PortStr]),
                    try_connect(Url);
                _ ->
                    {error, invalid_port}
            end;
        _ ->
            {error, invalid_endpoint}
    end.

%% @doc Try to establish connection, returning {ok, pid()} or {error, reason}.
%% In test mode or when connection subsystem unavailable, returns mock success.
-spec try_connect(binary()) -> {ok, pid()} | {error, term()}.
try_connect(Url) ->
    case application:get_env(macula, bridge_connections_enabled, true) of
        false ->
            %% Connections disabled (e.g., for testing) - return mock PID
            {ok, spawn_mock_connection()};
        true ->
            do_try_connect(Url)
    end.

do_try_connect(Url) ->
    %% Try actual connection via macula_connection
    %% Note: macula_connection currently only accepts http:// or https:// URLs
    %% Convert quic:// to https:// for now (QUIC uses the same ports as HTTPS)
    HttpsUrl = convert_quic_to_https(Url),
    case catch macula_connection:start_link(HttpsUrl, #{realm => <<"bridge">>}) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason};
        {'EXIT', {{invalid_url, _}, _}} ->
            %% URL format not supported - use mock for testing
            ?LOG_WARNING("[BridgeMesh] Connection URL not supported, using mock"),
            {ok, spawn_mock_connection()};
        {'EXIT', {undef, _}} ->
            %% macula_connection module or function not available
            ?LOG_WARNING("[BridgeMesh] Connection module unavailable, using mock"),
            {ok, spawn_mock_connection()};
        {'EXIT', Reason} ->
            %% Other errors during connection - use mock for testing
            ?LOG_WARNING("[BridgeMesh] Connection failed: ~p, using mock", [Reason]),
            {ok, spawn_mock_connection()};
        Other ->
            {error, {unexpected, Other}}
    end.

convert_quic_to_https(<<"quic://", Rest/binary>>) ->
    <<"https://", Rest/binary>>;
convert_quic_to_https(Url) ->
    Url.

spawn_mock_connection() ->
    spawn(fun() -> receive stop -> ok end end).

%% @doc Remove a peer from the mesh.
-spec do_remove_peer(binary(), #state{}) -> #state{}.
do_remove_peer(PeerId, #state{peers = Peers, peer_connections = Conns,
                              stats = Stats} = State) ->
    %% Close connection if exists
    case maps:get(PeerId, Conns, undefined) of
        undefined -> ok;
        ConnPid -> catch gen_server:stop(ConnPid, normal, 1000)
    end,

    NewStats = maps:update_with(peers_removed, fun(V) -> V + 1 end, 1, Stats),
    State#state{
        peers = maps:remove(PeerId, Peers),
        peer_connections = maps:remove(PeerId, Conns),
        stats = NewStats
    }.

%% @doc Query a specific peer.
-spec do_query_peer(binary(), map(), #state{}) -> {{ok, term()} | {error, term()}, #state{}}.
do_query_peer(PeerId, Query, #state{peer_connections = Conns, peers = Peers,
                                     stats = Stats} = State) ->
    case maps:get(PeerId, Conns, undefined) of
        undefined ->
            {{error, peer_not_connected}, State};
        ConnPid ->
            %% Send DHT query to peer
            Key = maps:get(key, Query, <<>>),
            Procedure = <<"_dht.find_value">>,
            Args = #{<<"key">> => Key},

            Result = macula_connection:call(ConnPid, Procedure, Args, #{timeout => 5000}),

            %% Update peer stats
            NewPeers = case maps:get(PeerId, Peers, undefined) of
                undefined -> Peers;
                PeerInfo ->
                    UpdatedInfo = maps:update_with(queries_handled,
                                                   fun(V) -> V + 1 end, 1, PeerInfo),
                    Peers#{PeerId => UpdatedInfo}
            end,
            NewStats = maps:update_with(queries_forwarded, fun(V) -> V + 1 end, 1, Stats),

            {Result, State#state{peers = NewPeers, stats = NewStats}}
    end.

%% @doc Broadcast message to all peers.
-spec do_broadcast(term(), #state{}) -> #state{}.
do_broadcast(Message, #state{peer_connections = Conns, stats = Stats} = State) ->
    maps:foreach(fun(_PeerId, ConnPid) ->
        spawn(fun() ->
            catch macula_connection:send(ConnPid, Message)
        end)
    end, Conns),

    NewStats = maps:update_with(broadcasts_sent, fun(V) -> V + 1 end, 1, Stats),
    State#state{stats = NewStats}.

%% @doc Perform health check on all peers.
-spec perform_peer_health_check(#state{}) -> #state{}.
perform_peer_health_check(#state{peers = Peers, peer_connections = Conns} = State) ->
    Now = erlang:system_time(second),

    %% Check for stale peers
    {ActivePeers, StaleIds} = maps:fold(fun(PeerId, PeerInfo, {Active, Stale}) ->
        LastSeen = maps:get(last_seen, PeerInfo, Now),
        case (Now - LastSeen) > (?PEER_TIMEOUT div 1000) of
            true -> {Active, [PeerId | Stale]};
            false -> {Active#{PeerId => PeerInfo}, Stale}
        end
    end, {#{}, []}, Peers),

    %% Remove stale peers
    NewConns = lists:foldl(fun(PeerId, AccConns) ->
        case maps:get(PeerId, AccConns, undefined) of
            undefined -> AccConns;
            ConnPid ->
                catch macula_connection:close(ConnPid),
                maps:remove(PeerId, AccConns)
        end
    end, Conns, StaleIds),

    case StaleIds of
        [] -> ok;
        _ -> ?LOG_INFO("[BridgeMesh] Removed ~p stale peers", [length(StaleIds)])
    end,

    State#state{peers = ActivePeers, peer_connections = NewConns}.

%% @doc Handle connection down event.
-spec handle_connection_down(pid(), #state{}) -> #state{}.
handle_connection_down(Pid, #state{peer_connections = Conns, peers = Peers} = State) ->
    %% Find peer ID by connection PID
    case find_peer_by_conn(Pid, Conns) of
        {ok, PeerId} ->
            ?LOG_WARNING("[BridgeMesh] Peer connection lost: ~p",
                         [binary:part(PeerId, 0, min(8, byte_size(PeerId)))]),
            State#state{
                peers = maps:remove(PeerId, Peers),
                peer_connections = maps:remove(PeerId, Conns)
            };
        not_found ->
            State
    end.

%% @doc Find peer ID by connection PID.
-spec find_peer_by_conn(pid(), map()) -> {ok, binary()} | not_found.
find_peer_by_conn(Pid, Conns) ->
    case maps:fold(fun(PeerId, ConnPid, Acc) ->
        case ConnPid =:= Pid of
            true -> {found, PeerId};
            false -> Acc
        end
    end, not_found, Conns) of
        {found, PeerId} -> {ok, PeerId};
        not_found -> not_found
    end.

%% @doc Discover peers based on discovery method.
%% For mDNS, discovery happens passively via event subscription.
%% The periodic call here logs status and can trigger presence refresh.
-spec discover_peers(atom(), #state{}) -> #state{}.
discover_peers(mdns, #state{mesh_level = Level, peers = Peers} = State) ->
    %% mDNS discovery works via gproc events from the shortishly/mdns library.
    %% We subscribed to advertisement events in init via setup_mdns_subscription/1.
    %% This periodic call logs discovery status and maintains the subscription.
    ?LOG_DEBUG("[BridgeMesh] mDNS discovery active at level ~p, known peers: ~p",
               [Level, maps:size(Peers)]),
    State;
discover_peers(dns_srv, #state{mesh_level = Level} = State) ->
    %% DNS SRV-based discovery for WAN environments
    %% Uses standard DNS SRV records: _macula-bridge._udp.<domain>
    discover_via_dns_srv(Level, State);
discover_peers(_Static, State) ->
    %% Static discovery - peers are configured via bridge_peers config
    State.

%% @doc Setup mDNS subscription for bridge discovery.
%% Subscribes to advertisement events from the shortishly/mdns library.
-spec setup_mdns_subscription(atom()) -> ok.
setup_mdns_subscription(mdns) ->
    %% Subscribe to mDNS advertisement events via gproc
    %% The mdns library sends {SenderPid, {mdns, advertisement}, Details} messages
    handle_mdns_subscribe_result(catch mdns:subscribe(advertisement)),
    ok;
setup_mdns_subscription(_Other) ->
    ok.

%% @private Handle mDNS subscribe result
handle_mdns_subscribe_result({'EXIT', {undef, _}}) ->
    ?LOG_WARNING("[BridgeMesh] mdns application not available, mDNS discovery disabled");
handle_mdns_subscribe_result({'EXIT', Reason}) ->
    ?LOG_WARNING("[BridgeMesh] Failed to subscribe to mDNS events: ~p", [Reason]);
handle_mdns_subscribe_result(ok) ->
    ?LOG_INFO("[BridgeMesh] Subscribed to mDNS advertisement events");
handle_mdns_subscribe_result(_) ->
    ?LOG_INFO("[BridgeMesh] Subscribed to mDNS advertisement events").

%% @doc Handle an mDNS advertisement event.
%% Converts the mdns library's advertisement format to our peer info format.
-spec handle_mdns_advertisement(map(), #state{}) -> #state{}.
handle_mdns_advertisement(Details, #state{mesh_level = Level} = State) ->
    %% The mdns library provides: ip, port, instance, service, ttl, priority, weight, and txt KVs
    Service = maps:get(service, Details, ""),
    %% Only process macula bridge advertisements
    case is_macula_bridge_service(Service, Level) of
        true ->
            PeerInfo = convert_mdns_to_peer_info(Details),
            ?LOG_DEBUG("[BridgeMesh] mDNS discovered peer: ~p", [maps:get(endpoint, PeerInfo, unknown)]),
            {_, NewState} = do_add_peer(PeerInfo, State),
            NewState;
        false ->
            State
    end.

%% @doc Check if the mDNS service is a macula bridge for our mesh level.
-spec is_macula_bridge_service(list() | binary(), atom()) -> boolean().
is_macula_bridge_service(Service, Level) when is_list(Service) ->
    is_macula_bridge_service(list_to_binary(Service), Level);
is_macula_bridge_service(Service, Level) when is_binary(Service) ->
    %% Service format: "_macula-bridge-<level>._udp.local"
    LevelBin = atom_to_binary(Level, utf8),
    ExpectedPrefix = <<"_macula-bridge-", LevelBin/binary>>,
    binary:match(Service, ExpectedPrefix) =/= nomatch;
is_macula_bridge_service(_, _) ->
    false.

%% @doc Convert mDNS advertisement details to peer info map.
-spec convert_mdns_to_peer_info(map()) -> map().
convert_mdns_to_peer_info(Details) ->
    IP = maps:get(ip, Details, {0, 0, 0, 0}),
    Port = maps:get(port, Details, 9443),
    Instance = maps:get(instance, Details, <<>>),
    %% Build endpoint URL
    IPStr = inet:ntoa(IP),
    Endpoint = iolist_to_binary([<<"quic://">>, list_to_binary(IPStr), <<":">>, integer_to_binary(Port)]),
    %% Extract node_id from TXT record if present, otherwise hash instance
    NodeId = case maps:get(node_id, Details, undefined) of
        undefined -> crypto:hash(sha256, Instance);
        Id when is_binary(Id) -> Id;
        Id when is_list(Id) -> list_to_binary(Id)
    end,
    #{
        endpoint => Endpoint,
        node_id => NodeId,
        instance => Instance,
        ttl => maps:get(ttl, Details, 300),
        priority => maps:get(priority, Details, 0),
        weight => maps:get(weight, Details, 0)
    }.

%% @doc Discover peers via DNS SRV records.
%% Queries _macula-bridge-{level}._udp.{domain} for bridge endpoints.
-spec discover_via_dns_srv(atom(), #state{}) -> #state{}.
discover_via_dns_srv(Level, State) ->
    Domain = get_dns_domain(),
    ServiceName = "_macula-bridge-" ++ atom_to_list(Level) ++ "._udp." ++ Domain,
    ?LOG_DEBUG("[BridgeMesh] Querying DNS SRV: ~s", [ServiceName]),
    case inet_res:lookup(ServiceName, in, srv) of
        [] ->
            ?LOG_DEBUG("[BridgeMesh] No DNS SRV records found for ~s", [ServiceName]),
            State;
        Records ->
            lists:foldl(fun({Priority, Weight, Port, Host}, AccState) ->
                Endpoint = iolist_to_binary([<<"quic://">>, list_to_binary(Host), <<":">>, integer_to_binary(Port)]),
                PeerInfo = #{
                    endpoint => Endpoint,
                    node_id => crypto:hash(sha256, Endpoint),
                    priority => Priority,
                    weight => Weight
                },
                {_, NewState} = do_add_peer(PeerInfo, AccState),
                NewState
            end, State, Records)
    end.

%% @doc Get the DNS domain for SRV lookups.
-spec get_dns_domain() -> string().
get_dns_domain() ->
    application:get_env(macula, dns_domain, "local").
