%%%-------------------------------------------------------------------
%% @doc Macula Application Root Supervisor.
%%
%% This is the top-level supervisor for the Macula application.
%% It manages all Macula subsystems in an always-on architecture.
%%
%% Supervision Hierarchy (v0.16.0+):
%% <pre>
%% macula_root (this module - application root)
%% ├── macula_routing_server (core DHT infrastructure - always on)
%% ├── macula_nat_system (NAT traversal - always on)
%% │   ├── macula_nat_detector
%% │   ├── macula_hole_punch
%% │   ├── macula_relay_registry
%% │   └── macula_connection_upgrade
%% ├── macula_bootstrap_system (bootstrap services - always on)
%% │   ├── macula_bootstrap_server
%% │   ├── macula_bootstrap_registry
%% │   └── macula_bootstrap_health
%% ├── macula_gateway_system (gateway services - always on)
%% │   ├── macula_gateway_health
%% │   ├── macula_gateway_diagnostics
%% │   ├── macula_gateway_quic_server
%% │   ├── macula_gateway
%% │   └── macula_gateway_workers_sup
%% ├── macula_bridge_system (hierarchical mesh bridging - optional)
%% │   ├── macula_bridge_node (parent mesh connection)
%% │   ├── macula_bridge_mesh (peer bridge mesh)
%% │   └── macula_bridge_cache (parent query results cache)
%% ├── macula_peers_sup (dynamic peer connections - always on)
%% ├── macula_peer_discovery (DHT-based P2P mesh - always on)
%% ├── macula_platform_system (distributed coordination - always on)
%% │   └── macula_crdt (LWW-Register, OR-Set, G-Counter, PN-Counter)
%% └── macula_registry_system (package distribution - always on)
%%     ├── macula_registry_store (ETS + disk storage)
%%     ├── macula_registry_server (publish/fetch API)
%%     ├── macula_cluster_controller (app lifecycle)
%%     └── macula_app_monitor (runtime defense)
%% </pre>
%%
%% Architecture Philosophy (v0.8.5):
%% - All nodes have ALL capabilities (no mode selection)
%% - Zero configuration required (TLS auto-generated)
%% - Simplified deployment (every node is bootstrap + gateway + peer)
%% - True P2P mesh (nodes connect on-demand based on capability discovery)
%%
%% Naming Convention (v0.7.10+):
%% - _root: Application root supervisor (one per application)
%% - _system: Subsystem root supervisors (gateway, peer, bootstrap, etc.)
%% - _sup: Worker supervisors (workers_sup, peers_sup)
%%
%% @end
%%%-------------------------------------------------------------------

-module(macula_root).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    ?LOG_INFO(""),
    ?LOG_INFO("═══════════════════════════════════════════════════════════════"),
    ?LOG_INFO("  Starting Macula v0.8.5 (Always-On Architecture)"),
    ?LOG_INFO("  All capabilities enabled: Bootstrap + Gateway + Peer"),
    ?LOG_INFO("═══════════════════════════════════════════════════════════════"),
    ?LOG_INFO(""),

    %% Ensure TLS certificates exist (auto-generate if missing)
    {CertPath, KeyPath} = macula_tls:get_cert_paths(),
    {ok, _CertPath, _KeyPath, TlsNodeID} = macula_tls:ensure_cert_exists(CertPath, KeyPath),

    %% Use NODE_ID from environment if available, otherwise use TLS-derived ID
    %% NODE_ID env var is used by applications like ping_pong for peer addressing
    NodeID = get_node_id_from_env_or_tls(TlsNodeID),

    ?LOG_INFO("✓ TLS Certificate: ~s", [CertPath]),
    ?LOG_INFO("✓ Private Key: ~s", [KeyPath]),
    ?LOG_INFO("✓ Node ID: ~s (source: ~s)", [NodeID, case os:getenv("NODE_ID") of false -> "TLS"; _ -> "NODE_ID env" end]),
    ?LOG_INFO(""),

    %% Get configuration
    Port = get_quic_port(),
    Realm = get_realm(),
    HealthPort = get_health_port(),
    HealthInterval = get_bootstrap_health_interval(),

    BootstrapPeers = get_bootstrap_peers(),

    ?LOG_INFO("Configuration:"),
    ?LOG_INFO("  QUIC Port: ~p", [Port]),
    ?LOG_INFO("  Realm: ~s", [Realm]),
    ?LOG_INFO("  Health Port: ~p", [HealthPort]),
    ?LOG_INFO("  Bootstrap Health Interval: ~pms", [HealthInterval]),
    case BootstrapPeers of
        [] -> ?LOG_INFO("  Bootstrap Peers: none (this node is a bootstrap peer)");
        _ -> ?LOG_INFO("  Bootstrap Peers: ~s", [string:join([binary_to_list(P) || P <- BootstrapPeers], ", ")])
    end,
    ?LOG_INFO(""),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    },

    %% Check if bridge system is enabled (for hierarchical mesh)
    BridgeConfig = get_bridge_config(),
    BridgeEnabled = maps:get(bridge_enabled, BridgeConfig, false),

    ChildSpecs = [
        %% 1. Core DHT routing (always on)
        get_routing_server_spec(NodeID),

        %% 2. NAT system (NAT detection, hole punching, relay - always on)
        get_nat_system_spec(),

        %% 3. Bootstrap system (always on)
        get_bootstrap_system_spec(Realm, HealthInterval),

        %% 4. Gateway system (always on)
        get_gateway_system_spec(Port, Realm, HealthPort),

        %% 5. Bridge system (hierarchical mesh - optional)
        get_bridge_system_spec(BridgeConfig),

        %% 6. Peer connections supervisor (always on)
        get_peers_sup_spec(),

        %% 7. Peer discovery (DHT-based P2P mesh formation)
        get_peer_discovery_spec(NodeID, Port, Realm),

        %% 8. Platform system (distributed coordination - always on)
        get_platform_system_spec(NodeID, Realm),

        %% 9. Registry system (package distribution - always on)
        get_registry_system_spec(NodeID, Realm)
    ],

    ?LOG_INFO("Starting subsystems:"),
    ?LOG_INFO("  [1/9] Core DHT Routing"),
    ?LOG_INFO("  [2/9] NAT System (Detection + Hole Punch + Relay)"),
    ?LOG_INFO("  [3/9] Bootstrap System"),
    ?LOG_INFO("  [4/9] Gateway System"),
    case BridgeEnabled of
        true ->
            ParentBridges = maps:get(parent_bridges, BridgeConfig, []),
            MeshLevel = maps:get(mesh_level, BridgeConfig, cluster),
            ?LOG_INFO("  [5/9] Bridge System (level: ~p, parents: ~p)", [MeshLevel, length(ParentBridges)]);
        false ->
            ?LOG_INFO("  [5/9] Bridge System (disabled)")
    end,
    ?LOG_INFO("  [6/9] Peers Supervisor"),
    ?LOG_INFO("  [7/9] Peer Discovery (P2P Mesh)"),
    ?LOG_INFO("  [8/9] Platform System (Masterless CRDT)"),
    ?LOG_INFO("  [9/9] Registry System (Package Distribution)"),
    ?LOG_INFO(""),

    %% Schedule bootstrap peer connections after supervision tree is up
    case BootstrapPeers of
        [] ->
            ?LOG_INFO("No bootstrap peers configured - this node is a bootstrap peer"),
            ok;
        _ ->
            ?LOG_INFO("Will connect to bootstrap peers after startup: ~p", [BootstrapPeers]),
            spawn(fun() -> connect_to_bootstrap_peers(BootstrapPeers, Realm, NodeID) end)
    end,

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

%% @private
%% @doc Get routing server child spec (core DHT infrastructure).
get_routing_server_spec(NodeID) ->
    #{
        id => macula_routing_server,
        start => {macula_routing_server, start_link, [NodeID, get_routing_config()]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [macula_routing_server]
    }.

%% @private
%% @doc Get NAT system child spec (NAT detection, hole punching, relay).
get_nat_system_spec() ->
    #{
        id => macula_nat_system,
        start => {macula_nat_system, start_link, [#{}]},
        restart => permanent,
        shutdown => infinity,  % supervisor shutdown
        type => supervisor,
        modules => [macula_nat_system]
    }.

%% @private
%% @doc Get bootstrap system child spec.
get_bootstrap_system_spec(Realm, HealthInterval) ->
    #{
        id => macula_bootstrap_system,
        start => {macula_bootstrap_system, start_link, [#{
            realm => Realm,
            health_check_interval => HealthInterval
        }]},
        restart => permanent,
        shutdown => infinity,  % supervisor shutdown
        type => supervisor,
        modules => [macula_bootstrap_system]
    }.

%% @private
%% @doc Get gateway system child spec.
get_gateway_system_spec(Port, Realm, HealthPort) ->
    #{
        id => macula_gateway_system,
        start => {macula_gateway_system, start_link, [[
            {port, Port},
            {realm, Realm},
            {health_port, HealthPort}
        ]]},
        restart => permanent,
        shutdown => infinity,  % supervisor shutdown
        type => supervisor,
        modules => [macula_gateway_system]
    }.

%% @private
%% @doc Get peers supervisor child spec (simple_one_for_one for dynamic peers).
get_peers_sup_spec() ->
    #{
        id => macula_peers_sup,
        start => {macula_peers_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,  % supervisor shutdown
        type => supervisor,
        modules => [macula_peers_sup]
    }.

%% @private
%% @doc Get peer discovery child spec (DHT-based P2P mesh formation).
get_peer_discovery_spec(NodeID, Port, Realm) ->
    Host = get_hostname(),
    #{
        id => macula_peer_discovery,
        start => {macula_peer_discovery, start_link, [#{
            node_id => NodeID,
            host => Host,
            port => Port,
            realm => Realm,
            discovery_interval => 30000  % 30 seconds
        }]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [macula_peer_discovery]
    }.

%% @private
%% @doc Get QUIC port from environment variable or config.
%% Environment variable renamed from GATEWAY_PORT to MACULA_QUIC_PORT (v0.8.5).
get_quic_port() ->
    case os:getenv("MACULA_QUIC_PORT") of
        false ->
            %% Fallback to old GATEWAY_PORT for backward compatibility
            case os:getenv("GATEWAY_PORT") of
                false ->
                    application:get_env(macula, quic_port,
                        application:get_env(macula, gateway_port, 9443));
                PortStr ->
                    list_to_integer(PortStr)
            end;
        PortStr ->
            list_to_integer(PortStr)
    end.

%% @private
%% @doc Get realm from environment variable or config.
get_realm() ->
    case os:getenv("MACULA_REALM") of
        false ->
            application:get_env(macula, realm,
                application:get_env(macula, gateway_realm, <<"com.example.realm">>));
        RealmStr ->
            list_to_binary(RealmStr)
    end.

%% @private
%% @doc Get health check port from environment variable or config.
get_health_port() ->
    case os:getenv("HEALTH_PORT") of
        false ->
            application:get_env(macula, health_port, 8080);
        PortStr ->
            list_to_integer(PortStr)
    end.

%% @private
%% @doc Get bootstrap health check interval.
get_bootstrap_health_interval() ->
    application:get_env(macula, bootstrap_health_interval, 60000).

%% @private
%% @doc Get DHT routing configuration.
get_routing_config() ->
    #{
        k => application:get_env(macula, dht_k, 20),        %% Kademlia K parameter
        alpha => application:get_env(macula, dht_alpha, 3)  %% Concurrent queries
    }.

%% @private
%% @doc Get bootstrap peer URLs from environment variable.
%% Returns list of binary URLs or empty list if not configured.
%% Environment variable format: "https://peer1:4433,https://peer2:4433"
get_bootstrap_peers() ->
    case os:getenv("MACULA_BOOTSTRAP_PEERS") of
        false ->
            [];
        PeersStr ->
            %% Split comma-separated URLs and convert to binaries
            Urls = string:tokens(PeersStr, ","),
            [list_to_binary(string:trim(Url)) || Url <- Urls]
    end.

%% @private
%% @doc Get hostname from environment variable or default to localhost.
%% Environment variable: MACULA_HOSTNAME
%% Used for peer discovery registration so peers can reach each other.
get_hostname() ->
    case os:getenv("MACULA_HOSTNAME") of
        false ->
            <<"localhost">>;
        HostnameStr ->
            list_to_binary(HostnameStr)
    end.

%% @private
%% @doc Get node ID from NODE_ID environment variable or fall back to TLS-derived ID.
%% NODE_ID env var allows applications to set consistent peer identifiers.
%% This is critical for peer addressing in demos like ping_pong.
-spec get_node_id_from_env_or_tls(binary()) -> binary().
get_node_id_from_env_or_tls(TlsNodeId) ->
    case os:getenv("NODE_ID") of
        false ->
            TlsNodeId;
        NodeIdStr ->
            list_to_binary(NodeIdStr)
    end.

%% @private
%% @doc Get platform system child spec (distributed coordination).
get_platform_system_spec(NodeID, Realm) ->
    #{
        id => macula_platform_system,
        start => {macula_platform_system, start_link, [#{
            node_id => NodeID,
            realm => Realm
        }]},
        restart => permanent,
        shutdown => infinity,  % supervisor shutdown
        type => supervisor,
        modules => [macula_platform_system]
    }.

%% @private
%% @doc Get registry system child spec (package distribution).
%% The registry system provides:
%% - Package publishing with Ed25519 signatures
%% - Package fetching with integrity verification
%% - Static security scanning before deployment
%% - Runtime monitoring with memory/queue limits
%% - Application lifecycle management (deploy/upgrade/stop)
get_registry_system_spec(NodeID, Realm) ->
    #{
        id => macula_registry_system,
        start => {macula_registry_system, start_link, [#{
            node_id => NodeID,
            realm => Realm
        }]},
        restart => permanent,
        shutdown => infinity,  % supervisor shutdown
        type => supervisor,
        modules => [macula_registry_system]
    }.

%% @private
%% @doc Get bridge system child spec (hierarchical mesh bridging).
%% The bridge system enables nodes to participate in a hierarchical mesh:
%% Cluster -&gt; Street -&gt; Neighborhood -&gt; City -&gt; Province -&gt; Country -&gt; Region -&gt; Global
%%
%% When enabled, DHT queries that fail locally are escalated to parent mesh levels.
%% Results are cached at each level to reduce parent query load.
get_bridge_system_spec(Config) ->
    #{
        id => macula_bridge_system,
        start => {macula_bridge_system, start_link, [Config]},
        restart => permanent,
        shutdown => infinity,  % supervisor shutdown
        type => supervisor,
        modules => [macula_bridge_system]
    }.

%% @private
%% @doc Get bridge system configuration from environment variables.
%% Environment variables:
%%   MACULA_BRIDGE_ENABLED - "true" to enable bridge functionality
%%   MACULA_MESH_LEVEL - Mesh level: cluster, street, neighborhood, city, etc.
%%   MACULA_PARENT_BRIDGES - Comma-separated parent bridge endpoints
%%   MACULA_BRIDGE_DISCOVERY - Discovery method: static, mdns, dns_srv
%%   MACULA_BRIDGE_CACHE_TTL - Cache TTL in seconds (default varies by level)
%%   MACULA_BRIDGE_CACHE_SIZE - Maximum cache entries (default 10000)
-spec get_bridge_config() -> map().
get_bridge_config() ->
    BridgeEnabled = get_bridge_enabled(),
    MeshLevel = get_mesh_level(),
    ParentBridges = get_parent_bridges_config(),
    DiscoveryMethod = get_bridge_discovery_method(),
    CacheTTL = get_bridge_cache_ttl(),
    CacheSize = get_bridge_cache_size(),

    #{
        bridge_enabled => BridgeEnabled,
        mesh_level => MeshLevel,
        parent_bridges => ParentBridges,
        discovery_method => DiscoveryMethod,
        cache_ttl => CacheTTL,
        cache_max_size => CacheSize,
        escalation_enabled => BridgeEnabled andalso ParentBridges =/= []
    }.

%% @private
%% @doc Check if bridge functionality is enabled.
-spec get_bridge_enabled() -> boolean().
get_bridge_enabled() ->
    case os:getenv("MACULA_BRIDGE_ENABLED") of
        "true" -> true;
        "1" -> true;
        _ -> application:get_env(macula, bridge_enabled, false)
    end.

%% @private
%% @doc Get the mesh level for this node.
%% Valid levels: cluster, street, neighborhood, city, province, country, region, global
-spec get_mesh_level() -> atom().
get_mesh_level() ->
    case os:getenv("MACULA_MESH_LEVEL") of
        false ->
            application:get_env(macula, mesh_level, cluster);
        LevelStr ->
            list_to_existing_atom(LevelStr)
    end.

%% @private
%% @doc Get parent bridge endpoints from environment or config.
%% Format: "quic://host1:port1,quic://host2:port2"
-spec get_parent_bridges_config() -> [binary()].
get_parent_bridges_config() ->
    case os:getenv("MACULA_PARENT_BRIDGES") of
        false ->
            application:get_env(macula, parent_bridges, []);
        BridgesStr ->
            Urls = string:tokens(BridgesStr, ","),
            [list_to_binary(string:trim(Url)) || Url <- Urls]
    end.

%% @private
%% @doc Get bridge discovery method.
%% Valid methods: static, mdns, dns_srv
-spec get_bridge_discovery_method() -> atom().
get_bridge_discovery_method() ->
    case os:getenv("MACULA_BRIDGE_DISCOVERY") of
        false ->
            application:get_env(macula, bridge_discovery, static);
        MethodStr ->
            list_to_existing_atom(MethodStr)
    end.

%% @private
%% @doc Get cache TTL (0 means use level-based default).
-spec get_bridge_cache_ttl() -> non_neg_integer().
get_bridge_cache_ttl() ->
    case os:getenv("MACULA_BRIDGE_CACHE_TTL") of
        false ->
            application:get_env(macula, bridge_cache_ttl, 0);
        TTLStr ->
            list_to_integer(TTLStr)
    end.

%% @private
%% @doc Get maximum cache size.
-spec get_bridge_cache_size() -> pos_integer().
get_bridge_cache_size() ->
    case os:getenv("MACULA_BRIDGE_CACHE_SIZE") of
        false ->
            application:get_env(macula, bridge_cache_size, 10000);
        SizeStr ->
            list_to_integer(SizeStr)
    end.

%% @private
%% @doc Connect to bootstrap peers to join their DHT network.
%% Waits briefly for supervision tree to stabilize, then initiates connections.
%% NodeID is passed to the peer connection so the gateway stores the stream
%% under the correct identifier (matching what applications like ping_pong use).
connect_to_bootstrap_peers(Peers, Realm, NodeID) ->
    %% Wait for supervision tree to fully start
    timer:sleep(2000),

    ?LOG_INFO("[DHT Bootstrap] Connecting to ~p bootstrap peers with node_id: ~s...",
              [length(Peers), NodeID]),

    lists:foreach(fun(PeerUrl) ->
        ?LOG_INFO("[DHT Bootstrap] Connecting to: ~s", [PeerUrl]),

        %% Start a peer connection via macula_peers_sup
        %% Pass node_id so the connection uses this ID in CONNECT message
        case macula_peers_sup:start_peer(PeerUrl, #{realm => Realm, node_id => NodeID}) of
            {ok, PeerPid} ->
                ?LOG_INFO("[DHT Bootstrap] Successfully connected to ~s (PID: ~p, node_id: ~s)",
                         [PeerUrl, PeerPid, NodeID]),

                %% Add bootstrap peer to routing table for DHT routing
                add_bootstrap_to_routing_table(PeerUrl);
            {error, Reason} ->
                ?LOG_ERROR("[DHT Bootstrap] Failed to connect to ~s: ~p",
                         [PeerUrl, Reason])
        end
    end, Peers),

    ?LOG_INFO("[DHT Bootstrap] Bootstrap peer connections initiated"),
    ok.

%% @private
%% @doc Add bootstrap peer to routing table after successful connection.
%% Parses URL to extract host/port, generates node_id from URL, adds to DHT routing table.
add_bootstrap_to_routing_table(PeerUrl) ->
    %% Parse URL to get host and port
    case uri_string:parse(PeerUrl) of
        #{host := Host, port := Port} ->
            %% Generate node_id for bootstrap peer (deterministic from URL)
            NodeId = crypto:hash(sha256, PeerUrl),

            %% Create node info for routing table
            NodeInfo = #{
                node_id => NodeId,
                address => {Host, Port}
            },

            %% Add to routing table
            case whereis(macula_routing_server) of
                undefined ->
                    ?LOG_WARNING("[DHT Bootstrap] Routing server not available, skipping routing table add");
                RoutingServerPid ->
                    macula_routing_server:add_node(RoutingServerPid, NodeInfo),
                    ?LOG_INFO("[DHT Bootstrap] Added bootstrap peer ~s to routing table (node_id: ~s)",
                             [PeerUrl, binary:encode_hex(NodeId)])
            end;
        _Other ->
            ?LOG_ERROR("[DHT Bootstrap] Failed to parse bootstrap URL: ~s", [PeerUrl])
    end,
    ok.
